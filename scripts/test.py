#! /usr/bin/env python
#
# This script starts RTN DAQ and DAS processes which use the isis-lanl libraries,
# and the SCL stub called StubComponent.
#
# When running this script with no args, it will start up the processes, run
# through the test cases for which StubComponent is the driver, kill the process,
# analyze the log files and core files, and exit.  If there are core files at
# all from the run, or if the log files contain error statements, then this
# script raises the IsisTestError exception to the user.
#
# Alternatively, you can run with the -g or --gdb option and an xterm will open
# for each process and run gdb on it.  You will have to manually examine any
# log files or core files that are created this way, since this script will not
# attempt to do so.  As a convenience, you can run this script with the -c or
# --clean option when you are finished with gdb-based debugging.
#
# Specify the readout mode of the DAQs by --bistatic=[SIM|HW|REPLAY] or --monostatic=[SIM|HW|REPLAY].
# Default is sim.  Be sure values are consistent with ReadoutMode in DataAcqSvcsSAP.xml .
#
# Hints:
#   1: You can see how many reads from the three FDs the ReadoutEventLoop selects on,
#   respectively: ctrl FD, data FD, diag FD.  Monostatic for example:
#       egrep 'returned.*select.*FDs set:1,.,.' `ls -dt *Monostatic*.coe.log | head -1` | wc -l
#       egrep 'returned.*select.*FDs set:.,1,.' `ls -dt *Monostatic*.coe.log | head -1` | wc -l
#       egrep 'returned.*select.*FDs set:.,.,1' `ls -dt *Monostatic*.coe.log | head -1` | wc -l
#   Of course you can get fancier with this, for example you could put the DaqSession run number
#   in the egrep regex.
#
# TODO: Sometimes when running the script we get stderr output:
#           ls: write error: Broken pipe
# at various points in the script where we ls -dt *log .  This seems to happen
# when there are a lot of log files.

import datetime
import getopt
import os
import re
import subprocess
import sys
import time

scriptName_g = re.sub(r'.*\/', '', sys.argv[0])

scriptHelp_g = """Usage: %s [options]
Options:
    -b, --bistatic=[sim|hw]             Specify the readout mode of the BDAQ as sim or hw.
                                        Sim is default.  Replay is unsupported until needed.
                                        Must be consistent with DataAcqSvcsSAP.xml ReadoutMode.
    -c, --clean                         Kills renegade ISIS processes.
    -g, --gdb                           Opens an xterm for each process to run it in gdb.
                                        Log and core files are not examined.
    -h, --help                          Print this help.
    -m, --monostatic=[sim|hw]           Specify the readout mode of the MDAQ as sim or hw.
                                        Sim is default.  Replay is unsupported until needed.
                                        Must be consistent with DataAcqSvcsSAP.xml ReadoutMode.
    -v, --version                       Print script version.""" % scriptName_g

def cmdOs(cmdStr, printCmd=False):
    if printCmd:
        print 'Issuing direct OS command: %s' % cmdStr
    os.system(cmdStr)

def getNumCoreFiles():
    myOsCmd = subprocess.Popen('ls -dt1 core* | wc -l', stdout=subprocess.PIPE, shell=True)
    (numRet, stdErr) = myOsCmd.communicate()
    numRet = int(numRet.rstrip())
    return numRet

class IsisTestError(Exception):
    pass

class ErrorBuildup:
    def __init__(self):
        self.errors = []
    def assertExpr(self, boolExpr, errorMsg):
        if not boolExpr:
            self.errors.append(errorMsg)
    def addError(self, errorMsg):
        self.assertExpr(False, errorMsg)

class IsisProcess:
    def __init__(self, path, argsStr = "", hasCoeLog = True, hasStdoutLog = False, hasStderrLog = False, *argsP, **kw):
        self.theProcess = None
        self.path = path;
        self.argsStr = argsStr
        self.hasCoeLog = hasCoeLog
        self.hasStdoutLog = hasStdoutLog
        self.hasStderrLog = hasStderrLog

    def getGdbCmd(self):
        gdbCmd = 'gdb %s %s'%(self.path, self.argsStr)
        return ( 'xterm -geometry 200x78+0+0 -foreground white -background black '
                    +'-title %s -e %s'%(self.getBasename(), gdbCmd) )

    def startProcGdb(self, *argsP, **kw):
        cmdOs(self.getGdbCmd()+'&')

    def startProc(self, *argsP, **kw):
        #kw['args'] = path
        #subprocess.Popen.__init__(self, *argsP, **kw)
        print 'Starting up process: %s %s'%(self.path, self.argsStr)
        self.theProcess = subprocess.Popen('%s %s'%(self.path, self.argsStr), stdout=subprocess.PIPE, shell=True)

    def stopProc(self):
        # To kill the process, we pgrep it to get the PID and then kill it.
        # We don't use Popen.terminate method because we want to stop renegade procs
        # from a previous run.
        #
        # Unfortunately, what complicates things is that the pgrep also picks up
        # the sh process which forks from this python script.  Thus we use a little
        # regex to get the one we want.
        #
        # TODO: Some of the processes redirect stdout and stderr to file, but killing
        # the processes this way is observed to result in the loss of unflushed data
        # on those FDs.

        # The following causes junk on stderr.
        #cmdOs(r"kill `pgrep -f '^%s'`" % self.path)

        # Get the PID
        #
        (pidStr, stdErr) = subprocess.Popen(
                    "pgrep -f '^%s'" % self.path, stdout=subprocess.PIPE, shell=True).communicate()
        # Note: I was going to pgrep based on the return from getGdbCmd, but the geometry arg throws it off,
        # so we'll just do this reasonably sound regex to get the right one.
        (gdbPidStr, stdErr) = subprocess.Popen(
                    "pgrep -f '^xterm .* gdb %s'" % self.path,
                    stdout=subprocess.PIPE, shell=True).communicate()

        # Go in for the kill
        #
        if not ''==pidStr.rstrip():
            cmdOs('kill %s' % pidStr)
        if not ''==gdbPidStr.rstrip():
            cmdOs('kill %s' % gdbPidStr)

    def isRunning(self):
        myOsCmd = subprocess.Popen('pgrep -f %s | wc -l' % self.path,
                    stdout=subprocess.PIPE, shell=True)
        (numRet, stdErr) = myOsCmd.communicate()
        numRet = int(numRet.rstrip())
        numRet -= 1
        return numRet

    def getBasename(self):
        return re.sub(r'.*\/', '', self.path)

    def getNumLogStatements(self, logLevel):
        """Get the number of log statements of the inputted logLevel.
        """

        if logLevel=='stdout' or logLevel=='stderr':
            (numRet, stdErr) = subprocess.Popen(
                        'cat `ls -dt *%s.%s.log | sort | tail -1` | wc -l'
                        %(self.getBasename(), logLevel),
                        stdout=subprocess.PIPE, shell=True).communicate()
            numRet = int(numRet.rstrip())
            return numRet
        else:
            (numRet, stdErr) = subprocess.Popen(
                        'egrep %s `ls -dt *%s.coe.log | head -1` | wc -l'
                        %(logLevel,self.getBasename()),
                        stdout=subprocess.PIPE, shell=True).communicate()
            numRet = int(numRet.rstrip())
            return numRet

    def printLogStatements(self, logLevel):
        """Print the log statements of the inputted logLevel.
        """

        # Under some circumstances, errors can be spammy.
        numLogs = self.getNumLogStatements(logLevel)
        maxLogsToPrint = 10

        print "--- Beginning printout of %s's %s log statements"%(
                    self.getBasename(), logLevel)

        if logLevel=='stdout' or logLevel=='stderr':
            cmdOs( 'head -%d `ls -dt *%s.%s.log | sort | tail -1`'
                        %(maxLogsToPrint, self.getBasename(), logLevel) )
        else:
            # Provide some indentation to make things a little easier to look at.
            #cmdOs( 'echo -n "    "', printCmd=False )
            cmdOs( 'egrep %s `ls -dt *%s.coe.log | head -1` | head -%d'
                        %(logLevel,self.getBasename(),maxLogsToPrint) )

        if maxLogsToPrint<numLogs:
            print "... [%s truncates after the first %d log statements]"%(scriptName_g, maxLogsToPrint)
        print "--- End printout"

def assessProcs(isisProcs):
    print 'Assessing which processes are running. '
    for procI in isisProcs:
        print '    %s: %s' % (procI.path, procI.isRunning(),)

def runTestGdb(isisProcs):
    # Note: isis-lanl's infra/isis-automate.py uses:
    #gdb_str = "gdb --eval-command='handle SIGPIPE nostop' --eval-command=run --args "
    for procI in isisProcs.values():
        procI.startProcGdb()

    print( "%s will exit.  To cleanup, run the same script with the -c or --clean option.  "%scriptName_g
                +"Analyzing log or core files is up to you.  " )

def runTest(isisProcs):
    # Start up procs
    #
    isisProcs['dvs'].startProc()
    # When running over the network, the DVS needs time to start up
    # before DAQ tries to establish connection.
    time.sleep(1)
    isisProcs['das'].startProc()
    isisProcs['mdaq'].startProc()
    isisProcs['bdaq'].startProc()
    # Wait for the others to initialize, since StubComponent is going
    # to start the test straight away.
    time.sleep(1)
    isisProcs['sclStub'].startProc()

    # We expect all processes are running at this point.
    isisProcs['mdaq'].expectedRunning = True
    isisProcs['bdaq'].expectedRunning = True
    isisProcs['das'].expectedRunning = True
    isisProcs['dvs'].expectedRunning = True
    isisProcs['sclStub'].expectedRunning = True
    for procI in isisProcs.values():
        assert procI.isRunning()==procI.expectedRunning, 'Process %s in wrong running state. '%procI.getBasename()

    print '%s will now wait on StubComponent child process to exit. ' % scriptName_g
    isisProcs['sclStub'].theProcess.wait()

    # We expect sclStub has exited.
    isisProcs['sclStub'].expectedRunning = False
    for procI in isisProcs.values():
        actualRunning = procI.isRunning()
        assert actualRunning==procI.expectedRunning, 'Process %s in wrong running state of %d. '%(
                    procI.getBasename(), actualRunning)

def cleanup(isisProcs):
    for procI in isisProcs.values():
        procI.stopProc()
        # We expect it to work.
        assert not procI.isRunning(), "Process:%s"%procI.getBasename()
    # Kill readout process forked from DAQ
    cmdOs('pkill readout-sim')

def postAnalyze(isisProcs, initNumCoreFiles):
    # TODO: There's a weird bug where some of the stdout and stderr log files have timestamp Dec 31  1969.
    # Maybe the cause is because we kill the procs and thus they never reach the fclose call.
    # Anyway, this works around it so I get the correct log files when I do something like:
    # vim `ls -dt *log | head -10`
    for procI in isisProcs.values():
        if procI.hasStdoutLog:
            cmdOs('touch `ls -dt *%s.stdout.log | sort | tail -1`'%procI.getBasename())
        if procI.hasStderrLog:
            cmdOs('touch `ls -dt *%s.stderr.log | sort | tail -1`'%procI.getBasename())

    # Wish to display all LM_ERROR log statements and core files, so need to build them up.
    errorBuildup = ErrorBuildup()

    # Determine log warnings and errors.
    for procI in isisProcs.values():
        if procI.hasCoeLog:
            numWarnings = procI.getNumLogStatements('LM_WARNING')
            if 0<numWarnings:
                #procI.printLogStatements('LM_WARNING')
                print 'WARNING: %s has %d LM_WARNING log statements. '%(
                            procI.getBasename(), numWarnings)
            numErrors = procI.getNumLogStatements('LM_ERROR')
            if 0<numErrors:
                procI.printLogStatements('LM_ERROR')
                errorBuildup.addError( "%s's most recent log file contains %d LM_ERROR statements. "%(
                            procI.getBasename(), procI.getNumLogStatements('LM_ERROR')) )
            # I (BJO) use LM_EMERGENCY for debug statements not intended for the baseline.
            # Issue me an advisory if I leave them in the code.
            numEmergency = procI.getNumLogStatements('LM_EMERGENCY')
            if 0<numEmergency:
                print "WARNING: %s has %d LM_EMERGENCY log statements.  They're not allowed in the baseline. "%(
                            procI.getBasename(), numEmergency)
        if procI.hasStderrLog:
            numStderr = procI.getNumLogStatements('stderr')
            if 0<numStderr:
                procI.printLogStatements('stderr')
                errorBuildup.addError( "%s's most recent stderr log file contains %d lines. "%(
                            procI.getBasename(), procI.getNumLogStatements('stderr')) )

    finalNumCoreFiles = getNumCoreFiles()
    if initNumCoreFiles!=finalNumCoreFiles:
        # Show core filenames and the procs that created them on stdout.
        cmdOs('file `ls -dt core* | head -%d`'%(finalNumCoreFiles-initNumCoreFiles))
        errorBuildup.addError( 'The number of core files in cwd increased by %d during the run.'%(
                    finalNumCoreFiles-initNumCoreFiles) )

    if not 0==len(errorBuildup.errors):
        raise IsisTestError(errorBuildup.errors)

    print('Test has concluded with success.')

def __MAIN__():
    # Process args
    #
    (opts, args) = getopt.getopt(sys.argv[1:], "b:cghm:t:v", ["clean", "bistatic", "gdb", "help", "monostatic", "version"])
    bReadoutMode = 'sim'
    mReadoutMode = 'sim'
    useGdb = False
    cleanupOnly = False
    for optI, argI in opts:
        if optI in ('-b', '--bistatic'):
            bReadoutMode = argI
        if optI in ('-c', '--clean'):
            cleanupOnly = True
            print 'Will cleanup by killing renegade ISIS processes. '
        if optI in ('-g', '--gdb'):
            useGdb = True
        if optI in ('-h', '--help'):
            print(scriptHelp_g)
            sys.exit(0)
        if optI in ('-m', '--monostatic'):
            mReadoutMode = argI
        if optI in ('-v', '--version'):
            print "'cleartool catcs' to see the version. "
            sys.exit(0)

    try:
        isisProcs = {}
        # Count num core files currently in cwd
        initNumCoreFiles = getNumCoreFiles()

        isis_root = os.getenv('ISIS_ROOT')
        if isis_root==None:
            raise ValueError('User needs to define ISIS_ROOT env var.')
        print "Using ISIS_ROOT:%s" % isis_root

        #
        # TODO: Determine what daq2dvs.conf to pass when running a DAQ with hardware.
        isisProcs = {'mdaq': IsisProcess(isis_root+'/Devel/Sen/Daq/MonostaticDataAcqSvcs', hasStdoutLog=True, hasStderrLog=True),
                    'bdaq': IsisProcess(isis_root+'/Devel/Sen/Daq/BistaticDataAcqSvcs', hasStdoutLog=True, hasStderrLog=True),
                    'das': IsisProcess(isis_root+'/Devel/Sen/Das/DataAnalysisSvcs', hasStdoutLog=True, hasStderrLog=True),
                    'dvs': IsisProcess(isis_root+'/COTS/isis-lanl/dvs/dvs-root-tthread',
                     argsStr='-i 0 --daq2dvs=daq2dvs-sim.conf', hasCoeLog=False, hasStdoutLog=True, hasStderrLog=True),
                    'sclStub': IsisProcess(isis_root+'/test/DaqDas/StubComponent'), }

        cleanup(isisProcs)
        # Count num core files currently in cwd
        initNumCoreFiles = getNumCoreFiles()

        if not cleanupOnly:
            if not useGdb :
                runTest(isisProcs)
            else:
                runTestGdb(isisProcs)
    finally:
        if not useGdb and not cleanupOnly:
            cleanup(isisProcs)
            # Note: Exceptions thrown from postAnalyze supercede those thrown from above.
            postAnalyze(isisProcs, initNumCoreFiles)

if __name__=='__main__':
    __MAIN__()

