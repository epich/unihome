#! /usr/bin/env python
#
# General utility module for Python scripts.

import datetime
import os
import time
import site
import sys
import re
import subprocess
import threading

class GeneralError(Exception):
   """Exception for Python scripts.

   Provides the capability to wrap a propogating exception and add supplemental information.  See reraise method.
   """

   def __init__(self, supplementalMsg):
      """Initialize data.

      Keyword arguments:
      supplementalMsg -- the message to add to the propogating exception
      """
      
      self.supplementalMsg = supplementalMsg
      Exception.__init__( self, supplementalMsg )

   def reraise(self):
      """Combine the information of the currently propogating exception with self.supplementalMsg and reraise self.

      This method is meant to be invoked from within an except clause.

      Specifically, the reraised Exception retains the trackback of the currently propogating exception and its 
      type and message.  The reraised Exception is of type GeneralError.

      Based on tips at http://blog.ianbicking.org/2007/09/12/re-raising-exceptions/
      """

      (caughtClass, caughtExc, caughtTb) = sys.exc_info()
      if not self.args:
         arg0 = ''
      else:
         arg0 = self.args[0]
      arg0 += '\nRaised because of %s: %s' % (caughtClass, caughtExc)
      self.args = (arg0,) + self.args[1:]
      raise self.__class__, self, caughtTb

class ShellCmdError(GeneralError):
   """The cmd function throws this exception for failed shell commands."""
   
   pass

class FdReader(threading.Thread):
   """Reads a system FD (file descriptor) in a thread and optionally prints it to stdout.
   """

   def __init__(s, readFd, printLines):
      """Initialize data and initialize the thread super class.
      
      Keyword arguments:
      readFd -- integer file descriptor which this thread should read.
      printLines -- a boolean for whether to print the lines to stdout.
      """
      
      threading.Thread.__init__(s)
      s.readFd = readFd
      s.printLines = printLines
      # List of strings which are concatenated to get the full output
      # from reading readFd
      s.outBuf = []
   
   def run(s):
      """Run the thread, which exits when the FD closes."""
      
      while True:
         readLine = s.readFd.readline()
         if not readLine: break
         s.outBuf.append(readLine)
         if s.printLines: print readLine,
         time.sleep(0)

   def getOutput(s):
      return ''.join(s.outBuf)

def cmd(cmdStr, shellStr='sh', background=False, printStdout=False, printStderr=False, printDebug=False, ignoreReturnCode=False):
   """Issue a shell command

   If background is False (the default), the stdout is returned from this function after
   the command finishes.

   If background is True, None is returned right away after fork-executing the command.

   WARNING: cmdStr must be trusted or validated for security.

   Keyword arguments:
   shellStr -- specify the shell to use to execute the command.
   background -- whether to issue the command and return promptly.
   printStdout -- whether to print the stdout.
   printStderr -- whether to print the stderr.
   printDebug -- whether to print internal debugging information, including 
   the command issued and its timing.
   ignoreReturnCode -- True means ignore the return code and return the stdout,
                    False means throw an exception if return code is not 0.
   """

   if printDebug:
      startTime = time.time()
      print 'Using shell:%s to issue command: %s' % (shellStr, cmdStr,)

   # For reference, this is the simpler way to do it if there's no requirement to print
   # output in real time.
   #
   #childSh = subprocess.Popen(shellStr, stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE)
   #(out, err) = childSh.communicate(cmdStr)
   #
   # The FdReader objects don't work in conjunction with the communicate call.  They conflict in their usage
   # of the stdout and stderr pipes.

   childSh = subprocess.Popen(cmdStr, executable=shellStr, bufsize=1, shell=True, 
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)

   fdReaders = { 'stdout':FdReader(childSh.stdout, printStdout), 
            'stderr':FdReader(childSh.stderr, printStderr), }
   for readerI in fdReaders.values():
      readerI.start()
   if background:
      return None
   for readerI in fdReaders.values():
      readerI.join()
   # If we don't wait, returncode was observed to not be up to date.
   childSh.wait()

   if printDebug:
      print('Completed in %f seconds.' % (time.time()-startTime))

   if childSh.returncode!=None and childSh.returncode!=0 and not ignoreReturnCode:
      raise ShellCmdError('Shell command failed with errno:%s cmd:%s stderr:%s' % (
         childSh.returncode,
         cmdStr,
         fdReaders['stderr'].getOutput()))

   return fdReaders['stdout'].getOutput()

def sourceBash(bashFile):
   """Source a Bash file and set its environment variables in the Python environment.

   Reference: http://stackoverflow.com/questions/3503719/emulating-bash-source-in-python

   Keyword arguments:
   bashFile -- string path to the Bash file to source
   """
   envOut = cmd("source %s && env"%(bashFile,), shellStr="bash")
   for envI in envOut.splitlines():
      (key, _, val,) = envI.partition("=")
      os.environ[key] = val

def setupSite():
   """Add Python packages which are not yet installed system-wide."""
   site.addsitedir(getOtsRoot()+'/python/cx_Oracle')

def printSshAdvisory():
   """Print an advisory to the user about SSH."""
   print ( "If prompted for a password, you may cease the prompting by generating SSH keys.  "
            +"Web search 'setup ssh keys' or 'ssh without password' or similar. " )

def getScriptName():
   """Get the name of the current script."""
   return os.path.basename( sys.argv[0] )

def getRhelVersion():
   """Get the version of RHEL in dotted number format.  eg 5.2"""

   # Looks at the /etc/redhat-release file which contains the version number, processing it through sed to
   # extract just the version number.
   (rhelRelease, err) = subprocess.Popen( r"cat /etc/redhat-release | sed 's/.*release \([0-9\.]*\) .*/\1/'",
            stdout=subprocess.PIPE, shell=True).communicate()
   if err!=None:
      print( 'WARNING: While getting RHEL version, output on stderr: %s' % err )
   return rhelRelease.rstrip()

def userTmpDir():
   """Get a temporary directory under the user's home, possibly creating it if it doesn't exist."""
   homeDir = os.path.expanduser("~")
   tmpDir = homeDir+'/py-tmp'
   cmd( 'mkdir -p %s' % (tmpDir,) )
   return tmpDir

def truncateString(inStr, strMaxLen, suffix=''):
   """Truncate a string.

   Take the inputted inStr and append suffix such that the result has a number
   of chars no more than strMaxLen.

   Keyword arguments:
   inStr -- the string to truncate
   strMaxLen -- maximum length for the returned string
   suffix -- the string with which the return should end.
   """
   if strMaxLen<len(suffix):
      raise Exception('Unexpected comparison: %s<%s'%(len(suffix),strMaxLen))
      
   tStr = inStr[:(strMaxLen-len(suffix))]+suffix
   return tStr

def datetime2time(dt):
   """Convert datetime.datetime to time.time.

   Just awkward enough a conversion to warrant a helper function.
   """

   return time.mktime( dt.timetuple() ) + 1e-6*dt.microsecond


