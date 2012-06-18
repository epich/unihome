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
   pass

class FdReader(threading.Thread):
   def __init__(s, readFd, printLines):
      threading.Thread.__init__(s)
      s.readFd = readFd
      s.printLines = printLines
      s.outBuf = ''

   def run(s):
      while True:
         readLine = s.readFd.readline()
         if not readLine: break
         s.outBuf += readLine
         if s.printLines: print readLine,
         time.sleep(0)

def cmd(cmdStr, shellStr='sh', background=False, printStdout=False, printStderr=False, printDebug=False):
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
   (out, err) = (fdReaders['stdout'].outBuf, fdReaders['stderr'].outBuf)

   if printDebug:
      print('Completed in %f seconds.' % (time.time()-startTime))

   # If we don't wait, returncode may not be up to date yet.
   childSh.wait()
   if childSh.returncode!=None and childSh.returncode!=0:
      raise ShellCmdError('Shell command failed with errno:%s cmd:%s stderr:%s' % (childSh.returncode,cmdStr,err))

   return out

def setupSite():
   # Add packages which we need but which sysadmin hasn't installed as system-wide package.
   site.addsitedir('/proj/user/boreilly/ots/lib/python2.6/site-packages')

def printSshAdvisory():
   print ( "If prompted for a password, you may cease the prompting by generating SSH keys.  "
            +"Web search 'setup ssh keys' or 'ssh without password' or similar. " )

def getProjRoot():
   return re.sub(r'(.*vobs\/proj).*', r'\1', sys.path[0])

def getRhelVersion():
   """Get the version of RHEL in dotted number format.  eg 5.2"""

   (rhelRelease, err) = subprocess.Popen( r"cat /etc/redhat-release | sed 's/.*release \([0-9\.]*\) .*/\1/'",
            stdout=subprocess.PIPE, shell=True).communicate()
   if err!=None:
      print( 'WARNING: While getting RHEL version, output on stderr: %s' % err )
   return rhelRelease.rstrip()

def userTmpDir():
   homeDir = os.path.expanduser("~")
   tmpDir = homeDir+'/proj-tmp'
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

