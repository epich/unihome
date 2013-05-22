#! /usr/bin/env python
#
# Utilities for managing Elisp.  Byte compiling primarily.

import os
import sys
# Bootstrap generalUtil
sys.path.append( '~/unihome/py' )
import generalUtil

evilDir_g = 'evil'

def byteCompile(filesString, loadPathL=[]):
   """Byte compile the inputted elisp files.

   loadPathL -- list of load paths, passed to emacs -L option.  ie paths to dependencies.
   filesString -- string which is passed to command line.  Example: '*.el'
   """
   loadPathOpts = ['-L '+loadPathI+' ' for loadPathI in loadPathL]
   generalUtil.cmd( 'emacs --batch -Q %s -f batch-byte-compile %s'%(''.join(loadPathOpts), filesString,), printDebug=True, printStdout=True )

def buildEvil():
   generalUtil.cmd('make -C %s'%(evilDir_g,), printStdout=True)

def buildMiscElisp():
   """Build miscellaneous Elisp.  Covers Evil because of Evil's dependency on miscellaneous Elisp. """
   generalUtil.cmd('make -f misc-elisp.mk', printStdout=True)

def buildMy():
   byteCompile('my/my-util.el')
   byteCompile('my/my-proj.el')
   byteCompile('my/my-config.el')

def buildEmacsD():
   # Create directories Emacs expects
   generalUtil.cmd('mkdir -p ~/.semanticdb ~/emacs-data ~/emacs-data/backup ~/emacs-data/semanticdb')
   buildMiscElisp()
   buildEvil()
   buildMy()

def __MAIN__():
   buildEmacsD()

if __name__=='__main__':
   __MAIN__()

