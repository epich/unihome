#! /usr/bin/env python
#
# Utilities for managing Elisp.  Byte compiling primarily.

import os
import sys
# Bootstrap myutil
sys.path.append(sys.path[0]+'/py')
import myutil

evilDir_g = 'evil'

def byteCompile(filesString, loadPathL=[]):
   """Byte compile the inputted elisp files.

   loadPathL -- list of load paths, passed to emacs -L option.  ie paths to dependencies.
   filesString -- string which is passed to command line.  Example: '*.el'
   """
   loadPathOpts = ['-L '+loadPathI+' ' for loadPathI in loadPathL]
   myutil.cmd( 'emacs --batch -Q %s -f batch-byte-compile %s'%(''.join(loadPathOpts),
                                                                    filesString,),
                    printDebug=True,
                    printStdout=True,
                    printStderr=True )

def buildEvil():
   myutil.cmd('make -C %s'%(evilDir_g,), printStdout=True)

def buildMiscElisp():
   """Build miscellaneous Elisp.  Covers Evil because of Evil's dependency on miscellaneous Elisp. """
   myutil.cmd('make -f misc-elisp.mk', printStdout=True)

def buildMy():
   byteCompile('my/my-util.el')
   byteCompile('my/my-proj.el')
   byteCompile('my/my-config.el', loadPathL=['my', 'evil',])

def buildEmacsD():
   # Create directories Emacs expects
   for dirI in ['~/.emacs.d', '~/.emacs.d/semanticdb', '~/.emacs.d/backup',]:
      if not os.path.exists(dirI):
         myutil.cmd('mkdir -p %s'%(dirI,))
   # Make sure abbrev_defs exists or I'll get an annoying prompt
   myutil.cmd('touch ~/.emacs.d/abbrev_defs')
   buildMiscElisp()
   buildEvil()
   buildMy()

def __MAIN__():
   buildEmacsD()

if __name__=='__main__':
   __MAIN__()
