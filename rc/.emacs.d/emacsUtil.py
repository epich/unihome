#! /usr/bin/env python
#
# Utilities for managing Elisp.  Byte compiling primarily.

import os
import sys
# Bootstrap generalUtil
sys.path.append( '~/unihome/py' )
import generalUtil

evilDir_g = 'evil'
cedetDir_g = 'cedet-1.1'
jdeeVersion_g = '2.4.0.1'

def byteCompile(filesString, loadPathL=[]):
   """Byte compile the inputted elisp files.

   loadPathL -- list of load paths, passed to emacs -L option.  ie paths to dependencies.
   filesString -- string which is passed to command line.  Example: '*.el'
   """
   loadPathOpts = ['-L '+loadPathI+' ' for loadPathI in loadPathL]
   generalUtil.cmd( 'emacs --batch -Q %s -f batch-byte-compile %s'%(''.join(loadPathOpts), filesString,), printDebug=True, printStdout=True )

def buildEvil():
   generalUtil.cmd('make -C %s'%(evilDir_g,), printStdout=True)

def buildGotoChg():
   byteCompile('evil/lib/goto-chg.el')

def buildUndoTree():
   byteCompile('evil/lib/undo-tree.el')

def buildRainbowDelimiters():
   byteCompile('rainbow-delimiters/rainbow-delimiters.el')
   
def buildCedet():
   # CEDET contributes to annoying interrogations at Emacs closing time.
   # Create the .semanticdb dir to avoid one interrogation question.
   generalUtil.cmd('mkdir -p ~/.semanticdb')
   # There was a case where cedet*/semantic/Makefile needed a tender touch.
   # Might as well touch 'em all.
   generalUtil.cmd('touch `find %s -name Makefile`'%(cedetDir_g,))
   generalUtil.cmd('make -C %s'%(cedetDir_g,), printStdout=True)
   # CEDET seems to pointlessly modify source controlled files.
   # Revert them for version control convenience.
   revertL = ['cedet-1.1/cogre/cogre-loaddefs.el',
              'cedet-1.1/common/cedet-loaddefs.el',
              'cedet-1.1/contrib/contrib-loaddefs.el',
              'cedet-1.1/ede/ede-loaddefs.el',
              'cedet-1.1/eieio/eieio-loaddefs.el',
              'cedet-1.1/semantic/semantic-loaddefs.el',
              'cedet-1.1/speedbar/speedbar-loaddefs.el',
              'cedet-1.1/srecode/srecode-loaddefs.el',
              ]
   generalUtil.cmd('hg revert %s'%(' '.join(revertL),), printStdout=True)

def buildJdee():
   # Simply unzip binary distribution.
   if not os.path.exists('jdee-%s'%(jdeeVersion_g)):
      generalUtil.cmd('unzip jdee-bin-%s.zip'%(jdeeVersion_g))

def buildMy():
   byteCompile('my/my-util.el')
   byteCompile('my/my-config.el')

def buildEmacsD(buildCedetP):
   buildEvil()
   buildUndoTree()
   buildGotoChg()
   buildRainbowDelimiters()
   if buildCedetP:
      buildCedet()
   buildJdee()
   buildMy()

def __MAIN__():
   buildEmacsD()

if __name__=='__main__':
   __MAIN__()

