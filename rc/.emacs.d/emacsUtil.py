#! /usr/bin/env python
#
# Utilities for managing Elisp.  Byte compiling primarily.

import os
import sys
# Bootstrap generalUtil
sys.path.append( '~/unihome/py' )
import generalUtil

evilDir_g = 'evil'
rainbowDelimitersDir_g = 'rainbow-delimiters'
cedetDir_g = 'cedet-1.1'
jdeeDir_g = 'jdee-2.4.0.1'
pareditDir_g = './paredit'

def byteCompile(filesString, loadPathL=[]):
   """Byte compile the inputted elisp files.

   loadPathL -- list of load paths, passed to emacs -L option.  ie paths to dependencies.
   filesString -- string which is passed to command line.  Example: '*.el'
   """
   loadPathOpts = ['-L '+loadPathI+' ' for loadPathI in loadPathL]
   generalUtil.cmd( 'emacs --batch -Q %s -f batch-byte-compile %s'%(''.join(loadPathOpts), filesString,), printStdout=True )

def buildEvil():
   generalUtil.cmd('make -C %s'%(evilDir_g,), printStdout=True)
   
def buildRainbowDelimiters():
   byteCompile(rainbowDelimitersDir_g+'/rainbow-delimiters.el')
   
def buildCedet():
   # CEDET contributes to annoying interrogations at Emacs closing time.
   # Create the .semanticdb dir to avoid one interrogation question.
   generalUtil.cmd('mkdir -p ~/.semanticdb')
   # There was a case where cedet*/semantic/Makefile needed a tender touch.
   # Might as well touch 'em all.
   generalUtil.cmd('touch `find %s -name Makefile`'%(cedetDir_g,))
   generalUtil.cmd('make -C %s'%(cedetDir_g,), printStdout=True)

def buildJdee():
   # Assumes jde.el was patched to change jde-cedet-max-version .
   jdeeLispDir = jdeeDir_g+'/lisp'
   patchedFilePathNoExt = '%s/jde'%(jdeeLispDir,)
   if os.path.isfile(patchedFilePathNoExt+'.elc'):
      generalUtil.cmd('rm %s'%(patchedFilePathNoExt+'.elc',))
   # TODO: Doesn't compile
   #byteCompile(patchedFilePathNoExt+'.el', [cedetDir_g+'/semantic', jdeeLispDir,])

def buildParedit():
   byteCompile(pareditDir_g+'/paredit.el')

def buildInitElisp():
   # TODO: Runtime errors
   pass#byteCompile('init.el', [evilDir_g, pareditDir_g, TODO,])

def buildEmacsD():
   buildEvil()
   buildRainbowDelimiters()
   buildCedet()
   buildJdee()
   buildParedit()
   buildInitElisp()

def __MAIN__():
   buildEmacsD()

if __name__=='__main__':
   __MAIN__()

