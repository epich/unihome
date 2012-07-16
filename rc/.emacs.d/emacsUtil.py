#! /usr/bin/env python
#
# Something quick and dirty to make the third party Emacs code I use.

import os
import sys

def buildEvil():
   os.chdir('evil')
   os.system('make')
   os.chdir('..')

def buildCedet():
   # CEDET contributes to annoying interrogations at Emacs closing time.
   # Create the .semanticdb dir to avoid one interrogation question.
   os.system('mkdir -p ~/.semanticdb')
   os.chdir('cedet-1.1')
   # There was a case where cedet*/semantic/Makefile needed a tender touch.
   # Might as well touch 'em all.
   os.system('touch `find . -maxdepth 2 -name Makefile`')
   os.system('make')

def buildJdee():
   # Assumes jde.el was patched to change jde-cedet-max-version .
   patchedFilePath = 'jdee-2.4.0.1/lisp/jde.elc'
   if os.path.isfile(patchedFilePath):
      os.system('rm %s'%(patchedFilePath,))

def buildEmacsD():
   buildEvil()
   buildCedet()
   buildJdee()

def __MAIN__():
   buildEmacsD()

if __name__=='__main__':
   __MAIN__()

