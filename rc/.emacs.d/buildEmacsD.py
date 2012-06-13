#! /usr/bin/env python
#
# Something quick and dirty to make the third party Emacs code I use.

import os

def buildEvil():
   os.chdir('evil')
   os.system('make')
   os.chdir('..')

def buildAutoComplete():
   os.chdir('auto-complete-1.3.1/')
   os.system('mkdir -p build')
   os.system('make clean')
   os.system('make install DIR=build')
   os.chdir('..')

def buildCedet():
   os.chdir('cedet-1.1')
   # There was a case where cedet*/semantic/Makefile needed a tender touch.
   # Might as well touch 'em all.
   os.system('touch `find . -maxdepth 2 -name Makefile`')
   os.system('make')

def buildJdee():
   # Assumes jde.el was patched to change jde-cedet-max-version .
   patchedFilePath = 'jdee-2.4.0.1/jde.elc'
   if os.path.isfile(patchedFilePath):
      os.system('rm %s'%(patchedFilePath,))

def __MAIN__():
   buildEvil()
   buildAutoComplete()
   buildCedet()
   buildJdee()

if __name__=='__main__':
   __MAIN__()

