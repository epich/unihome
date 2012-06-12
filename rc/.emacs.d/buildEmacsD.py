#! /usr/bin/env python
#
# Something quick and dirty to make the third party Emacs code I use.

import os

def buildEvil():
   os.chdir('evil')
   os.system('make clean')
   os.system('make')
   os.chdir('..')

def buildAutoComplete():
   os.chdir('auto-complete-1.3.1/')
   os.system('mkdir -p build')
   os.system('make clean')
   os.system('make install DIR=build')
   os.chdir('..')

def __MAIN__():
   buildEvil()
   buildAutoComplete()

if __name__=='__main__':
   __MAIN__()

