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
   os.chdir('ac/auto-complete-1.3.1/')
   os.system('mkdir -p build')
   os.system('make clean')
   # Java Auto Complete requires a patch to Auto Complete.
   #
   # It appears it was accepted upstream:
   #  https://github.com/jixiuf/auto-complete/commit/6e1e832d2ee07ef3155dce92181c90bd3ff3d4ef
   # However, the latest pull of auto-complete didn't work right away, so I use
   # version 1.3.1 which needs the patch.
   os.system('patch --forward -p0 <../../ajc-java-complete/popup-patch.diff')
   os.system('make install DIR=build')
   os.chdir('../..')

def buildYasnippet():
   # Distribution's install docs don't mention a compilation step.
   # It contains a Rakefile, but I don't know if I want to
   # fuss with the expectation that rake is available.
   pass

def buildJavaAutoComplete():
   os.chdir('ajc-java-complete')
   os.system('bunzip2 -k java_base.tag.bz2')
   os.chdir('..')

def __MAIN__():
   buildEvil()
   buildAutoComplete()
   buildYasnippet()
   buildJavaAutoComplete()

if __name__=='__main__':
   __MAIN__()

