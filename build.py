#! /usr/bin/env python
#
# Build unihome

import os
import sys
sys.path.append(sys.path[0]+'/py')
import generalUtil
sys.path.append(sys.path[0]+'/rc/.emacs.d')
import emacsUtil

def __MAIN__():
   emacsDir = os.path.dirname( emacsUtil.__file__ )
   os.chdir(emacsDir)
   emacsUtil.buildEmacsD()

if __name__=='__main__':
   __MAIN__()

