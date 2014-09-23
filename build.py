#! /usr/bin/env python
#
# Build unihome

import getopt
import os
import sys
sys.path.append(sys.path[0]+'/emacs')
import emacsUtil

scriptHelp_g = """Usage: %s [options]

Build unihome.

Options:
  -h, --help  Print this help.

Arguments:
  [default]     Build"""

def __MAIN__():
   # Parse args
   #
   (opts, args) = getopt.getopt(sys.argv[1:], 'h', ["help", ])
   for optI, argI in opts:
      if optI in ('-h', '--help'):
         print(scriptHelp_g)
         sys.exit(0)

   emacsDir = os.path.dirname( emacsUtil.__file__ )
   os.chdir(emacsDir)
   emacsUtil.buildEmacsD()

if __name__=='__main__':
   __MAIN__()
