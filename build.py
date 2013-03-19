#! /usr/bin/env python
#
# Build unihome

import getopt
import os
import sys
sys.path.append(sys.path[0]+'/py')
import generalUtil
sys.path.append(sys.path[0]+'/rc/.emacs.d')
import emacsUtil

scriptHelp_g = """Usage: %s [options]

Build unihome.

Options:
  -h, --help  Print this help.
  --no-cedet  Don't build CEDET"""

def __MAIN__():
   buildCedet = True

   # Parse args
   #
   (opts, args) = getopt.getopt(sys.argv[1:], 'h', ["help", "no-cedet",])
   for optI, argI in opts:
      if optI in ('-h', '--help'):
         print(scriptHelp_g)
         sys.exit(0)
      if optI in ('--no-cedet'):
         buildCedet = False

   emacsDir = os.path.dirname( emacsUtil.__file__ )
   os.chdir(emacsDir)
   emacsUtil.buildEmacsD(buildCedet)

if __name__=='__main__':
   __MAIN__()

