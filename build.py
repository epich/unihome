#! /usr/bin/env python
#
# Build unihome

import getopt
import os
import sys
# Bootstrap myutil
sys.path.append(sys.path[0]+'/py')
import myutil

scriptHelp_g = """Usage: %s [options]

Build unihome.

Options:
  -h, --help  Print this help.

Arguments:
  [default]     Build"""

def buildEmacs():
   os.chdir(sys.path[0]+'/emacs')
   # Create directories Emacs expects
   for dirI in ['~/.emacs.d', '~/.emacs.d/semanticdb', '~/.emacs.d/backup',]:
      if not os.path.exists(dirI):
         myutil.cmd('mkdir -p %s'%(dirI,))
   # Make sure abbrev_defs exists or I'll get an annoying prompt
   myutil.cmd('touch ~/.emacs.d/abbrev_defs')
   myutil.cmd(
      'emacs --batch -L my --eval "{}" -f batch-byte-compile my/*.el'.format(
         # Need to package-initialize because byte compiled files
         # reference packages
         "(progn (require 'package) (package-initialize))"),
      printDebug=True,
      printStdout=True,
      printStderr=True)

def __MAIN__():
   # Parse args
   #
   (opts, args) = getopt.getopt(sys.argv[1:], 'h', ["help", ])
   for optI, argI in opts:
      if optI in ('-h', '--help'):
         print(scriptHelp_g)
         sys.exit(0)

   buildEmacs()

if __name__=='__main__':
   __MAIN__()
