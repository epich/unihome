#! /usr/bin/env python3

import argparse
# Merely importing allows left and right arrow keys when entering input
import readline
import os
import re

parser = argparse.ArgumentParser(
    description='Rename file based on regex.')
parser.add_argument('files', nargs='+')
args = parser.parse_args()

old_prior = None
new_prior = None
for f in args.files:
  old = input("Enter regex for old{}. {}: "
              .format(" ({})".format(old_prior) if old_prior else "",
                      f))
  new = input("Enter regex for new{}. {}: "
              .format(" ({})".format(new_prior) if new_prior else "",
                      f))
  if old:
    old_prior = old
  else:  
    old = old_prior
  if new:
    new_prior = new
  else:  
    new = new_prior
  
  new_file = re.sub(old, new, os.path.basename(f))
  print("Moving to: {}".format(new_file))
  os.system('mv "{}" "{}"'.format(
    f,
    os.path.join(os.path.dirname(f),
                 new_file)))
