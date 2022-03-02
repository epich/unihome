#! /usr/bin/env python3

import argparse
import os
import re

parser = argparse.ArgumentParser(
    description='Rename file based on regex.')
parser.add_argument('files', nargs='+')
args = parser.parse_args()

for f in args.files:
  # TODO: Present default based on last input
  old = input("Enter regex for old in: {}: ".format(f))
  new = input("Enter regex for new in: {}: ".format(f))
  new_file = re.sub(old, new, os.path.basename(f))
  os.system('mv "{}" "{}"'.format(
    f,
    os.path.join(os.path.dirname(f),
                 new_file)))
