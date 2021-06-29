#! /usr/bin/env python3

import argparse
import os

parser = argparse.ArgumentParser(
    description='Add prefix to files interactively. Intended to induce a lexigraphical ordering of music files within a folder.')
parser.add_argument('files', nargs='+')
args = parser.parse_args()

for f in args.files:
  prefix = input("Enter prefix for {}: ".format(f))
  os.system('mv "{}" "{}/{}{}"'.format(f, os.path.dirname(f), prefix, os.path.basename(f)))
