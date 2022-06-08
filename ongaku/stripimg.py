#! /usr/bin/env python3

import argparse
# Merely importing allows left and right arrow keys when entering input
import readline
import os
import re

parser = argparse.ArgumentParser(
    description='Strip image from mp3 files. Downloads from Youtube often have image with botched aspect ratio.')
parser.add_argument('files', nargs='+')
args = parser.parse_args()

for f in args.files:
  os.system('eyeD3 --remove-all-images "{}"'.format(f))
