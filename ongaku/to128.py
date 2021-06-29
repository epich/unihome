#! /usr/bin/env python3

import argparse
import os

parser = argparse.ArgumentParser(description='Reencode to 128kbps mp3s.')
parser.add_argument('files', nargs='+')
args = parser.parse_args()

for f in args.files:
  print("Converting to 128kbps mp3: {}".format(f))
  os.system('mv "{}" "{}.bak"'.format(f,f))
  os.system('ffmpeg -i "{}.bak" -ab 128k "{}".mp3 '.format(f, os.path.splitext(f)[0]))
  os.system('rm "{}.bak"'.format(f))
