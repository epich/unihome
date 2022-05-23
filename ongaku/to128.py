#! /usr/bin/env python3

import argparse
import os

parser = argparse.ArgumentParser(description='Reencode to 128kbps mp3s.')
parser.add_argument('files', nargs='+')
args = parser.parse_args()

for f in args.files:
  print("Converting to 128kbps mp3: {}".format(f))
  ret = os.system('mv "{}" "{}.bak"'.format(f,f))
  if ret:
    raise Error("os.system failed with error code: {}".format(ret))
  ret = os.system('ffmpeg -i "{}.bak" -ab 128k "{}".mp3 '.format(f, os.path.splitext(f)[0]))
  if ret:
    raise Error("os.system failed with error code: {}".format(ret))
  ret = os.system('rm "{}.bak"'.format(f))
  if ret:
    raise Error("os.system failed with error code: {}".format(ret))
