#!/usr/bin/env python
# -*- coding: utf-8 -*-

import logging
import subprocess
import sys

import wrap_tool


def main (argv):
    try:
        logging.basicConfig ()
        return wrap_tool.wrap_tool (tool='link', argv=argv[1:])
    except subprocess.CalledProcessError:
        return 1

if __name__ == '__main__':
    sys.exit (main (sys.argv))
# eof: link.py
