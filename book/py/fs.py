#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2017 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import os
import os.path


SUBFOLDERS =  [ 'boards', \
                'castlings', \
                'castlings/long_left', \
                'castlings/short_right', \
                'en_passants', \
                'examples', \
                'pieces' ]


def find_subfolder(path=None):
    if path is None:
        path = os.getcwd()
    return os.path.abspath(path)

def mkdirs(folder_name):
    path = os.path.normpath(os.path.abspath(folder_name))
    if not os.path.isdir(path):
        print folder_name
        os.makedirs(path)

def create_subfolders(path=None):
    print
    old = os.getcwd()
    print "Old:", old
    try:
        root = find_subfolder(path=path)
        print "Root:", root
        os.chdir(root)

        print
        print "Subfolders:"
        for folder_name in SUBFOLDERS:
            mkdirs(folder_name)
    finally:
        print "Restoring:", old
        os.chdir(old)

    print


if __name__ == '__main__':
    create_subfolders()
