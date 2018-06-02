#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2017, 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import os
import os.path


SUBFOLDERS =  [ 'boards', \
                \
                'castlings', \
                'castlings/01_oc', \
                'castlings/02_c', \
                'castlings/03_oct', \
                'castlings/04_ct', \
                'castlings/05_oma', \
                'castlings/06_ma', \
                'castlings/07_oaoa', \
                'castlings/08_aoa', \
                'castlings/09_omv', \
                'castlings/10_mv', \
                'castlings/11_on', \
                'castlings/12_n', \
                'castlings/13_ohd', \
                'castlings/14_hd', \
                'castlings/15_otr', \
                'castlings/16_tr', \
                'castlings/17_ocot', \
                'castlings/18_cot', \
                'castlings/19_od', \
                'castlings/20_d', \
                'castlings/21_oo', \
                'castlings/22_o', \
                \
                'en_passants', \
                \
                'examples', \
                'examples/01_oc', \
                'examples/02_c', \
                'examples/03_oct', \
                'examples/04_ct', \
                'examples/05_oma', \
                'examples/06_ma', \
                'examples/07_oaoa', \
                'examples/08_aoa', \
                'examples/09_omv', \
                'examples/10_mv', \
                'examples/11_on', \
                'examples/12_n', \
                'examples/13_ohd', \
                'examples/14_hd', \
                'examples/15_otr', \
                'examples/16_tr', \
                'examples/17_ocot', \
                'examples/18_cot', \
                'examples/19_od', \
                'examples/20_d', \
                'examples/21_oo', \
                'examples/22_o', \
                \
                'pieces', \
                'pieces/star', \
                'rush' ]


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
