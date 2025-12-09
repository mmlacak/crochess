#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2017 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

import os
import os.path


SUBFOLDERS =    [   \
                    'boards', \
                    \
                    'castlings', \
                    'castlings/02_c', \
                    'castlings/04_ct', \
                    'castlings/06_ma', \
                    'castlings/08_aoa', \
                    'castlings/10_mv', \
                    'castlings/12_n', \
                    'castlings/14_hd', \
                    'castlings/16_tr', \
                    'castlings/18_cot', \
                    'castlings/20_d', \
                    'castlings/22_o', \
                    'castlings/24_c14', \
                    'castlings/26_c20', \
                    'castlings/28_c26', \
                    'castlings/30_ct14', \
                    'castlings/32_ct20', \
                    'castlings/34_ct26', \
                    'castlings/24_c14m', \
                    'castlings/26_c20m', \
                    'castlings/28_c26m', \
                    'castlings/30_ct14m', \
                    'castlings/32_ct20m', \
                    'castlings/34_ct26m', \
                    'castlings/24_c14s', \
                    'castlings/26_c20s', \
                    'castlings/28_c26s', \
                    'castlings/30_ct14s', \
                    'castlings/32_ct20s', \
                    'castlings/34_ct26s', \
                    \
                    'en_passants', \
                    \
                    'examples', \
                    'examples/02_c', \
                    'examples/04_ct', \
                    'examples/06_ma', \
                    'examples/08_aoa', \
                    'examples/10_mv', \
                    'examples/12_n', \
                    'examples/14_hd', \
                    'examples/16_tr', \
                    'examples/18_cot', \
                    'examples/20_d', \
                    'examples/22_o', \
                    'examples/24_c14', \
                    'examples/26_c20', \
                    'examples/28_c26', \
                    'examples/30_ct14', \
                    'examples/32_ct20', \
                    'examples/34_ct26', \
                    'examples/24_c14m', \
                    'examples/26_c20m', \
                    'examples/28_c26m', \
                    'examples/30_ct14m', \
                    'examples/32_ct20m', \
                    'examples/34_ct26m', \
                    'examples/24_c14s', \
                    'examples/26_c20s', \
                    'examples/28_c26s', \
                    'examples/30_ct14s', \
                    'examples/32_ct20s', \
                    'examples/34_ct26s', \
                    \
                    'pieces', \
                    'pieces/bishop', \
                    'pieces/star', \
                    \
                    'rush', \
                    \
                    'isa', \
                    'isa/02_c', \
                    'isa/04_ct', \
                    'isa/06_ma', \
                    'isa/08_aoa', \
                    'isa/10_mv', \
                    'isa/12_n', \
                    'isa/14_hd', \
                    'isa/16_tr', \
                    'isa/18_cot', \
                    'isa/20_d', \
                    'isa/22_o', \
                    'isa/24_c14', \
                    'isa/26_c20', \
                    'isa/28_c26', \
                    'isa/30_ct14', \
                    'isa/32_ct20', \
                    'isa/34_ct26', \
                    'isa/24_c14m', \
                    'isa/26_c20m', \
                    'isa/28_c26m', \
                    'isa/30_ct14m', \
                    'isa/32_ct20m', \
                    'isa/34_ct26m', \
                    'isa/24_c14s', \
                    'isa/26_c20s', \
                    'isa/28_c26s', \
                    'isa/30_ct14s', \
                    'isa/32_ct20s', \
                    'isa/34_ct26s', \
                    \
                    'test', \
                ]


def find_subfolder( path=None ):
    if path is None:
        path = os.getcwd()
    return os.path.abspath( path )

def mkdirs( folder_name ):
    path = os.path.normpath( os.path.abspath( folder_name ) )
    if not os.path.isdir( path ):
        print( folder_name )
        os.makedirs( path )

def create_subfolders( path=None ):
    print
    old = os.getcwd()
    print( "Old:", old )
    try:
        root = find_subfolder( path=path )
        print( "Root:", root )
        os.chdir( root )

        print
        print( "Subfolders:" )
        for folder_name in SUBFOLDERS:
            mkdirs( folder_name )
    finally:
        print( "Restoring:", old )
        os.chdir( old )

    print


if __name__ == '__main__':
    create_subfolders()
