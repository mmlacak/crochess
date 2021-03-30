#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import os.path


def get_abs_combed_path(path):
    path = os.path.abspath( path )
    path = os.path.normcase( path )
    path = os.path.normpath( path )

    return path

def get_combed_path(path):
    path = os.path.normcase( path )
    path = os.path.normpath( path )

    return path

def get_project_root_path(main_script_path):
    path = os.path.join(os.getcwd(), main_script_path)
    path = os.path.dirname( path )
    path = get_abs_combed_path( path )

    return path

def get_rel_path_or_abs(path, start=None):
    _path = path
    if start is not None:
        _path = os.path.relpath(_path, start)
    return _path
