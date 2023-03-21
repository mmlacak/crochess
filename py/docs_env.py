#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


import os.path

import py.paths as P


COMPILER_DOCS = 'doxygen'

SOURCE_WS_DOCS_FOLDER = 'ws'


def get_docs_dir( root_path ):
    return os.path.join( root_path, SOURCE_WS_DOCS_FOLDER )


def get_compile_docs_cmd(root_path, compiler_docs=COMPILER_DOCS):
    cmd_lst = [compiler_docs, ]
    cwd_docs = get_docs_dir(root_path)
    return cwd_docs, cmd_lst
