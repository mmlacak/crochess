#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


import os.path

import py.paths as P


DOCS_COMPILER = 'make'

DOCS_OPTION_CLEAN = 'clean'
DOCS_OPTION_HTML = 'html'
DOCS_OPTION_PDF = 'latexpdf'

DOCS_FOLDER = 'docs'
DOCS_SOURCE_FOLDER = 'docs/source'
DOCS_BUILD_FOLDER = 'docs/build'


def get_docs_dir( root_path ):
    return os.path.join( root_path, DOCS_FOLDER )


def get_compile_docs_cmd(root_path, docs_option=DOCS_OPTION_HTML):
    cmd_lst = [DOCS_COMPILER, docs_option, ]
    cwd_docs = get_docs_dir(root_path)
    return cwd_docs, cmd_lst
