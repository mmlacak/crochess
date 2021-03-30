#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import os.path

import py.paths as P



#
# ### $ echo $LD_LIBRARY_PATH
# ### /usr/lib64/:.:./lib:../lib:..
#
# $ export LD_LIBRARY_PATH=.:./lib:../lib:..
#
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# $ gdc -shared -fPIC -L. -defaultlib=phobos2 lib.d -o libarr.so
#
# $ gdc -L. -larr  array.d -o arr
#
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# $ dmd -shared -fPIC -L-L. -defaultlib=phobos2 lib.d -of=libarr.so
#
# $ dmd -defaultlib=phobos2 -L-L. -L-larr  array.d -of=arr
#
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# $ ldc2 -shared --relocation-model=pic -L-L. -defaultlib=phobos2-ldc lib.d -of=libarr.so
# ### $ ldc2 -shared --relocation-model=pic -L-L.  lib.d -of=libarr.so
#
# $ ldc2 -L-L. -L-larr  array.d -of=arr
#



COMPILER_GDC = 'gdc'
COMPILER_LDC2 = 'ldc2'
COMPILER_DMD = 'dmd'

DEFAULT_COMPILER = COMPILER_GDC

EXECUTABLE_FILE_NAME = 'crochess'
LIBRARY_FILE_NAME = 'libcrochess.so'
EXECUTABLE_DEBUG_FOLDER = 'debug'
EXECUTABLE_RELEASE_FOLDER = 'release'


OPTIONS_GDC_DEBUG = ['-Wall', '-pedantic', ]
OPTIONS_GDC_RELEASE = ['-Wall', '-pedantic', '-O3', ]

OPTIONS_LDC2_DEBUG = ['', ]
OPTIONS_LDC2_RELEASE = ['-O3', ]

OPTIONS_DMD_DEBUG = ['', ]
OPTIONS_DMD_RELEASE = ['-O', ]


SOURCE_WS_FOLDER = 'ws'
SOURCE_IGNORE_FILE_PATH = '.IGNORE.'
SOURCE_FILE_EXT = '.d'

SOURCE_APP_FOLDER = 'crochess'
SOURCE_APP_SRC_FOLDER = 'src'

SOURCE_LIB_FOLDER = 'libcrochess'
SOURCE_LIB_SRC_FOLDER = 'src'


def get_default_compiler_options(compiler=DEFAULT_COMPILER, is_release_or_debug=False):
    if compiler == COMPILER_GDC:
        return OPTIONS_GDC_RELEASE if is_release_or_debug else OPTIONS_GDC_DEBUG
    elif compiler == COMPILER_LDC2:
        return OPTIONS_LDC2_RELEASE if is_release_or_debug else OPTIONS_LDC2_DEBUG
    elif compiler == COMPILER_DMD:
        return OPTIONS_DMD_RELEASE if is_release_or_debug else OPTIONS_DMD_DEBUG
    else:
        raise RuntimeError("Unknown compiler '%s'." % compiler) # return []

def get_output_compiler_options(root_path, cmd_cwd, compiler=DEFAULT_COMPILER, is_release_or_debug=False):
    out_file = get_exec_file_path(root_path, is_release_or_debug=is_release_or_debug) # EXECUTABLE_FILE_NAME

    out_file = P.get_rel_path_or_abs(out_file, cmd_cwd)

    out_lst = []
    if compiler == COMPILER_GDC:
        out_lst = ['-o', out_file, ]
    elif compiler == COMPILER_LDC2:
        out_lst = ['-of=%s' % out_file, ]
    elif compiler == COMPILER_DMD:
        out_lst = ['-of=%s' % out_file, ]
    else:
        raise RuntimeError("Unknown compiler '%s'." % compiler)

    return out_lst

def get_exec_file_path(root_path, is_release_or_debug=False):
    if is_release_or_debug:
        return os.path.join(get_app_release_dir(root_path), EXECUTABLE_FILE_NAME)
    else:
        return os.path.join(get_app_debug_dir(root_path), EXECUTABLE_FILE_NAME)


def get_app_dir(root_path):
    return os.path.join(root_path, SOURCE_WS_FOLDER, SOURCE_APP_FOLDER)

def get_app_source_dir(root_path):
    return os.path.join(get_app_dir(root_path), SOURCE_APP_SRC_FOLDER)

def get_app_debug_dir(root_path):
    return os.path.join(get_app_dir(root_path), EXECUTABLE_DEBUG_FOLDER)

def get_app_release_dir(root_path):
    return os.path.join(get_app_dir(root_path), EXECUTABLE_RELEASE_FOLDER)


def get_lib_dir(root_path):
    return os.path.join(root_path, SOURCE_WS_FOLDER, SOURCE_LIB_FOLDER)

def get_lib_source_dir(root_path):
    return os.path.join(get_lib_dir(root_path), SOURCE_LIB_SRC_FOLDER)

def get_lib_debug_dir(root_path):
    return os.path.join(get_lib_dir(root_path), EXECUTABLE_DEBUG_FOLDER)

def get_lib_release_dir(root_path):
    return os.path.join(get_lib_dir(root_path), EXECUTABLE_RELEASE_FOLDER)


def get_source_path_list(cmd_cwd, src_dir):
    full_path = os.path.join(cmd_cwd, src_dir)
    file_lst = os.listdir(full_path)
    new_lst  = [ os.path.join(src_dir, f) for f in file_lst
                    if SOURCE_IGNORE_FILE_PATH not in f
                        and os.path.isfile( os.path.join(full_path, f) )
                        and os.path.splitext(f)[ 1 ] == SOURCE_FILE_EXT ]
    return new_lst


def get_compile_app_cmd(root_path, rel_path=None, compiler=DEFAULT_COMPILER, is_release_or_debug=False, adx_options_list=None):
    cmd_lst = [compiler, ]

    cmd_lst += get_default_compiler_options(compiler=compiler, is_release_or_debug=is_release_or_debug)

    if adx_options_list is not None:
        cmd_lst += adx_options_list

    cmd_cwd = rel_path or get_app_dir(root_path) # os.getcwd())
    src_dir = P.get_rel_path_or_abs(get_app_source_dir(root_path), cmd_cwd) # get_app_source_dir(root_path)
    cmd_lst += get_source_path_list(cmd_cwd, src_dir)

    cmd_lst += get_output_compiler_options(root_path, cmd_cwd, compiler=compiler, is_release_or_debug=is_release_or_debug)

    return cmd_cwd, cmd_lst


def get_ls_exe_file_cmd(root_path, is_release_or_debug=False):
    cmd_str = get_exec_file_path(root_path, is_release_or_debug=is_release_or_debug)
    cmd_lst = ['ls', '-Fal', cmd_str, ]
    return cmd_lst

def get_run_exe_file_cmd(root_path, is_release_or_debug=False, options_list=None):
    cmd_str = get_exec_file_path(root_path, is_release_or_debug=is_release_or_debug)

    cmd_lst = [cmd_str, ]

    if options_list is not None:
        cmd_lst += options_list

    return cmd_lst
