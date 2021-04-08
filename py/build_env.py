#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import os.path

import py.paths as P


#
# $ export LD_LIBRARY_PATH=.:./lib:../lib:..:../../bin:ws/bin:~/src/crochess/ws/bin
#
# $ export PATH=${PATH}:~/src/crochess/ws/bin
#
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# gcc -Wall -pedantic -O3 -shared -fPIC -I../inc libcrochess.c -o ../../bin/libcrochess.so # in ./ws/libcrochess/src
#
# clang -Wall -pedantic -O3 -shared -fPIC -I../inc libcrochess.c -o ../../bin/libcrochess.so # in ./ws/libcrochess/src
#
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# gcc -O3 -Wall --pedantic -L../../bin -lcrochess -I../../libcrochess/inc -I../inc crochess.c -o ../../bin/crochess # in ./ws/crochess/src
#
# clang -O3 -Wall --pedantic -L../../bin -lcrochess -I../../libcrochess/inc -I../inc crochess.c -o ../../bin/crochess # in ./ws/crochess/src
#


COMPILER_GCC = 'gcc'
COMPILER_CLANG = 'clang'

DEFAULT_COMPILER = COMPILER_GCC

EXECUTABLE_FILE_NAME = 'crochess'
LIBRARY_FILE_NAME = 'libcrochess.so'
OBJECT_FILE_EXTENSIONS = ['.o', '.obj', ]
BUILD_BIN_FOLDER = 'bin'


OPTIONS_GCC_DEBUG = ['-Wall', '-pedantic', '-ggdb', ]
OPTIONS_GCC_RELEASE = ['-Wall', '-pedantic', '-O3', ]

OPTIONS_CLANG_DEBUG = ['-Wall', '-pedantic', '-ggdb', ]
OPTIONS_CLANG_RELEASE = ['-Wall', '-pedantic', '-O3', ]


OPTIONS_GCC_LIBRARY = ['--shared', '-fPIC', '-I../inc', ]
OPTIONS_GCC_EXECUTABLE = ['-L../../bin', '-lcrochess', '-I../../libcrochess/inc', '-I../inc', ]

OPTIONS_CLANG_LIBRARY = ['--shared', '-fPIC', '-I../inc', ]
OPTIONS_CLANG_EXECUTABLE = ['-L../../bin', '-lcrochess', '-I../../libcrochess/inc', '-I../inc', ]


SOURCE_WS_FOLDER = 'ws'
SOURCE_IGNORE_FILE_PATH = '.IGNORE.'
SOURCE_FILE_EXT = '.c'
HEADER_FILE_EXT = '.h'

SOURCE_APP_FOLDER = 'crochess'
SOURCE_APP_SRC_FOLDER = 'src'
SOURCE_APP_HEADER_FOLDER = 'inc'

SOURCE_LIB_FOLDER = 'libcrochess'
SOURCE_LIB_SRC_FOLDER = 'src'
SOURCE_LIB_HEADER_FOLDER = 'inc'


def get_compiler_optimization_options(compiler=DEFAULT_COMPILER, is_release_or_debug=False):
    if compiler == COMPILER_GCC:
        return OPTIONS_GCC_RELEASE if is_release_or_debug else OPTIONS_GCC_DEBUG
    elif compiler == COMPILER_CLANG:
        return OPTIONS_CLANG_RELEASE if is_release_or_debug else OPTIONS_CLANG_DEBUG
    else:
        raise RuntimeError("Unknown compiler '%s'." % compiler) # return []

def get_compiler_build_options(compiler=DEFAULT_COMPILER, is_executable_or_library=False):
    if compiler == COMPILER_GCC:
        return OPTIONS_GCC_EXECUTABLE if is_executable_or_library else OPTIONS_GCC_LIBRARY
    elif compiler == COMPILER_CLANG:
        return OPTIONS_CLANG_EXECUTABLE if is_executable_or_library else OPTIONS_CLANG_LIBRARY
    else:
        raise RuntimeError("Unknown compiler '%s'." % compiler) # return []


def get_output_compiler_options(root_path, file_name, cwd_cmd, compiler=DEFAULT_COMPILER):
    out_file = get_build_file_path(root_path, file_name)
    out_file = P.get_rel_path_or_abs(out_file, cwd_cmd)

    out_lst = []
    if compiler == COMPILER_GCC:
        out_lst = ['-o', out_file, ]
    elif compiler == COMPILER_CLANG:
        out_lst = ['-o', out_file, ]
    else:
        raise RuntimeError("Unknown compiler '%s'." % compiler)

    return out_lst


def get_app_dir(root_path):
    return os.path.join(root_path, SOURCE_WS_FOLDER, SOURCE_APP_FOLDER)

def get_app_src_dir(root_path):
    return os.path.join(get_app_dir(root_path), SOURCE_APP_SRC_FOLDER)

def get_app_header_dir(root_path):
    return os.path.join(get_app_dir(root_path), SOURCE_APP_HEADER_FOLDER)


def get_lib_dir(root_path):
    return os.path.join(root_path, SOURCE_WS_FOLDER, SOURCE_LIB_FOLDER)

def get_lib_src_dir(root_path):
    return os.path.join(get_lib_dir(root_path), SOURCE_LIB_SRC_FOLDER)

def get_lib_header_dir(root_path):
    return os.path.join(get_lib_dir(root_path), SOURCE_LIB_HEADER_FOLDER)


def get_build_dir(root_path):
    return os.path.join(root_path, SOURCE_WS_FOLDER, BUILD_BIN_FOLDER)

def get_build_file_path(root_path, file_name):
    return os.path.join(get_build_dir(root_path), file_name)


def get_source_path_list(cwd_cmd, src_dir):
    full_path = os.path.join(cwd_cmd, src_dir)
    file_lst = os.listdir(full_path)
    new_lst  = [ P.get_combed_path( os.path.join(src_dir, f) ) for f in file_lst
                    if SOURCE_IGNORE_FILE_PATH not in f
                        and os.path.isfile( os.path.join(full_path, f) )
                        and os.path.splitext(f)[ 1 ] == SOURCE_FILE_EXT ]
    return new_lst


def get_compile_app_cmd(root_path, compiler=DEFAULT_COMPILER, is_release_or_debug=False, adx_options_list=None):
    cmd_lst = [compiler, ]

    cmd_lst += get_compiler_optimization_options(compiler=compiler, is_release_or_debug=is_release_or_debug)

    cmd_lst += get_compiler_build_options(compiler=compiler, is_executable_or_library=True)

    if adx_options_list is not None:
        cmd_lst += adx_options_list

    cwd_app = get_app_src_dir(root_path)
    src_dir = P.get_rel_path_or_abs(get_app_src_dir(root_path), cwd_app) # get_app_src_dir(root_path)
    cmd_lst += get_source_path_list(cwd_app, src_dir)

    cmd_lst += get_output_compiler_options(root_path, EXECUTABLE_FILE_NAME, cwd_app, compiler=compiler)

    return cwd_app, cmd_lst

def get_compile_lib_cmd(root_path, compiler=DEFAULT_COMPILER, is_release_or_debug=False, adx_options_list=None):
    cmd_lst = [compiler, ]

    cmd_lst += get_compiler_optimization_options(compiler=compiler, is_release_or_debug=is_release_or_debug)

    cmd_lst += get_compiler_build_options(compiler=compiler, is_executable_or_library=False)

    if adx_options_list is not None:
        cmd_lst += adx_options_list

    cwd_lib = get_lib_src_dir(root_path)
    src_dir = P.get_rel_path_or_abs(get_lib_src_dir(root_path), cwd_lib) # get_lib_src_dir(root_path)
    cmd_lst += get_source_path_list(cwd_lib, src_dir)

    cmd_lst += get_output_compiler_options(root_path, LIBRARY_FILE_NAME, cwd_lib, compiler=compiler)

    return cwd_lib, cmd_lst


def get_ls_cmd():
    cmd_lst = ['ls', '-Fal', ]
    return cmd_lst

def get_run_exe_file_cmd(root_path, options_list=None):
    cmd_str = get_build_file_path(root_path, EXECUTABLE_FILE_NAME)

    cmd_lst = [cmd_str, ]

    if options_list is not None:
        cmd_lst += options_list

    return cmd_lst
