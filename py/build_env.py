#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


import os.path
import enum as E

import py.paths as P


#
# $ export LD_LIBRARY_PATH=.:./lib:../lib:..:../../bin:ws/bin:~/src/crochess/ws/bin
#
# $ export PATH=${PATH}:~/src/crochess/ws/bin
#
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# gcc -Wall -pedantic -O3 -shared -fPIC -I../inc cc_version.c -o ../../bin/libcrochess.so # in ./ws/libcrochess/src
#
# clang -Wall -pedantic -O3 -shared -fPIC -I../inc cc_version.c -o ../../bin/libcrochess.so # in ./ws/libcrochess/src
#
#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# gcc -O3 -Wall --pedantic -L../../bin -lcrochess -I../../libcrochess/inc -I../inc crochess.c -o ../../bin/crochess # in ./ws/crochess/src
#
# clang -O3 -Wall --pedantic -L../../bin -lcrochess -I../../libcrochess/inc -I../inc crochess.c -o ../../bin/crochess # in ./ws/crochess/src
#


class Target( E.Enum ):
    NONE = 0
    LIB_CROCHESS = 1
    # LIB_LINENOISE = 2
    EXE_CROCHESS = 3
    EXE_TESTS = 4


COMPILER_GCC = 'gcc'
COMPILER_CLANG = 'clang'

DEFAULT_COMPILER = COMPILER_GCC

EXECUTABLE_FILE_NAME = 'crochess'
LIBRARY_FILE_NAME = 'libcrochess.so'
TESTS_FILE_NAME = 'tests'
OBJECT_FILE_EXTENSIONS = ['.o', '.obj', ]
BUILD_BIN_FOLDER = 'bin'


OPTIONS_GCC_DEBUG = [ '-Wall', '-pedantic', '-O0', '-ggdb', '-Wfatal-errors', ] # TEMP :: -Wfatal-errors (?) # -fmax-errors=N (?)
OPTIONS_GCC_RELEASE = [ '-Wall', '-pedantic', '-O3', '-Wfatal-errors', ] # TEMP :: -Wfatal-errors (?) # -fmax-errors=N (?)
OPTIONS_GCC_EXTRA_WARNINGS = [ '-Wextra', ] # '-fdiagnostics-show-option'
OPTIONS_GCC_SILENCE = [ '-Wno-return-type', '-Wno-comment', '-Wno-type-limits', ]
OPTIONS_GCC_DEBUG_CONSTS = [ '-D__CC_DEBUG__', ]
OPTIONS_GCC_RELEASE_CONSTS = [] # [ '-D__CC_DEBUG__', ]

OPTIONS_CLANG_DEBUG = ['-Wall', '-pedantic', '-O0', '-ggdb', '-Wfatal-errors', ] # TEMP :: -Wfatal-errors (?) # -fmax-errors=N (?)
OPTIONS_CLANG_RELEASE = ['-Wall', '-pedantic', '-O3', '-Wfatal-errors', ] # TEMP :: -Wfatal-errors (?) # -fmax-errors=N (?)
OPTIONS_CLANG_EXTRA_WARNINGS = [ '-Wextra', ] # '-fdiagnostics-show-option'
OPTIONS_CLANG_SILENCE = [ '-Wno-format-security', '-Wno-gnu-zero-variadic-macro-arguments', ]
OPTIONS_CLANG_DEBUG_CONSTS = [ '-D__CC_DEBUG__', ]
OPTIONS_CLANG_RELEASE_CONSTS = [] # [ '-D__CC_DEBUG__', ]


OPTIONS_GCC_LIBRARY = [ '--shared', '-fPIC', '-I../inc', ]
OPTIONS_GCC_LIBRARY_DEPENDENCIES = [ '-lm', ]
OPTIONS_GCC_LIBRARY_LINENOISE = [ '--shared', '-fPIC', '-I./', ]
OPTIONS_GCC_LIBRARY_LINENOISE_DEPENDENCIES = [ ]
OPTIONS_GCC_EXECUTABLE = [ '-I../../libcrochess/inc', '-I../inc', ]
OPTIONS_GCC_EXECUTABLE_DEPENDENCIES = [ '-L../../bin', '-lcrochess', ]

OPTIONS_CLANG_LIBRARY = ['--shared', '-fPIC', '-I../inc', ]
OPTIONS_CLANG_LIBRARY_DEPENDENCIES = [ '-lm', ]
OPTIONS_CLANG_LIBRARY_LINENOISE = ['--shared', '-fPIC', '-I./', ]
OPTIONS_CLANG_LIBRARY_LINENOISE_DEPENDENCIES = [ ]
OPTIONS_CLANG_EXECUTABLE = ['-I../../libcrochess/inc', '-I../inc', ]
OPTIONS_CLANG_EXECUTABLE_DEPENDENCIES = ['-L../../bin', '-lcrochess', ]


DOCS_FOLDER = 'docs'
DOCS_SOURCE_FOLDER = 'docs/source'

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

SOURCE_TESTS_FOLDER = 'tests'
SOURCE_TESTS_SRC_FOLDER = 'src'
SOURCE_TESTS_HEADER_FOLDER = 'inc'


def get_compiler_optimization_options( compiler=DEFAULT_COMPILER, is_release_or_debug=False, is_extra_warnings=False, is_silence=False, is_consts=False ):
    options = None

    if compiler == COMPILER_GCC:
        options = OPTIONS_GCC_RELEASE[ : ] if is_release_or_debug else OPTIONS_GCC_DEBUG[ : ]

        if is_extra_warnings:
            options += OPTIONS_GCC_EXTRA_WARNINGS

        if is_silence:
            options += OPTIONS_GCC_SILENCE

        if is_consts:
            options += OPTIONS_GCC_RELEASE_CONSTS if is_release_or_debug else OPTIONS_GCC_DEBUG_CONSTS
    elif compiler == COMPILER_CLANG:
        options = OPTIONS_CLANG_RELEASE[ : ] if is_release_or_debug else OPTIONS_CLANG_DEBUG[ : ]

        if is_extra_warnings:
            options += OPTIONS_CLANG_EXTRA_WARNINGS

        if is_silence:
            options += OPTIONS_CLANG_SILENCE

        if is_consts:
            options += OPTIONS_CLANG_RELEASE_CONSTS if is_release_or_debug else OPTIONS_CLANG_DEBUG_CONSTS
    else:
        raise RuntimeError("Unknown compiler '%s'." % compiler)

    return options

def get_compiler_build_options( compiler=DEFAULT_COMPILER, target=Target.LIB_CROCHESS ):
    if compiler == COMPILER_GCC:
        if target in [ Target.EXE_CROCHESS, Target.EXE_TESTS ]:
            return OPTIONS_GCC_EXECUTABLE
        elif target == Target.LIB_CROCHESS:
            return OPTIONS_GCC_LIBRARY
        # elif target == Target.LIB_LINENOISE:
        #     return OPTIONS_GCC_LIBRARY_LINENOISE
        else:
            raise RuntimeError("Unknown target '%s'." % str(target))
    elif compiler == COMPILER_CLANG:
        if target in [ Target.EXE_CROCHESS, Target.EXE_TESTS ]:
            return OPTIONS_CLANG_EXECUTABLE
        elif target == Target.LIB_CROCHESS:
            return OPTIONS_CLANG_LIBRARY
        # elif target == Target.LIB_LINENOISE:
        #     return OPTIONS_CLANG_LIBRARY_LINENOISE
        else:
            raise RuntimeError("Unknown target '%s'." % str(target))
    else:
        raise RuntimeError("Unknown compiler '%s'." % compiler)

def get_compiler_build_dependencies( compiler=DEFAULT_COMPILER, target=Target.LIB_CROCHESS ):
    if compiler == COMPILER_GCC:
        if target in [ Target.EXE_CROCHESS, Target.EXE_TESTS ]:
            return OPTIONS_GCC_EXECUTABLE_DEPENDENCIES
        elif target == Target.LIB_CROCHESS:
            return OPTIONS_GCC_LIBRARY_DEPENDENCIES
        # elif target == Target.LIB_LINENOISE:
        #     return OPTIONS_GCC_LIBRARY_LINENOISE_DEPENDENCIES
        else:
            raise RuntimeError("Unknown target '%s'." % str(target))
    elif compiler == COMPILER_CLANG:
        if target in [ Target.EXE_CROCHESS, Target.EXE_TESTS ]:
            return OPTIONS_CLANG_EXECUTABLE_DEPENDENCIES
        elif target == Target.LIB_CROCHESS:
            return OPTIONS_CLANG_LIBRARY_DEPENDENCIES
        # elif target == Target.LIB_LINENOISE:
        #     return OPTIONS_CLANG_LIBRARY_LINENOISE_DEPENDENCIES
        else:
            raise RuntimeError("Unknown target '%s'." % str(target))
    else:
        raise RuntimeError("Unknown compiler '%s'." % compiler)


def get_output_compiler_options( root_path, file_name, cwd_cmd, compiler=DEFAULT_COMPILER ):
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


def get_app_dir( root_path ):
    return os.path.join(root_path, SOURCE_WS_FOLDER, SOURCE_APP_FOLDER)

def get_app_src_dir( root_path ):
    return os.path.join(get_app_dir(root_path), SOURCE_APP_SRC_FOLDER)

def get_app_header_dir( root_path ):
    return os.path.join(get_app_dir(root_path), SOURCE_APP_HEADER_FOLDER)


def get_lib_dir( root_path ):
    return os.path.join(root_path, SOURCE_WS_FOLDER, SOURCE_LIB_FOLDER)

def get_lib_src_dir( root_path ):
    return os.path.join(get_lib_dir(root_path), SOURCE_LIB_SRC_FOLDER)

def get_lib_header_dir( root_path ):
    return os.path.join(get_lib_dir(root_path), SOURCE_LIB_HEADER_FOLDER)


# def get_lib_ln_dir( root_path ):
#     return os.path.join(root_path, SOURCE_WS_FOLDER, SOURCE_LIB_LN_FOLDER)

# def get_lib_ln_src_dir( root_path ):
#     return os.path.join(get_lib_ln_dir(root_path), SOURCE_LIB_LN_SRC_FOLDER)

# def get_lib_ln_header_dir( root_path ):
#     return os.path.join(get_lib_ln_dir(root_path), SOURCE_LIB_LN_HEADER_FOLDER)


def get_tests_dir( root_path ):
    return os.path.join(root_path, SOURCE_WS_FOLDER, SOURCE_TESTS_FOLDER)

def get_tests_src_dir( root_path ):
    return os.path.join(get_tests_dir(root_path), SOURCE_TESTS_SRC_FOLDER)

def get_tests_header_dir( root_path ):
    return os.path.join(get_tests_dir(root_path), SOURCE_TESTS_HEADER_FOLDER)


def get_build_dir( root_path ):
    return os.path.join(root_path, SOURCE_WS_FOLDER, BUILD_BIN_FOLDER)

def get_build_file_path( root_path, file_name ):
    return os.path.join(get_build_dir(root_path), file_name)


def get_source_path_list( cwd_cmd, src_dir, target=Target.LIB_CROCHESS ):
    full_path = os.path.join(cwd_cmd, src_dir)
    file_lst = os.listdir(full_path)

    # if target == Target.LIB_LINENOISE:
    #     new_lst  = [ P.get_combed_path( os.path.join(src_dir, f) ) for f in file_lst
    #                     if SOURCE_IGNORE_FILE_PATH not in f
    #                         and os.path.isfile( os.path.join(full_path, f) )
    #                         and os.path.splitext( os.path.basename(f) )[ 0 ] in SOURCE_LIB_LN_SRC_FILE_NAMES
    #                         and os.path.splitext(f)[ 1 ] == SOURCE_FILE_EXT ]
    # else:
    new_lst  = [ P.get_combed_path( os.path.join(src_dir, f) ) for f in file_lst
                    if SOURCE_IGNORE_FILE_PATH not in f
                        and os.path.isfile( os.path.join(full_path, f) )
                        and os.path.splitext(f)[ 1 ] == SOURCE_FILE_EXT ]

    return new_lst


def get_compile_app_cmd( root_path, compiler=DEFAULT_COMPILER, is_release_or_debug=False, is_extra_warnings=False, is_silence=False, is_consts=False, adx_options_list=None ):
    cmd_lst = [compiler, ]

    cmd_lst += get_compiler_optimization_options(compiler=compiler, is_release_or_debug=is_release_or_debug, is_extra_warnings=is_extra_warnings, is_silence=is_silence, is_consts=is_consts)

    cmd_lst += get_compiler_build_options(compiler=compiler, target=Target.EXE_CROCHESS)

    if adx_options_list is not None:
        cmd_lst += adx_options_list

    cwd_app = get_app_src_dir(root_path)
    src_dir = P.get_rel_path_or_abs(get_app_src_dir(root_path), cwd_app) # get_app_src_dir(root_path)
    cmd_lst += get_source_path_list(cwd_app, src_dir, target=Target.EXE_CROCHESS)

    cmd_lst += get_compiler_build_dependencies(compiler=compiler, target=Target.EXE_CROCHESS)

    cmd_lst += get_output_compiler_options(root_path, EXECUTABLE_FILE_NAME, cwd_app, compiler=compiler)

    return cwd_app, cmd_lst

def get_compile_lib_cmd( root_path, compiler=DEFAULT_COMPILER, is_release_or_debug=False, is_extra_warnings=False, is_silence=False, is_consts=False, adx_options_list=None ):
    cmd_lst = [compiler, ]

    cmd_lst += get_compiler_optimization_options(compiler=compiler, is_release_or_debug=is_release_or_debug, is_extra_warnings=is_extra_warnings, is_silence=is_silence, is_consts=is_consts)

    cmd_lst += get_compiler_build_options(compiler=compiler, target=Target.LIB_CROCHESS)

    if adx_options_list is not None:
        cmd_lst += adx_options_list

    cwd_lib = get_lib_src_dir(root_path)
    src_dir = P.get_rel_path_or_abs(get_lib_src_dir(root_path), cwd_lib) # get_lib_src_dir(root_path)
    cmd_lst += get_source_path_list(cwd_lib, src_dir, target=Target.LIB_CROCHESS)

    cmd_lst += get_compiler_build_dependencies(compiler=compiler, target=Target.LIB_CROCHESS)

    cmd_lst += get_output_compiler_options(root_path, LIBRARY_FILE_NAME, cwd_lib, compiler=compiler)

    return cwd_lib, cmd_lst

def get_compile_tests_cmd( root_path, compiler=DEFAULT_COMPILER, is_release_or_debug=False, is_extra_warnings=False, is_silence=False, is_consts=False, adx_options_list=None ):
    cmd_lst = [compiler, ]

    cmd_lst += get_compiler_optimization_options(compiler=compiler, is_release_or_debug=is_release_or_debug, is_extra_warnings=is_extra_warnings, is_silence=is_silence, is_consts=is_consts)

    cmd_lst += get_compiler_build_options(compiler=compiler, target=Target.EXE_TESTS)

    if adx_options_list is not None:
        cmd_lst += adx_options_list

    cwd_tests = get_tests_src_dir(root_path)
    src_dir = P.get_rel_path_or_abs(get_tests_src_dir(root_path), cwd_tests) # get_app_src_dir(root_path)
    cmd_lst += get_source_path_list(cwd_tests, src_dir, target=Target.EXE_TESTS)

    cmd_lst += get_compiler_build_dependencies(compiler=compiler, target=Target.EXE_TESTS)

    cmd_lst += get_output_compiler_options(root_path, TESTS_FILE_NAME, cwd_tests, compiler=compiler)

    return cwd_tests, cmd_lst


def get_ctags_cmd():
    # cmd_lst = ['pwd', ]
    # cmd_lst = ['ctags', '-R', 'ws/*', ] # ctags: Warning: cannot open input file "ws/*" : No such file or directory
    cmd_lst = ['ctags', '-R', 'ws', ]
    # cmd_lst = ['./ctags.sh', ]
    return cmd_lst

def get_ls_cmd():
    cmd_lst = ['ls', '-Fal', ]
    return cmd_lst

def get_run_exe_file_cmd( root_path, exe_name=EXECUTABLE_FILE_NAME, options_list=None ):
    # cmd_str = get_build_file_path(root_path, exe_name)
    # cmd_lst = [ cmd_str, ]

    # Not running with full path anymore, to simulate run from console, and use the same history file.
    cmd_lst = [ exe_name, ]

    if options_list is not None:
        cmd_lst += options_list

    return cmd_lst
