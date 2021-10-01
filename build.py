#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.


import sys
import os
# import os.path
import subprocess

import py.paths as P
import py.run_subproc as RS
import py.run_compiler as RC
import py.build_env as BE
import py.docs_env as DE


PROJECT_ROOT_PATH = P.get_project_root_path( sys.argv[ 0 ] )


def remove_build_files(root_path, all_files_or_obj_only=False):
    build_dir = BE.get_build_dir(root_path)

    if os.path.exists(build_dir):
        file_paths = [  P.get_abs_combed_path( os.path.join( build_dir, f ) )
                        for f in os.listdir(build_dir)
                        if all_files_or_obj_only
                            or RS.any_item_in( BE.OBJECT_FILE_EXTENSIONS, f ) ]

        for file_path in file_paths:
            os.remove(file_path)
    else:
        os.makedirs(build_dir)


def main():
    script_argv, compile_lib_argv, compile_app_argv, executable_argv = RC.split_cmd_compiler_args(sys.argv)

    is_dry_run = True if RS.any_item_in( ['-n', '--dry-run'], script_argv) else False
    is_verbose = True if is_dry_run or RS.any_item_in( ['-v', '--verbose'], script_argv) else False
    is_debug = True if is_dry_run or RS.any_item_in( ['-d', '--debug'], script_argv) else False

    is_debug_build = True if RS.any_item_in( ['-D', '--debug-build'], script_argv) else False
    is_release_build = True if not is_debug_build and RS.any_item_in( ['-R', '--release'], script_argv) else False
    is_build = is_release_build or is_debug_build
    is_release_or_debug = is_release_build and not is_debug_build

    is_extra_warnings = True if RS.any_item_in( ['-XW', '--extra-warnings'], script_argv) else False
    is_silence =  True if RS.any_item_in( ['-S', '--silence'], script_argv) else False

    is_gcc = True if RS.any_item_in( ['-gcc', '--gcc'], script_argv) else False
    is_clang = True if RS.any_item_in( ['-clang', '--clang'], script_argv) else False
    compiler =  BE.COMPILER_GCC if is_gcc else \
                BE.COMPILER_CLANG if is_clang else \
                BE.DEFAULT_COMPILER

    is_docs = True if RS.any_item_in( ['-docs', '--docs'], script_argv) else False
    compiler_docs = DE.COMPILER_DOCS

    # is_run_app_only = True if RS.any_item_in( ['-X', '--execute'], script_argv) else False
    is_run_app = True if RS.any_item_in( ['-r', '--run'], script_argv) else False
    is_run_tests = True if RS.any_item_in( ['-t', '--tests'], script_argv) else False

    if is_verbose:
        print( "" )
        print( "Project root folder: '%s'." % PROJECT_ROOT_PATH )

    if is_debug:
        print( "" )
        print( "Compiler: %s." % str( compiler ) )
        print( "Script args: %s." % str( script_argv ) )
        print( "Compile library args: %s." % str( compile_lib_argv ) )
        print( "Compile app args: %s." % str( compile_app_argv ) )
        print( "executable args: %s." % str( executable_argv ) )

    old_cwd = os.getcwd()
    docs_cwd = DE.get_docs_dir( PROJECT_ROOT_PATH )
    cmd_cwd = BE.get_build_dir( PROJECT_ROOT_PATH )

    if is_debug:
        print( "" )
        print( "Currently in: %s." % str( old_cwd ) )
        print( "Docs dir: %s." % str( docs_cwd ) )
        print( "Build dir: %s." % str( cmd_cwd ) )

    if is_docs:
        cwd_docs, cmd_docs = DE.get_compile_docs_cmd(PROJECT_ROOT_PATH, compiler_docs=compiler_docs)

        if is_debug:
            print( "Compiling docs in: %s." % str( cwd_docs ) )
            print( "Compiling docs with: %s." % str( cmd_docs ) )

        if not is_dry_run:
            print( "." * 72 )
            result = RS.run_process( cmd_docs, cwd=cwd_docs )
            print( result )
            print( "-" * 72 )

    if is_build:
        if not is_dry_run:
            remove_build_files(PROJECT_ROOT_PATH, all_files_or_obj_only=True)

        cwd_lib, compile_lib_cmd_lst = BE.get_compile_lib_cmd(PROJECT_ROOT_PATH,
                                                              compiler=compiler,
                                                              is_release_or_debug=is_release_or_debug,
                                                              is_extra_warnings=is_extra_warnings,
                                                              is_silence=is_silence,
                                                              adx_options_list=compile_lib_argv)

        if is_debug:
            print( "Compiling in: %s." % str( cwd_lib ) )
            print( "Compiling with: %s." % str( compile_lib_cmd_lst ) )

        if not is_dry_run:
            print( "." * 72 )
            # os.chdir(cwd_app)
            result = RS.run_process( compile_lib_cmd_lst, cwd=cwd_lib )
            print( result )
            print( "-" * 72 )

            remove_build_files(PROJECT_ROOT_PATH, all_files_or_obj_only=False)

        cwd_app, compile_app_cmd_lst = BE.get_compile_app_cmd(PROJECT_ROOT_PATH,
                                                              compiler=compiler,
                                                              is_release_or_debug=is_release_or_debug,
                                                              is_extra_warnings=is_extra_warnings,
                                                              is_silence=is_silence,
                                                              adx_options_list=compile_app_argv)

        if is_debug:
            print( "Compiling in: %s." % str( cwd_app ) )
            print( "Compiling with: %s." % str( compile_app_cmd_lst ) )

        if not is_dry_run:
            print( "." * 72 )
            result = RS.run_process( compile_app_cmd_lst, cwd=cwd_app )
            print( result )
            print( "-" * 72 )

            remove_build_files(PROJECT_ROOT_PATH, all_files_or_obj_only=False)

        cwd_tests, compile_tests_cmd_lst = BE.get_compile_tests_cmd(PROJECT_ROOT_PATH,
                                                                    compiler=compiler,
                                                                    is_release_or_debug=is_release_or_debug,
                                                                    is_extra_warnings=is_extra_warnings,
                                                                    is_silence=is_silence,
                                                                    adx_options_list=compile_app_argv)

        if is_debug:
            print( "Compiling in: %s." % str( cwd_tests ) )
            print( "Compiling with: %s." % str( compile_tests_cmd_lst ) )

        if not is_dry_run:
            print( "." * 72 )
            result = RS.run_process( compile_tests_cmd_lst, cwd=cwd_tests )
            print( result )
            print( "-" * 72 )

            remove_build_files(PROJECT_ROOT_PATH, all_files_or_obj_only=False)

    if is_build or is_run_app:
        ls_cmd_lst = BE.get_ls_cmd()

        if is_debug:
            print( "" )
            print( "Running: %s." % str( ls_cmd_lst ) )

        if not is_dry_run:
            # print( "" )
            print( "." * 72 )
            result = RS.run_process( ls_cmd_lst, cwd=cmd_cwd )
            print( result )
            print( "-" * 72 )

    if is_run_app or is_run_tests:
        exe_name = BE.EXECUTABLE_FILE_NAME if is_run_app else BE.TESTS_FILE_NAME
        run_cmd_lst = BE.get_run_exe_file_cmd(PROJECT_ROOT_PATH, exe_name=exe_name, options_list=executable_argv)

        if is_debug:
            print( "" )
            print( "Running: %s." % str( run_cmd_lst ) )

        if not is_dry_run:
            try:
                print( "." * 72 )
                result = subprocess.run( run_cmd_lst, cwd=cmd_cwd ) # RS.run_process() --> subprocess.check_output() --> buffered output --> not good!
                print( result )
                print( "-" * 72 )
            except FileNotFoundError:
                # FileNotFoundError: [Errno 2] No such file or directory: '/home/pero/src/crochess/ws/build/crochess'
                print( "Executable '%s' not found." % run_cmd_lst[ 0 ] )


if __name__ == '__main__':
    main()
