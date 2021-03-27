#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import sys
import os
# import os.path

import py.paths as P
import py.run_subproc as RS
import py.run_compiler as RC
import py.build_env as BE


PROJECT_ROOT_PATH = P.get_project_root_path( sys.argv[ 0 ] )


def main():
    script_argv, compiler_argv, linker_argv, executable_argv = RC.split_cmd_compiler_args(sys.argv)

    is_dry_run = True if RS.any_item_in_list( ['-n', '--dry-run'], script_argv) else False
    is_verbose = True if is_dry_run or RS.any_item_in_list( ['-v', '--verbose'], script_argv) else False
    is_debug = True if is_dry_run or RS.any_item_in_list( ['-d', '--debug'], script_argv) else False

    is_debug_build = True if RS.any_item_in_list( ['-D', '--debug'], script_argv) else False
    is_release_build = True if not is_debug_build and RS.any_item_in_list( ['-R', '--release'], script_argv) else False

    # is_run_build_only = True if RS.any_item_in_list( ['-X', '--execute'], script_argv) else False
    is_run_build = True if RS.any_item_in_list( ['-r', '--run'], script_argv) else False

    if is_verbose:
        print( "" )
        print( "Project root folder: '%s'." % PROJECT_ROOT_PATH )

    if is_debug:
        print( "" )
        print( "Script args: %s." % str( script_argv ) )
        print( "compiler args: %s." % str( compiler_argv ) )
        print( "linker args: %s." % str( linker_argv ) )
        print( "executable args: %s." % str( executable_argv ) )

    if is_debug_build or is_release_build:
        cmd_cwd, compile_cmd_lst = BE.get_compile_app_cmd(PROJECT_ROOT_PATH, rel_path=None, is_release_or_debug=is_release_build, adx_options_list=compiler_argv)

    if is_run_build:
        ls_cmd_lst = BE.get_ls_exe_file_cmd(PROJECT_ROOT_PATH, is_release_or_debug=is_release_build)
        run_cmd_lst = BE.get_run_exe_file_cmd(PROJECT_ROOT_PATH, is_release_or_debug=is_release_build, options_list=compiler_argv)

    old_cwd = os.getcwd()

    if is_debug:
        print( "" )
        print( "Currently in: %s." % str( old_cwd ) )

        if is_debug_build or is_release_build:
            print( "Compiling in: %s." % str( cmd_cwd ) )
            print( "Compiling with: %s." % str( compile_cmd_lst ) )

    if is_debug_build or is_release_build:
        if not is_dry_run:
            os.chdir(cmd_cwd)
            result = RS.run_process( compile_cmd_lst )
            print( result )

        if is_debug:
            print( "" )
            print( "Returning back into: %s." % str( old_cwd ) )

        if not is_dry_run:
            os.chdir(old_cwd)

    if is_run_build:
        if is_debug:
            if not is_dry_run:
                print( "" )
                result = RS.run_process( ls_cmd_lst )
                print( result )

            print( "" )
            print( "Running: %s." % str( run_cmd_lst ) )

        if not is_dry_run:
            result = RS.run_process( run_cmd_lst )
            print( result )


if __name__ == '__main__':
    main()
