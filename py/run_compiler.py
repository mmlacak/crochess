#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


def split_cmd_compiler_args( argv ):
    arg_sep_count = 0 # 0 --> script argv, 1 --> compile lib argv, 2 --> compile app argv, 3 --> run app argv
    compiler_args = False
    linker_args = False
    executable_args = False

    script_argv = []
    cc_lib_argv = []
    cc_app_argv = []
    exec_app_argv = []

    for index, arg in enumerate( argv ):
        if arg != '-*-':
            if index > 0: # index == 0 --> arg == '.../crochess/push.py', i.e. executing script, not an argument
                a = arg

                if arg_sep_count == 0:
                    script_argv.append( a )
                elif arg_sep_count == 1:
                    cc_lib_argv.append( a )
                elif arg_sep_count == 2:
                    cc_app_argv.append( a )
                elif arg_sep_count == 3:
                    exec_app_argv.append( a )
                else:
                    raise RuntimeError("Too many arg groups, expected: script.py <args> [-*- [<compile lib args>] [-*- [<compile app args>] [-*- [<run app args>]]]],\nin cmd line: '%s'." % argv)
        else:
            arg_sep_count += 1

            if arg_sep_count == 1:
                compiler_args = True
            elif arg_sep_count == 2:
                linker_args = True
            elif arg_sep_count == 3:
                executable_args = True

    # if cc_lib_argv or compiler_args:
    #     # not empty argv, or explicitly to call with no args
    #     cc_lib_argv = ['git', 'commit'] + cc_lib_argv

    # if cc_app_argv or linker_args:
    #     # not empty argv, or explicitly to call with no args
    #     cc_app_argv = ['git', 'push'] + cc_app_argv

    return (script_argv, cc_lib_argv, cc_app_argv, exec_app_argv)
