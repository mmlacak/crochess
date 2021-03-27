#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


def split_cmd_compiler_args(argv):
    arg_sep_count = 0 # 0 --> script argv, 1 --> compiler argv, 2 --> linker argv, 3 --> run executable
    compiler_args = False
    linker_args = False
    executable_args = False

    script_argv = []
    compiler_argv = []
    linker_argv = []
    executable_argv = []

    for index, arg in enumerate(argv):
        if arg != '-*-':
            if index > 0: # index == 0 --> arg == '.../crochess/push.py', i.e. executing script, not an argument
                a = arg

                if arg_sep_count == 0:
                    script_argv.append( a )
                elif arg_sep_count == 1:
                    compiler_argv.append( a )
                elif arg_sep_count == 2:
                    linker_argv.append( a )
                elif arg_sep_count == 3:
                    executable_argv.append( a )
                else:
                    raise RuntimeError("Too many arg groups, expected: script.py <args> [-*- [<compiler args>] [-*- [<linker args>] [-*- [<executable args>]]]],\nin cmd line: '%s'." % argv)
        else:
            arg_sep_count += 1

            if arg_sep_count == 1:
                compiler_args = True
            elif arg_sep_count == 2:
                linker_args = True
            elif arg_sep_count == 3:
                executable_args = True

    # if compiler_argv or compiler_args:
    #     # not empty argv, or explicitly to call with no args
    #     compiler_argv = ['git', 'commit'] + compiler_argv

    # if linker_argv or linker_args:
    #     # not empty argv, or explicitly to call with no args
    #     linker_argv = ['git', 'push'] + linker_argv

    return (script_argv, compiler_argv, linker_argv, executable_args)
