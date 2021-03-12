#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import subprocess


def run_process(cmd_args_list):
    output_str = ""

    try:
        output_str = subprocess.check_output(cmd_args_list)
    except subprocess.CalledProcessError:
        pass

    return str(output_str, encoding='utf-8', errors='replace')

def split_cmd_git_args(argv):
    arg_sep_count = 0 # 0 --> script argv, 1 --> git commit argv, 2 --> git push argv
    git_commit_args = False
    git_push_args = False

    pre_git_argv = []
    git_commit_argv = []
    git_push_argv = []

    for index, arg in enumerate(argv):
        if arg != '-*-':
            if index > 0: # index 0 --> arg == '.../crochess/push.py'
                a = arg

                if arg_sep_count == 0:
                    pre_git_argv.append( a )
                elif arg_sep_count == 1:
                    git_commit_argv.append( a )
                elif arg_sep_count == 2:
                    git_push_argv.append( a )
                else:
                    raise RuntimeError("Too many arg groups, expected: script.py <args> -*- <git commit args> -*- <git push args>,\nin cmd line: '%s'." % argv)
        else:
            arg_sep_count += 1

            if arg_sep_count == 1:
                git_commit_args = True
            elif arg_sep_count == 2:
                git_push_args = True

    if git_commit_argv or git_commit_args:
        # not empty argv, or explicitly to call with no args
        git_commit_argv = ['git', 'commit'] + git_commit_argv

    if git_push_argv or git_push_args:
        # not empty argv, or explicitly to call with no args
        git_push_argv = ['git', 'push'] + git_push_argv

    return (pre_git_argv, git_commit_argv, git_push_argv)

def any_item_in_list(items, lst):
    return any( [ (i in lst) for i in items ] )
