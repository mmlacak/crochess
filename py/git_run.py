#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


# import sys
# import os
# import os.path
# import re
# import time
import subprocess


def run_process(cmd_args_list):
    output_str = None
    try:
        output_str = subprocess.check_output(cmd_args_list)
    except subprocess.CalledProcessError:
        pass
    return str(output_str, encoding='utf-8', errors='replace')

def split_cmd_git_args(argv):
    arg_sep_count = 0 # 0 --> script argv, 1 --> git commit argv, 2 --> git push argv

    pre_git_argv = []
    git_commit_argv = []
    git_push_argv = []

    for arg in argv:
        if arg != '--':
            if '.py' not in arg:
                a = arg
                if not arg.startswith('-'):
                    a = '"%s"' % (arg, )
                if arg_sep_count == 0:
                    pre_git_argv.append( a )
                elif arg_sep_count == 1:
                    git_commit_argv.append( a )
                elif arg_sep_count == 2:
                    git_push_argv.append( a )
                else:
                    raise RuntimeError("Too many arg groups, expected: script.py <args> -- <git commit args> -- <git push args>,\nin cmd line: '%s'." % argv)
        else:
            arg_sep_count += 1

    if git_commit_argv:
        # not empty
        git_commit_argv = ['git', 'commit'] + git_commit_argv

    if git_push_argv:
        # not empty
        git_push_argv = ['git', 'push'] + git_push_argv

    return (pre_git_argv, git_commit_argv, git_push_argv)

def any_item_in_list(items, lst):
    for item in items:
        if item in lst:
            return True
    return False
