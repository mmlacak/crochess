#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


import subprocess


def any_item_in(items, lst_or_str):
    return any( [ (i in lst_or_str) for i in items ] )

def one_or_none(lst):
    if len(lst) == 0:
        return None
    if len(lst) == 1:
        return lst[ 0 ]
    else:
        raise RuntimeError( "List too long, expected single element at most, got '%s'." % str(lst) )

def capture_option(starts_with, str_lst):
    return one_or_none( [ i[ len(sw) : ] for sw in starts_with for i in str_lst if i.startswith(sw) ] )

def run_process(cmd_args_list, cwd=None):
    output_str = ""

    try:
        # if cwd is not None:
        #     output_str = subprocess.check_output(cmd_args_list, cwd=cwd)
        # else:
        #     output_str = subprocess.check_output(cmd_args_list)
        output_str = subprocess.check_output(cmd_args_list, cwd=cwd)
    except subprocess.CalledProcessError:
        pass

    try:
        return str(output_str, encoding='utf-8', errors='replace')
    except TypeError:
        # TypeError: decoding str is not supported
        return output_str
