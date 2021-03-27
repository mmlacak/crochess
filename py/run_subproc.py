#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import subprocess


def any_item_in_list(items, lst):
    return any( [ (i in lst) for i in items ] )

def run_process(cmd_args_list):
    output_str = ""

    try:
        output_str = subprocess.check_output(cmd_args_list)
    except subprocess.CalledProcessError:
        pass

    return str(output_str, encoding='utf-8', errors='replace')
