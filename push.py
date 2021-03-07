#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import sys
import os
import os.path

import py.git_run as GR
import py.book_ver as BV


def get_combed_path(path):
    path = os.path.abspath( path )
    path = os.path.normpath( path )
    path = os.path.normcase( path )

    return path

def get_project_root_path():
    path = os.path.join(os.getcwd(), sys.argv[ 0 ])
    path = os.path.dirname( path )
    path = get_combed_path( path )

    return path


PROJECT_ROOT_PATH = get_project_root_path()


def main():
    pre_git_argv, git_commit_argv, git_push_argv = GR.split_cmd_git_args(sys.argv)

    verbose = True if GR.any_item_in_list( ['-v', '--verbose'], pre_git_argv) else False

    if verbose:
        print( "Project root folder: '%s'." % PROJECT_ROOT_PATH )

        now_version, now_short = BV.get_current_times()
        print( "New version: %s" % now_version )

        print( "Script args: %s" % str( pre_git_argv ) )
        print( "git commit args: %s" % str( git_commit_argv ) )
        print( "git push args: %s" % str( git_push_argv ) )


    BV.replace_all_entries( PROJECT_ROOT_PATH )





# import sys
# import os
# import os.path
# import re
# import time
# import subprocess


# print( "" )
# print( sys.argv )
# print( "" )
# for arg in sys.argv:
#     print( arg )
# print( "" )


if __name__ == '__main__':
    main()
