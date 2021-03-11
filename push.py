#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import sys
import os
import os.path
import time

import py.git_run as GR
import py.update_ver as UV


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

    is_dry_run = True if GR.any_item_in_list( ['-n', '--dry-run'], pre_git_argv) else False
    is_verbose = True if is_dry_run or GR.any_item_in_list( ['-v', '--verbose'], pre_git_argv) else False
    is_debug = True if is_dry_run or GR.any_item_in_list( ['-d', '--debug'], pre_git_argv) else False

    is_book = True if GR.any_item_in_list( ['-b', '--book'], pre_git_argv) else False

    is_major = True if GR.any_item_in_list( ['-M', '--major'], pre_git_argv) else False
    is_minor = True if not is_major and GR.any_item_in_list( ['-m', '--minor'], pre_git_argv) else False
    is_patch = True if not (is_major or is_minor) and GR.any_item_in_list( ['-p', '--patch'], pre_git_argv) else False

    if not (is_book or is_major or is_minor or is_patch):
        # raise RuntimeError("Specify at least one of --book, --major, --minor, --patch.")
        print( "Specify at least one of --book, --major, --minor, --patch to update version(s)." )

    if is_verbose:
        print( "" )
        print( "Project root folder: '%s'." % PROJECT_ROOT_PATH )

        now_version, now_short = UV.get_current_times()
        print( "New version: %s." % now_version )

        major, minor, patch, prerelease, build = UV.get_current_lib_versions( PROJECT_ROOT_PATH )
        if prerelease is None:
            print( "Library version: %s.%s.%s+%s." % (major, minor, patch, build) )
        else:
            print( "Library version: %s.%s.%s-%s+%s." % (major, minor, patch, prerelease, build) )

    if is_debug:
        print( "" )
        print( "Script args: %s." % str( pre_git_argv ) )
        print( "git commit args: %s." % str( git_commit_argv ) )
        print( "git push args: %s." % str( git_push_argv ) )

    if is_book or is_major or is_minor or is_patch:
        print( "" )

        if is_debug:
            print( "Updating versions of book: %s, major: %s, minor: %s, patch: %s." % (str(is_book), str(is_major), str(is_minor), str(is_patch)) )

        if not is_dry_run:
            UV.replace_all_entries( PROJECT_ROOT_PATH, is_book, is_major, is_minor, is_patch )

    if git_commit_argv:
        print( "" )

        if is_debug:
            print( "Running: %s" % str( git_commit_argv ) )

        if not is_dry_run:
            result = GR.run_process( git_commit_argv )

    if git_push_argv:
        print( "" )

        if is_debug:
            print( "Running: %s" % str( git_push_argv ) )

        if not is_dry_run:
            result = GR.run_process( git_push_argv )

    print( "" )


if __name__ == '__main__':
    main()
