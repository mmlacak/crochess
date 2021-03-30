#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import sys

import py.paths as P
import py.run_subproc as RS
import py.run_git as RG
import py.update_ver as UV


PROJECT_ROOT_PATH = P.get_project_root_path( sys.argv[ 0 ] )


def main():
    pre_git_argv, git_commit_argv, git_push_argv = RG.split_cmd_git_args(sys.argv)

    is_dry_run = True if RS.any_item_in( ['-n', '--dry-run'], pre_git_argv) else False
    is_verbose = True if is_dry_run or RS.any_item_in( ['-v', '--verbose'], pre_git_argv) else False
    is_debug = True if is_dry_run or RS.any_item_in( ['-d', '--debug'], pre_git_argv) else False

    is_book = True if RS.any_item_in( ['-b', '--book'], pre_git_argv) else False

    is_major = True if RS.any_item_in( ['-M', '--major'], pre_git_argv) else False
    is_minor = True if not is_major and RS.any_item_in( ['-m', '--minor'], pre_git_argv) else False
    is_patch = True if not (is_major or is_minor) and RS.any_item_in( ['-p', '--patch'], pre_git_argv) else False

    auto_updated_files = []

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
            auto_updated_files = UV.replace_all_entries( PROJECT_ROOT_PATH, is_book, is_major, is_minor, is_patch )

    if git_commit_argv:
        print( "" )

        if not is_dry_run and auto_updated_files:
            print( "Auto-updated: %s." % str( auto_updated_files ) )
            print( "" )

            if not RG.is_committing_all_files(git_commit_argv):
                if RG.is_committing_specified_files(git_commit_argv):
                    git_commit_argv.extend( auto_updated_files )
                else:
                    git_add = ['git', 'add', '--', ]
                    git_add.extend( auto_updated_files )
                    print( "Running: %s" % str( git_add ) )
                    result = RS.run_process( git_add )
                    print( result )

        if is_debug:
            print( "Running: %s" % str( git_commit_argv ) )

        if not is_dry_run:
            result = RS.run_process( git_commit_argv )
            print( result )

    if git_push_argv:
        print( "" )

        if is_debug:
            print( "Running: %s" % str( git_push_argv ) )

        if not is_dry_run:
            result = RS.run_process( git_push_argv )
            print( result )

    print( "" )


if __name__ == '__main__':
    main()
