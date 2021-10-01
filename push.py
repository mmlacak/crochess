#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSE, COPYING files for details.


import sys

import py.paths as P
import py.run_subproc as RS
import py.run_git as RG
import py.update_ver as UV


PROJECT_ROOT_PATH = P.get_project_root_path( sys.argv[ 0 ] )


def main():
    pre_git_argv, git_commit_argv, git_push_argv = RG.split_cmd_git_args(sys.argv)

    is_dry_run = True if RS.any_item_in( ['-n', '--dry-run'], pre_git_argv) else False
    is_wet_run = True if not is_dry_run and RS.any_item_in( ['-w', '--wet-run'], pre_git_argv) else False
    is_verbose = True if is_dry_run or is_wet_run or RS.any_item_in( ['-v', '--verbose'], pre_git_argv) else False
    is_debug = True if is_dry_run or is_wet_run or RS.any_item_in( ['-d', '--debug'], pre_git_argv) else False

    is_book = True if RS.any_item_in( ['-b', '--book'], pre_git_argv) else False
    is_docs = True if RS.any_item_in( ['-D', '--docs'], pre_git_argv) else False

    is_major = True if RS.any_item_in( ['-M', '--major'], pre_git_argv) else False
    is_minor = True if not is_major and RS.any_item_in( ['-m', '--minor'], pre_git_argv) else False
    is_feature = True if not (is_major or is_minor) and RS.any_item_in( ['-f', '--feature'], pre_git_argv) else False
    is_commit = True if not (is_major or is_minor or is_feature) and RS.any_item_in( ['-c', '--commit'], pre_git_argv) else False

    try:
        count = int( RS.capture_option( ['-C=', '--count='], pre_git_argv ) )
    except TypeError:
        # TypeError: int() argument must be a string, a bytes-like object or a number, not 'NoneType'
        count = None
    except ValueError:
        # Probably: ValueError: invalid literal for int() with base 10: 'abc'.
        count = None

    breaks = RS.capture_option( ['-B=', '--breaks='], pre_git_argv )

    auto_updated_files = []

    if not (is_book or is_docs or is_major or is_minor or is_feature or is_commit):
        # raise RuntimeError("Specify at least one of --book, --major, --minor, --feature or --commit.")
        print( "Specify at least one of --book, --docs, --major, --minor, --feature or --commit to update version(s)." )

    if is_verbose:
        print( "" )
        print( "Project root folder: '%s'." % PROJECT_ROOT_PATH )

        lib_ver = UV.get_current_lib_versions( PROJECT_ROOT_PATH, decompose_version=False )
        print( "Library version: %s." % str(lib_ver) )

        now_version, now_short = UV.get_current_times()
        print( "New version meta: %s." % now_version )

    if is_debug:
        print( "" )
        print( "Script args: %s." % str( pre_git_argv ) )
        print( "git commit args: %s." % str( git_commit_argv ) )
        print( "git push args: %s." % str( git_push_argv ) )

    if is_book or is_docs or is_major or is_minor or is_feature or is_commit:
        print( "" )

        if is_debug:
            print( "Updating versions of book: %s, docs: %s, major: %s, minor: %s, feature: %s, commit: %s." % (str(is_book), str(is_docs), str(is_major), str(is_minor), str(is_feature), str(is_commit)) )

        if not is_dry_run:
            auto_updated_files = UV.replace_all_entries( PROJECT_ROOT_PATH, is_book, is_docs, is_major, is_minor, is_feature, is_commit, count, breaks )

    if git_commit_argv:
        print( "" )

        if not is_dry_run:
            print( "Auto-updated: %s." % str( auto_updated_files ) )
            print( "" )

            if not is_wet_run and auto_updated_files:
                if not RG.is_committing_all_files(git_commit_argv):
                    if RG.is_committing_specified_files(git_commit_argv):
                        git_commit_argv.extend( auto_updated_files )
                    else:
                        git_add = ['git', 'add', '--', ]
                        git_add.extend( auto_updated_files )
                        print( "Running: %s" % str( git_add ) )
                        print( "." * 72 )
                        result = RS.run_process( git_add )
                        print( result )
                        print( "-" * 72 )

        if is_debug:
            print( "Running: %s" % str( git_commit_argv ) )

        if not is_dry_run and not is_wet_run:
            print( "." * 72 )
            result = RS.run_process( git_commit_argv )
            print( result )
            print( "-" * 72 )

    if git_push_argv:
        print( "" )

        if is_debug:
            print( "Running: %s" % str( git_push_argv ) )

        if not is_dry_run and not is_wet_run:
            print( "." * 72 )
            result = RS.run_process( git_push_argv )
            print( result )
            print( "-" * 72 )

    print( "" )


if __name__ == '__main__':
    main()
