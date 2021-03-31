#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.

import os
import os.path
import time
import re

import py.build_env as BE


BOOK_TEX_FOLDER = 'book'
BOOK_TEX_FILE_NAME = 'crochess.tex'
BOOK_IGNORE_TEX_FILE_NAME = 'crochess.IGNORE.tex'

README_FILE_NAME = 'README.md'
README_IGNORE_FILE_NAME = 'README.IGNORE.md'


# SOURCE_WS_FOLDER = 'ws'
# SOURCE_SRC_FOLDER = 'src'

# SOURCE_APP_FOLDER = 'crochess'
# SOURCE_APP_CONFIG_FILE = 'Cargo.toml'
# SOURCE_IGNORE_APP_CONFIG_FILE = 'Cargo.IGNORE.toml'
# SOURCE_APP_MAIN_FILE = 'main.rs'
# SOURCE_IGNORE_APP_MAIN_FILE = 'main.IGNORE.rs'

# SOURCE_LIB_FOLDER = 'libcrochess'
# SOURCE_LIB_CONFIG_FILE = 'Cargo.toml'
# SOURCE_IGNORE_LIB_CONFIG_FILE = 'Cargo.IGNORE.toml'
# SOURCE_LIB_MAIN_FILE = 'lib.rs'
# SOURCE_IGNORE_LIB_MAIN_FILE = 'lib.IGNORE.rs'

# TODO :: import everything else from py.build_env

SOURCE_APP_MAIN_FILE = 'crochess.d'
SOURCE_APP_MAIN_IGNORE_FILE = 'crochess.IGNORE.d'

SOURCE_LIB_MAIN_FILE = 'libcrochess.d'
SOURCE_LIB_MAIN_IGNORE_FILE = 'libcrochess.IGNORE.d'

#
# \"(?P<version>.*)\"
REG_EXP_COMPLETE_VERSION_STRING = re.compile( r'''\"(?P<version>.*)\"''' ) # "\"(?P<version>.*)\"" ) # r"""\"(?P<version>.*)\"""" )


#
# https://regex101.com/r/Ly7O1x/3/
# "^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$"gm

# REG_EXP_VERSION_DECONSTRUCTED = re.compile( r"""^version = \"(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?\"""" )

# Possible change, extended versioning + relaxed syntax + breakage info.
# Cargo expects SemVer 2.0 in TOML though, so would need to work around it, or ignore version info there.
#
# ^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<feature>0|[1-9]\d*)\.(?P<commit>0|[1-9]\d*)(?:-(?P<prerelease>.*?))?(?:\+(?P<meta>.*?))?(?:\~(?P<breaks>.*?))?$ # non-greedy, match-all
#
# ^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<feature>0|[1-9]\d*)\.(?P<commit>0|[1-9]\d*)(?:-(?P<prerelease>[^-~\+\s]*?))?(?:\+(?P<meta>[^-~\+\s]*?))?(?:\~(?P<breaks>[^-~\+\s]*?))?$ # non-greedy, dont-match-separators
#
# ^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<feature>0|[1-9]\d*)\.(?P<commit>0|[1-9]\d*)(?:-(?P<prerelease>[^-~\+\s]*))?(?:\+(?P<meta>[^-~\+\s]*))?(?:\~(?P<breaks>[^-~\+\s]*))?$ # greedy, dont-match-separators
#
# ^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<feature>0|[1-9]\d*)\.(?P<commit>0|[1-9]\d*)(?:-(?P<prerelease>[^~\+\s]*))?(?:\+(?P<meta>[^~\s]*))?(?:\~(?P<breaks>[^\s]*))?$ # greedy, dont-match-separators-of-following-groups
#
# ^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<feature>0|[1-9]\d*)\.(?P<commit>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<meta>[^~\s]*))?(?:\~(?P<breaks>[^\s]*))?$ # <-- this

REG_EXP_VERSION_DECONSTRUCTED = re.compile( r"""^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<feature>0|[1-9]\d*)\.(?P<commit>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<meta>[^~\s]*))?(?:\~(?P<breaks>[^\s]*))?$""" )


def get_current_times():
    now = time.gmtime()
    # now_long = time.strftime('%Y-%m-%d %H:%M:%S UTC', now) # e.g. '2020-05-17 02:11:11 UTC'
    book_version = time.strftime('%Y%m%d.%H%M%S', now) # e.g. '20200517.021111'
    book_short = time.strftime('%Y-%m-%d', now) # e.g. '2020-05-17'
    return (book_version, book_short)

def get_current_lib_versions(root_path, decompose_version=True):
    path = get_lib_source_file_path(root_path, SOURCE_LIB_MAIN_FILE)

    try:
        with open(path, 'r') as old:
            for line in old:
                if 'source-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
                    mo = REG_EXP_COMPLETE_VERSION_STRING.search(line)
                    if mo is not None:
                        version = mo.group('version')

                        if not decompose_version:
                            return version

                        mo = REG_EXP_VERSION_DECONSTRUCTED.match(version)
                        if mo is not None:
                            major, minor, feature, commit, prerelease, meta, breaks = mo.groups()
                            return (int(major), int(minor), int(feature), int(commit), \
                                    str(prerelease) if prerelease is not None else None, \
                                    str(meta) if meta is not None else None, \
                                    str(breaks) if breaks is not None else None)
                    else:
                        return (None, None, None, None, None, None, None) if decompose_version else None
    except FileNotFoundError:
        # FileNotFoundError: [Errno 2] No such file or directory: '.../crochess/ws/libcrochess/Cargo.toml'
        return (None, None, None, None, None, None, None) if decompose_version else None

    return (None, None, None, None, None, None, None) if decompose_version else None

def get_full_tex_path(root_path, tex_dir=BOOK_TEX_FOLDER, tex_name=BOOK_TEX_FILE_NAME):
    path = os.path.join(root_path, tex_dir, tex_name)
    return path

def get_full_readme_path(root_path, readme_name=README_FILE_NAME):
    path = os.path.join(root_path, readme_name)
    return path

def get_app_source_file_path(root_path, file_name):
    path = os.path.join(BE.get_app_src_dir(root_path), file_name)
    return path

def get_lib_source_file_path(root_path, file_name):
    path = os.path.join(BE.get_lib_src_dir(root_path), file_name)
    return path

def change_book_line_if_marked(line, git_version, book_version, book_short, is_book, is_source):
    new = line

    if is_book:
        if 'book-new-commit-version-squished-utc-date-time-place-marker' in line:
            new = '    Version: %s \\\\ [2.0em] %% book-new-commit-version-squished-utc-date-time-place-marker\n' % (book_version, )
        elif 'book-new-commit-version-date-place-marker' in line:
            new = '    %s \\\\ %% book-new-commit-version-date-place-marker\n' % book_short
        elif 'book-new-commit-version-date-small-place-marker' in line:
            new = '    \small{%s} \\\\ [0.5em] %% book-new-commit-version-date-small-place-marker\n' % book_short

    return new

def change_readme_line_if_marked(line, git_version, book_version, book_short, is_book, is_source):
    new = line

    if is_book:
        if 'readme-new-book-version-squished-utc-date-time-place-marker' in line:
            new = 'Version: %s <!--- readme-new-book-version-squished-utc-date-time-place-marker -->\n' % (book_version, )

    if is_source:
        if 'readme-new-app-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'Application: %s <!--- readme-new-app-version-major-minor-feature-commit+meta~breaks-place-marker --> \\\n' % (git_version, )

        if 'readme-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'Library: %s <!--- readme-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker -->\n' % (git_version, )

    return new

def change_source_app_line_if_marked(line, git_version, book_version, book_short, is_book, is_source):
    new = line

    if is_source:
        if 'source-new-app-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'export immutable APP_VERSION = "%s"; // source-new-app-version-major-minor-feature-commit+meta~breaks-place-marker\n' % git_version

    return new

def change_source_lib_line_if_marked(line, git_version, book_version, book_short, is_book, is_source):
    new = line

    if is_source:
        if 'source-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'export immutable LIB_VERSION = "%s"; // source-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker\n' % git_version

    return new

def replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_source, func_change_line_if):

    if os.path.exists(ignore_path):
        os.remove(ignore_path)

    os.rename(orig_path, ignore_path)

    with open(ignore_path, 'r') as old:
        with open(orig_path, 'w') as orig:
            for line in old:
                new = func_change_line_if(line, git_version, book_version, book_short, is_book, is_source)
                orig.write(new)

    return orig_path

def replace_book_entries(git_version, book_version, book_short, root_path, is_book, is_source):

    orig_path = get_full_tex_path(root_path)
    ignore_path = get_full_tex_path(root_path, tex_name=BOOK_IGNORE_TEX_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_source, change_book_line_if_marked)

def replace_readme_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_feature):

    orig_path = get_full_readme_path(root_path)
    ignore_path = get_full_readme_path(root_path, readme_name=README_IGNORE_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_source, change_readme_line_if_marked)

def replace_app_source_entries(git_version, book_version, book_short, root_path, is_book, is_source):

    orig_path = get_app_source_file_path(root_path, SOURCE_APP_MAIN_FILE)
    ignore_path = get_app_source_file_path(root_path, SOURCE_APP_MAIN_IGNORE_FILE)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_source, change_source_app_line_if_marked)

def replace_lib_source_entries(git_version, book_version, book_short, root_path, is_book, is_source):

    orig_path = get_lib_source_file_path(root_path, SOURCE_LIB_MAIN_FILE)
    ignore_path = get_lib_source_file_path(root_path, SOURCE_LIB_MAIN_IGNORE_FILE)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_source, change_source_lib_line_if_marked)

def replace_all_entries(root_path, is_book, is_source, breaks):
    assert is_book or is_source

    auto_updated_files = []
    book_version, book_short = get_current_times()
    git_version = "0.1.0+%s" % book_version

    def append_if_not_empty(path):
        if path:
            auto_updated_files.append(path)

    if is_book:
        # Does *not* use git_version.
        append_if_not_empty( replace_book_entries(git_version, book_version, book_short, root_path, is_book, is_source) )

    if is_source:
        major, minor, feature, commit, prerelease, old_meta, old_breaks = get_current_lib_versions( root_path, decompose_version=True )

        if major is not None and minor is not None and feature is not None:
            if is_major:
                major += 1
                minor = 0
                feature = 0
                commit = 0
            elif is_minor:
                minor += 1
                feature = 0
                commit = 0
            elif is_feature:
                feature += 1
                commit = 0
            elif is_commit:
                commit += 1

            git_version = "%s.%s.%s.%s" % ( str(major), str(minor), str(feature), str(commit) )

            if prerelease is not None:
                # prerelease is copied
                git_version += "-%s" % prerelease

            # old meta is not copied, by default it is date.time
            git_version += "+%s" % book_version

            if breaks is not None:
                # old breaks section is not copied
                git_version += "~%s" % breaks

        append_if_not_empty( replace_app_source_entries(git_version, book_version, book_short, root_path, is_book, is_source) )
        append_if_not_empty( replace_lib_source_entries(git_version, book_version, book_short, root_path, is_book, is_source) )

    if is_book or is_major or is_minor or is_feature:
        append_if_not_empty( replace_readme_entries(git_version, book_version, book_short, root_path, is_book, is_source) )

    return auto_updated_files
