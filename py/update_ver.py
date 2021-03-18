#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.

import os
import os.path
import time
import re

from .git_run import run_process


BOOK_TEX_FOLDER = 'book'
BOOK_TEX_FILE_NAME = 'crochess.tex'
BOOK_IGNORE_TEX_FILE_NAME = 'crochess.IGNORE.tex'

README_FILE_NAME = 'README.md'
README_IGNORE_FILE_NAME = 'README.IGNORE.md'


SOURCE_WS_FOLDER = 'ws'
SOURCE_SRC_FOLDER = 'src'

SOURCE_APP_FOLDER = 'crochess'
SOURCE_APP_CONFIG_FILE = 'Cargo.toml'
SOURCE_IGNORE_APP_CONFIG_FILE = 'Cargo.IGNORE.toml'
SOURCE_APP_MAIN_FILE = 'main.rs'
SOURCE_IGNORE_APP_MAIN_FILE = 'main.IGNORE.rs'

SOURCE_LIB_FOLDER = 'libcrochess'
SOURCE_LIB_CONFIG_FILE = 'Cargo.toml'
SOURCE_IGNORE_LIB_CONFIG_FILE = 'Cargo.IGNORE.toml'
SOURCE_LIB_MAIN_FILE = 'lib.rs'
SOURCE_IGNORE_LIB_MAIN_FILE = 'lib.IGNORE.rs'


#
# https://regex101.com/r/Ly7O1x/3/
# REG_EXP_VERSION_BUILD = re.compile( r"""/^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$/gm""" )

# REG_EXP_VERSION_BUILD = re.compile( r"""version = \"(?P<major>\d+)\.(?P<minor>\d+)\.(?P<patch>\d+)(\+(?P<buildmetadata>\d{14}))?\"""" )
REG_EXP_VERSION_BUILD = re.compile( r"""^version = \"(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?\"""" )

# Possible change, extended versioning + relaxed syntax + breakage info.
# Cargo expects SemVer 2.0 in TOML though, so would need to work around it, or ignore version info there.
#
# /^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)\.(?P<commit>0|[1-9]\d*)(?:-(?P<prerelease>(?:[^-~\+\s]*)))?(?:\~(?P<breaks>(?:[^-~\+\s]*)))?(?:\+(?P<buildmetadata>(?:[^-~\+\s]*)))?$/gm


def get_current_times():
    now = time.gmtime()
    # now_long = time.strftime('%Y-%m-%d %H:%M:%S UTC', now) # e.g. '2020-05-17 02:11:11 UTC'
    book_version = time.strftime('%Y%m%d%H%M%S', now) # e.g. '20200517021111'
    book_short = time.strftime('%Y-%m-%d', now) # e.g. '2020-05-17'
    return (book_version, book_short)

def get_current_lib_versions(root_path, ws_dir=SOURCE_WS_FOLDER, lib_dir=SOURCE_LIB_FOLDER, lib_config_name=SOURCE_LIB_CONFIG_FILE):
    path = get_full_lib_config_path(root_path, ws_dir=ws_dir, lib_dir=lib_dir, lib_config_name=lib_config_name)

    with open(path, 'r') as old:
        for line in old:
            mo = REG_EXP_VERSION_BUILD.match(line)
            if mo is not None:
                major, minor, patch, prerelease, build = mo.groups()
                return (int(major), int(minor), int(patch), \
                        str(prerelease) if prerelease is not None else None, \
                        str(build) if build is not None else None)

    return (None, None, None, None)

def get_full_tex_path(root_path, tex_dir=BOOK_TEX_FOLDER, tex_name=BOOK_TEX_FILE_NAME):
    path = os.path.join(root_path, tex_dir, tex_name)
    return path

def get_full_readme_path(root_path, readme_name=README_FILE_NAME):
    path = os.path.join(root_path, readme_name)
    return path

def get_full_app_config_path(root_path, ws_dir=SOURCE_WS_FOLDER, app_dir=SOURCE_APP_FOLDER, app_config_name=SOURCE_APP_CONFIG_FILE):
    path = os.path.join(root_path, ws_dir, app_dir, app_config_name)
    return path

def get_full_lib_config_path(root_path, ws_dir=SOURCE_WS_FOLDER, lib_dir=SOURCE_LIB_FOLDER, lib_config_name=SOURCE_LIB_CONFIG_FILE):
    path = os.path.join(root_path, ws_dir, lib_dir, lib_config_name)
    return path

def get_full_app_source_path(root_path, ws_dir=SOURCE_WS_FOLDER, app_dir=SOURCE_APP_FOLDER, app_src_dir=SOURCE_SRC_FOLDER, app_source_name=SOURCE_APP_MAIN_FILE):
    path = os.path.join(root_path, ws_dir, app_dir, app_src_dir, app_source_name)
    return path

def get_full_lib_source_path(root_path, ws_dir=SOURCE_WS_FOLDER, lib_dir=SOURCE_LIB_FOLDER, lib_src_dir=SOURCE_SRC_FOLDER, lib_source_name=SOURCE_LIB_MAIN_FILE):
    path = os.path.join(root_path, ws_dir, lib_dir, lib_src_dir, lib_source_name)
    return path

def change_book_line_if_marked(line, git_version, book_version, book_short, is_book, is_major, is_minor, is_patch):
    new = line

    if is_book:
        if 'book-new-commit-version-squished-utc-date-time-place-marker' in line:
            new = '    Version: %s \\\\ [2.0em] %% book-new-commit-version-squished-utc-date-time-place-marker\n' % (book_version, )
        elif 'book-new-commit-version-date-place-marker' in line:
            new = '    %s \\\\ %% book-new-commit-version-date-place-marker\n' % book_short
        elif 'book-new-commit-version-date-small-place-marker' in line:
            new = '    \small{%s} \\\\ [0.5em] %% book-new-commit-version-date-small-place-marker\n' % book_short

    return new

def change_readme_line_if_marked(line, git_version, book_version, book_short, is_book, is_major, is_minor, is_patch):
    new = line

    if is_book:
        if 'readme-new-book-version-squished-utc-date-time-place-marker' in line:
            new = 'Version: %s <!--- readme-new-book-version-squished-utc-date-time-place-marker -->\n' % (book_version, )

    if is_major or is_minor or is_patch:
        if 'readme-new-app-version-major-minor-patch+build-place-marker' in line:
            new = 'Application: %s <!--- readme-new-app-version-major-minor-patch+build-place-marker --> \\\n' % (git_version, )

        if 'readme-new-lib-version-major-minor-patch+build-place-marker' in line:
            new = 'Library: %s <!--- readme-new-lib-version-major-minor-patch+build-place-marker -->\n' % (git_version, )

    return new

def change_config_app_line_if_marked(line, git_version, book_version, book_short, is_book, is_major, is_minor, is_patch):
    new = line

    if is_major or is_minor or is_patch:
        if 'config-new-app-version-major-minor-patch+build-place-marker' in line:
            new = 'version = "%s" # config-new-app-version-major-minor-patch+build-place-marker\n' % git_version

    return new

def change_config_lib_line_if_marked(line, git_version, book_version, book_short, is_book, is_major, is_minor, is_patch):
    new = line

    if is_major or is_minor or is_patch:
        if 'config-new-lib-version-major-minor-patch+build-place-marker' in line:
            new = 'version = "%s" # config-new-lib-version-major-minor-patch+build-place-marker\n' % git_version

    return new

def change_source_app_line_if_marked(line, git_version, book_version, book_short, is_book, is_major, is_minor, is_patch):
    new = line

    if is_major or is_minor or is_patch:
        if 'source-new-app-version-major-minor-patch+build-place-marker' in line:
            new = 'pub const VERSION: &str = "%s"; /* source-new-app-version-major-minor-patch+build-place-marker */\n' % git_version

    return new

def change_source_lib_line_if_marked(line, git_version, book_version, book_short, is_book, is_major, is_minor, is_patch):
    new = line

    if is_major or is_minor or is_patch:
        if 'source-new-lib-version-major-minor-patch+build-place-marker' in line:
            new = 'pub const VERSION: &str = "%s"; /* source-new-lib-version-major-minor-patch+build-place-marker */\n' % git_version

    return new

def replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, func_change_line_if):

    if os.path.exists(ignore_path):
        os.remove(ignore_path)

    os.rename(orig_path, ignore_path)

    with open(ignore_path, 'r') as old:
        with open(orig_path, 'w') as orig:
            for line in old:
                new = func_change_line_if(line, git_version, book_version, book_short, is_book, is_major, is_minor, is_patch)
                orig.write(new)

    return orig_path

def replace_book_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch):

    orig_path = get_full_tex_path(root_path)
    ignore_path = get_full_tex_path(root_path, tex_name=BOOK_IGNORE_TEX_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, change_book_line_if_marked)

def replace_readme_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch):

    orig_path = get_full_readme_path(root_path)
    ignore_path = get_full_readme_path(root_path, readme_name=README_IGNORE_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, change_readme_line_if_marked)

def replace_app_config_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch):

    orig_path = get_full_app_config_path(root_path)
    ignore_path = get_full_app_config_path(root_path, app_config_name=SOURCE_IGNORE_APP_CONFIG_FILE)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, change_config_app_line_if_marked)

def replace_lib_config_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch):

    orig_path = get_full_lib_config_path(root_path)
    ignore_path = get_full_lib_config_path(root_path, lib_config_name=SOURCE_IGNORE_LIB_CONFIG_FILE)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, change_config_lib_line_if_marked)

def replace_app_source_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch):

    orig_path = get_full_app_source_path(root_path)
    ignore_path = get_full_app_source_path(root_path, app_source_name=SOURCE_IGNORE_APP_MAIN_FILE)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, change_source_app_line_if_marked)

def replace_lib_source_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch):

    orig_path = get_full_lib_source_path(root_path)
    ignore_path = get_full_lib_source_path(root_path, lib_source_name=SOURCE_IGNORE_LIB_MAIN_FILE)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, change_source_lib_line_if_marked)

def replace_all_entries(root_path, is_book, is_major, is_minor, is_patch):
    assert is_book or is_major or is_minor or is_patch

    auto_updated_files = []
    book_version, book_short = get_current_times()
    git_version = "0.1.0+%s" % book_version

    def append_if_not_empty(path):
        if path:
            auto_updated_files.append(path)

    if is_book:
        # Does *not* use git_version.
        append_if_not_empty( replace_book_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch) )

    if is_major or is_minor or is_patch:
        major, minor, patch, prerelease, build = get_current_lib_versions(root_path)

        if major is not None and minor is not None and patch is not None:
            if is_major:
                major += 1
                minor = 0
                patch = 0
            elif is_minor:
                minor += 1
                patch = 0
            elif is_patch:
                patch += 1

            if prerelease is None:
                git_version = "%s.%s.%s+%s" % ( str(major), str(minor), str(patch), book_version )
            else:
                git_version = "%s.%s.%s-%s+%s" % ( str(major), str(minor), str(patch), prerelease, book_version )

        append_if_not_empty( replace_app_config_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch) )
        append_if_not_empty( replace_lib_config_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch) )

        append_if_not_empty( replace_app_source_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch) )
        append_if_not_empty( replace_lib_source_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch) )

    if is_book or is_major or is_minor or is_patch:
        append_if_not_empty( replace_readme_entries(git_version, book_version, book_short, root_path, is_book, is_major, is_minor, is_patch) )

    return auto_updated_files
