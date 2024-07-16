#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

import os
import os.path
import time
import re

import py.build_env as BE


BOOK_TEX_FOLDER = 'book'
BOOK_TEX_FILE_NAME = 'crochess.tex'
BOOK_IGNORE_TEX_FILE_NAME = 'crochess.IGNORE.tex'

SPHINX_CONFIG_FILE_NAME = 'conf.py'
SPHINX_IGNORE_CONFIG_FILE_NAME = 'conf.IGNORE.py'

README_FILE_NAME = 'README.md'
README_IGNORE_FILE_NAME = 'README.IGNORE.md'


# TODO :: import everything else from py.build_env

APP_VERSION_SRC_FILE_NAME = 'crochess.c'
APP_VERSION_SRC_IGNORE_FILE_NAME = 'crochess.IGNORE.c'

LIB_VERSION_SRC_FILE_NAME = 'cc_version.c'
LIB_VERSION_SRC_IGNORE_FILE_NAME = 'cc_version.IGNORE.c'

TESTS_VERSION_SRC_FILE_NAME = 'tests.c'
TESTS_VERSION_SRC_IGNORE_FILE_NAME = 'tests.IGNORE.c'

#
# \"(?P<version>.*)\"
REG_EXP_COMPLETE_VERSION_STRING = re.compile( r'''\"(?P<version>.*)\"''' ) # "\"(?P<version>.*)\"" ) # r"""\"(?P<version>.*)\"""" )

#
# https://regex101.com/r/lqmlXE/2
# ^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)(?:\.(?P<feature>0|[1-9]\d*)(?:\.(?P<commit>0|[1-9]\d*))?)?(?:\:(?P<count>0|[1-9]\d*))?(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<meta>[^~\s]*))?(?:(?P<breaks>\~[^\s]*?))?$
#

REG_EXP_VERSION_DECONSTRUCTED = re.compile( r"""^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)(?:\.(?P<feature>0|[1-9]\d*)(?:\.(?P<commit>0|[1-9]\d*))?)?(?:\:(?P<count>0|[1-9]\d*))?(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<meta>[^~\s]*))?(?:(?P<breaks>\~[^\s]*?))?$""" )


def get_current_times():
    now = time.gmtime()
    # now_long = time.strftime('%Y-%m-%d %H:%M:%S UTC', now) # e.g. '2020-05-17 02:11:11 UTC'
    book_version = time.strftime('%Y%m%d.%H%M%S', now) # e.g. '20200517.021111'
    book_short = time.strftime('%Y-%m-%d', now) # e.g. '2020-05-17'
    return (book_version, book_short)

def get_current_lib_versions(root_path, decompose_version=True):
    path = get_lib_src_file_path(root_path, LIB_VERSION_SRC_FILE_NAME)

    try:
        with open(path, 'r') as old:
            for line in old:
                if 'source-new-libcrochess-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
                    mo = REG_EXP_COMPLETE_VERSION_STRING.search(line)
                    if mo is not None:
                        version = mo.group('version')

                        if not decompose_version:
                            return version

                        mo = REG_EXP_VERSION_DECONSTRUCTED.match(version)
                        if mo is not None:
                            major, minor, feature, commit, count, prerelease, meta, breaks = mo.groups()
                            return (int(major), \
                                    int(minor), \
                                    int(feature) if feature is not None else None, \
                                    int(commit) if commit is not None else None, \
                                    int(count) if count is not None else None, \
                                    str(prerelease) if prerelease is not None else None, \
                                    str(meta) if meta is not None else None, \
                                    str(breaks) if breaks is not None else None)
                    else:
                        return (None, None, None, None, None, None, None, None) if decompose_version else None
    except FileNotFoundError:
        # FileNotFoundError: [Errno 2] No such file or directory: '.../crochess/ws/libcrochess/Cargo.toml'
        return (None, None, None, None, None, None, None, None) if decompose_version else None

    return (None, None, None, None, None, None, None, None) if decompose_version else None

def get_full_tex_path(root_path, tex_dir=BOOK_TEX_FOLDER, tex_name=BOOK_TEX_FILE_NAME):
    path = os.path.join(root_path, tex_dir, tex_name)
    return path

def get_full_docs_path(root_path, docs_config_dir=BE.DOCS_SOURCE_FOLDER, docs_config_name=SPHINX_CONFIG_FILE_NAME):
    path = os.path.join(root_path, docs_config_dir, docs_config_name)
    return path

def get_full_readme_path(root_path, readme_name=README_FILE_NAME):
    path = os.path.join(root_path, readme_name)
    return path

def get_app_src_file_path(root_path, file_name):
    path = os.path.join(BE.get_app_src_dir(root_path), file_name)
    return path

def get_lib_src_file_path(root_path, file_name):
    path = os.path.join(BE.get_lib_src_dir(root_path), file_name)
    return path

def get_tests_src_file_path(root_path, file_name):
    path = os.path.join(BE.get_tests_src_dir(root_path), file_name)
    return path

def change_book_line_if_marked(line, git_version, book_version, book_short, is_book, is_docs, is_source):
    new = line

    if is_book:
        if 'book-new-commit-version-squished-utc-date-time-place-marker' in line:
            new = '    Version: %s \\\\ [2.0em] %% book-new-commit-version-squished-utc-date-time-place-marker\n' % (book_version, )
        elif 'book-new-commit-version-date-place-marker' in line:
            new = '    %s \\\\ %% book-new-commit-version-date-place-marker\n' % book_short
        elif 'book-new-commit-version-date-small-place-marker' in line:
            new = '    \small{%s} \\\\ [0.5em] %% book-new-commit-version-date-small-place-marker\n' % book_short

    return new

def change_docs_line_if_marked(line, git_version, book_version, book_short, is_book, is_docs, is_source):
    new = line

    if is_docs:
        if 'docs-new-lib-short-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'version = "%s"   # docs-new-lib-short-version-major-minor-feature-commit+meta~breaks-place-marker\n' % (git_version, )

        if 'docs-new-lib-full-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'release = "%s"   # docs-new-lib-full-version-major-minor-feature-commit+meta~breaks-place-marker\n' % (git_version, )

    return new

def change_readme_line_if_marked(line, git_version, book_version, book_short, is_book, is_docs, is_source):
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

def change_source_app_line_if_marked(line, git_version, book_version, book_short, is_book, is_docs, is_source):
    new = line

    if is_source:
        if 'source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'char const CROCHESS_VERSION[] = "%s"; // source-new-crochess-version-major-minor-feature-commit+meta~breaks-place-marker\n' % git_version

    return new

def change_source_lib_line_if_marked(line, git_version, book_version, book_short, is_book, is_docs, is_source):
    new = line

    if is_source:
        if 'source-new-libcrochess-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'char const CC_LIB_VERSION[] = "%s"; // source-new-libcrochess-version-major-minor-feature-commit+meta~breaks-place-marker\n' % git_version

    return new

def change_source_tests_line_if_marked(line, git_version, book_version, book_short, is_book, is_docs, is_source):
    new = line

    if is_source:
        if 'source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker' in line:
            new = 'char const CROCHESS_TESTS_VERSION[] = "%s"; // source-new-crochess-tests-version-major-minor-feature-commit+meta~breaks-place-marker\n' % git_version

    return new

def replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_docs, is_source, func_change_line_if):

    if os.path.exists(ignore_path):
        os.remove(ignore_path)

    os.rename(orig_path, ignore_path)

    with open(ignore_path, 'r') as old:
        with open(orig_path, 'w') as orig:
            for line in old:
                new = func_change_line_if(line, git_version, book_version, book_short, is_book, is_docs, is_source)
                orig.write(new)

    return orig_path

def replace_book_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source):

    orig_path = get_full_tex_path(root_path)
    ignore_path = get_full_tex_path(root_path, tex_name=BOOK_IGNORE_TEX_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_docs, is_source, change_book_line_if_marked)

def replace_docs_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source):

    orig_path = get_full_docs_path(root_path)
    ignore_path = get_full_docs_path(root_path, docs_config_name=SPHINX_IGNORE_CONFIG_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_docs, is_source, change_docs_line_if_marked)

def replace_readme_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source):

    orig_path = get_full_readme_path(root_path)
    ignore_path = get_full_readme_path(root_path, readme_name=README_IGNORE_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_docs, is_source, change_readme_line_if_marked)

def replace_app_source_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source):

    orig_path = get_app_src_file_path(root_path, APP_VERSION_SRC_FILE_NAME)
    ignore_path = get_app_src_file_path(root_path, APP_VERSION_SRC_IGNORE_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_docs, is_source, change_source_app_line_if_marked)

def replace_lib_source_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source):

    orig_path = get_lib_src_file_path(root_path, LIB_VERSION_SRC_FILE_NAME)
    ignore_path = get_lib_src_file_path(root_path, LIB_VERSION_SRC_IGNORE_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_docs, is_source, change_source_lib_line_if_marked)

def replace_tests_source_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source):

    orig_path = get_tests_src_file_path(root_path, TESTS_VERSION_SRC_FILE_NAME)
    ignore_path = get_tests_src_file_path(root_path, TESTS_VERSION_SRC_IGNORE_FILE_NAME)

    return replace_entries(git_version, book_version, book_short, orig_path, ignore_path, is_book, is_docs, is_source, change_source_tests_line_if_marked)

def replace_all_entries(root_path, is_book, is_docs, is_major, is_minor, is_feature, is_commit, is_meta, count, breaks):
    is_source = is_major or is_minor or is_feature or is_commit or is_meta
    assert is_book or is_docs or is_source

    auto_updated_files = []
    book_version, book_short = get_current_times()
    git_version = "0.1.0+%s" % book_version

    def append_if_not_empty(path):
        if path:
            auto_updated_files.append(path)

    if is_book:
        # Does *not* use git_version.
        append_if_not_empty( replace_book_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source) )

    if is_source:
        major, minor, feature, commit, old_count, prerelease, old_meta, old_breaks = get_current_lib_versions( root_path, decompose_version=True )
        assert major is not None and minor is not None

        if is_major:
            major += 1
            minor = 0

            if feature is not None:
                feature = 0

                if commit is not None:
                    commit = 0
        elif is_minor:
            minor += 1

            if feature is not None:
                feature = 0

                if commit is not None:
                    commit = 0
        elif is_feature:
            if feature is not None:
                feature += 1
            else:
                feature = 1

            if commit is not None:
                commit = 0
        elif is_commit:
            if feature is None:
                feature = 0

            if commit is not None:
                commit += 1
            else:
                commit = 1

        git_version = "%s.%s" % ( str(major), str(minor), )

        if feature is not None:
            git_version += ".%s" % str(feature)

            if commit is not None:
                git_version += ".%s" % str(commit)

        if not is_meta:
            if old_count is not None:
                count = count if count is not None and old_count < count else old_count + 1
                git_version += ":%s" % str(count)
            else:
                if count is not None:
                    git_version += ":%s" % str(count)
        else:
            if old_count is not None:
                git_version += ":%s" % str(old_count)
            else:
                if count is not None:
                    git_version += ":%s" % str(count)

        if prerelease is not None:
            # prerelease is copied
            git_version += "-%s" % prerelease

        # old meta is not copied, by default it is current date.time
        git_version += "+%s" % book_version

        if breaks is not None:
            if not breaks.startswith('~'):
                breakage = 4 if is_major else \
                           3 if is_minor else \
                           2 if is_feature else \
                           1
                breaks = '~' * breakage + breaks

            # old breaks section is not copied
            git_version += "%s" % breaks

        append_if_not_empty( replace_app_source_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source) )
        append_if_not_empty( replace_lib_source_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source) )
        append_if_not_empty( replace_tests_source_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source) )

    # Keep at back, in case library version gets updated too.
    if is_docs:
        git_version = get_current_lib_versions( root_path, decompose_version=False )
        assert git_version is not None and git_version.strip() != ""

        # *Does* use git_version.
        append_if_not_empty( replace_docs_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source) )

    if is_book or is_source:
        # Works, because if book, then git_version is *not* used.
        append_if_not_empty( replace_readme_entries(git_version, book_version, book_short, root_path, is_book, is_docs, is_source) )

    return auto_updated_files
