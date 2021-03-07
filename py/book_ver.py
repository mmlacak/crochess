#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.

import os
import os.path
import time

from .git_run import run_process


BOOK_TEX_FOLDER = 'book'
BOOK_TEX_FILE_NAME = 'crochess.tex'
BOOK_IGNORE_TEX_FILE_NAME = 'crochess.IGNORE.tex'

README_FILE_NAME = 'README.md'
README_IGNORE_FILE_NAME = 'README.IGNORE.md'


def get_current_times():
    now = time.gmtime()
    # now_long = time.strftime('%Y-%m-%d %H:%M:%S UTC', now) # e.g. '2020-05-17 02:11:11 UTC'
    now_version = time.strftime('%Y%m%d%H%M%S', now) # e.g. '20200517021111'
    now_short = time.strftime('%Y-%m-%d', now) # e.g. '2020-05-17'
    return (now_version, now_short)

def get_full_tex_path(root_path, tex_dir=BOOK_TEX_FOLDER, tex_name=BOOK_TEX_FILE_NAME):
    path = os.path.join(root_path, tex_dir, tex_name)
    return path

def get_full_readme_path(root_path, readme_name=README_FILE_NAME):
    path = os.path.join(root_path, readme_name)
    return path

def change_book_line_if_marked(line, now_version, now_short, is_book, is_major, is_minor, is_patch):
    new = line
    if is_book:
        if 'book-new-commit-version-squished-utc-date-time-place-marker' in line:
            new = '    Version: %s \\\\ [2.0em] %% book-new-commit-version-squished-utc-date-time-place-marker\n' % (now_version, )
        elif 'book-new-commit-version-date-place-marker' in line:
            new = '    %s \\\\ %% book-new-commit-version-date-place-marker\n' % now_short
        elif 'book-new-commit-version-date-small-place-marker' in line:
            new = '    \small{%s} \\\\ [0.5em] %% book-new-commit-version-date-small-place-marker\n' % now_short
    return new

def change_readme_line_if_marked(line, now_version, now_short, is_book, is_major, is_minor, is_patch):
    new = line
    if is_book:
        if 'readme-new-commit-version-squished-utc-date-time-place-marker' in line:
            new = 'Version: %s <!--- readme-new-commit-version-squished-utc-date-time-place-marker -->\n' % (now_version, )
    return new

def replace_entries(now_version, now_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, func_change_line_if):

    if os.path.exists(ignore_path):
        os.remove(ignore_path)

    os.rename(orig_path, ignore_path)

    with open(ignore_path, 'r') as old:
        with open(orig_path, 'w') as orig:
            for line in old:
                new = func_change_line_if(line, now_version, now_short, is_book, is_major, is_minor, is_patch)
                orig.write(new)

def replace_book_entries(now_version, now_short, root_path, is_book, is_major, is_minor, is_patch):

    orig_path = get_full_tex_path(root_path)
    ignore_path = get_full_tex_path(root_path, tex_name=BOOK_IGNORE_TEX_FILE_NAME)

    replace_entries(now_version, now_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, change_book_line_if_marked)

def replace_readme_entries(now_version, now_short, root_path, is_book, is_major, is_minor, is_patch):

    orig_path = get_full_readme_path(root_path)
    ignore_path = get_full_readme_path(root_path, readme_name=README_IGNORE_FILE_NAME)

    replace_entries(now_version, now_short, orig_path, ignore_path, is_book, is_major, is_minor, is_patch, change_readme_line_if_marked)

def replace_all_entries(root_path, is_book, is_major, is_minor, is_patch):
    assert is_book or is_major or is_minor or is_patch

    now_version, now_short = get_current_times()

    if is_book:
        replace_book_entries(now_version, now_short, root_path, is_book, is_major, is_minor, is_patch)

    if is_book or is_major or is_minor or is_patch:
        replace_readme_entries(now_version, now_short, root_path, is_book, is_major, is_minor, is_patch)
