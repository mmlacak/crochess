#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import os
import os.path
import re
import time


GIT_FOLDER = '.git'
GIT_LOG_FOLDER = 'logs'
GIT_LOG_FILE_NAME = 'HEAD'

LOG_LINE_REGEXP = '''(?P<parent>[\da-fA-F]{40})\s(?P<id>[\da-fA-F]{40})\s(?P<commiter>.*)\s(?P<time>\d{10})\s\+\d{4}\scommit:\s(?P<msg>.*)'''
LOG_LINE_REGEXP_OBJ = re.compile(LOG_LINE_REGEXP)

BOOK_TEX_FOLDER = 'book'
BOOK_TEX_FILE_NAME = 'crochess.tex'
BOOK_IGNORE_TEX_FILE_NAME = 'crochess.IGNORE.tex'


def get_project_path(root_path=None):
    path = root_path
    if path is None:
        path = os.getcwd()
    path = os.path.abspath(path)
    path = os.path.normpath(path)
    path = os.path.normcase(path)
    return path

def get_full_log_path(root_path=None, git_dir=GIT_FOLDER, git_log_dir=GIT_LOG_FOLDER, filename=GIT_LOG_FILE_NAME):
    path = get_project_path(root_path=root_path)
    path = os.path.join(path, git_dir, git_log_dir, filename)
    return path

def get_log_entry(root_path=None):
    log_path = get_full_log_path(root_path=root_path)
    last_line = None
    matches = None
    results = None

    try:
        with open(log_path, 'r') as log:
            for line in log:
                last_line = line
    except:
        return None

    if last_line is not None:
        matches = LOG_LINE_REGEXP_OBJ.match(last_line)

    if matches is not None:
        results = matches.group('parent', 'id', 'commiter', 'time', 'msg') 

        time_ = int(results[3])
        time_ = time.gmtime(time_) 
        long_ = time.strftime('%Y-%m-%d %H:%M:%S UTC', time_) # e.g. '2020-05-17 02:11:11 UTC'
        short_ = time.strftime('%Y-%m-%d', time_) # e.g. '2020-05-17'

        now = time.gmtime() 
        now_long = time.strftime('%Y-%m-%d %H:%M:%S UTC', now) # e.g. '2020-05-17 02:11:11 UTC'
        now_short = time.strftime('%Y-%m-%d', now) # e.g. '2020-05-17'

        results = results + (long_, short_, now_long, now_short) 

    return results

def get_full_tex_path(root_path=None, tex_dir=BOOK_TEX_FOLDER, tex_name=BOOK_TEX_FILE_NAME):
    path = get_project_path(root_path=root_path)
    path = os.path.join(path, tex_dir, tex_name)
    return path

def change_line_if_marked(line, id_, long_, short, now_long, now_short):
    new = line
    if 'last-commit-id-place-marker' in line:
        new = '    %% %s \\\\ %% last-commit-id-place-marker\n' % id_
    elif 'last-commit-date-time-place-marker' in line:
        new = '    %% %s \\\\ %% last-commit-date-time-place-marker\n' % long_
    elif 'new-commit-date-time-place-marker' in line:
        new = '    %s \\\\ [2.0em] %% new-commit-date-time-place-marker\n' % now_long
    elif 'new-commit-date-place-marker' in line:
        new = '    %s \\\\ %% new-commit-date-place-marker\n' % now_short # short
    elif 'new-commit-date-small-place-marker' in line:
        new = '    \small{%s} \\\\ [0.5em] %% new-commit-date-small-place-marker\n' % now_short # short
    return new

def replace_log_entries(root_path=None):
    log_results = get_log_entry(root_path=root_path)
    if log_results is None:
        return

    parent, id_, commiter, time_, msg, long_, short, now_long, now_short = log_results
    orig_path = get_full_tex_path(root_path=root_path)
    ignore_path = get_full_tex_path(root_path=root_path, tex_name=BOOK_IGNORE_TEX_FILE_NAME)

    if os.path.exists(ignore_path):
        os.remove(ignore_path)

    os.rename(orig_path, ignore_path)

    with open(ignore_path, 'r') as old:
        with open(orig_path, 'w') as orig:
            for line in old:
                new = change_line_if_marked(line, id_, long_, short, now_long, now_short)
                orig.write(new)


if __name__ == '__main__':
    # Should be run from project root folder.
    replace_log_entries()
