#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import os
import os.path
import re
import time

GIT_FOLDER = '.git'
GIT_REF_FILE_NAME = 'HEAD'
GIT_LOG_FOLDER = 'logs'
# GIT_LOG_FILE_NAME = 'HEAD'

REF_LINE_REGEXP = '''ref: (?P<path>.*)'''
REF_LINE_REGEXP_OBJ = re.compile(REF_LINE_REGEXP)

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

def get_ref_log_path(root_path=None, git_dir=GIT_FOLDER, git_ref_file_name=GIT_REF_FILE_NAME):
    path = get_project_path(root_path=root_path)
    path = os.path.join(path, git_dir, git_ref_file_name)
    return path

def get_full_log_path(root_path=None, git_dir=GIT_FOLDER, git_log_folder=GIT_LOG_FOLDER, git_log_paths=None):
    if git_log_paths is None:
        return None
    path = get_project_path(root_path=root_path)
    path = os.path.join(path, git_dir, git_log_folder, *git_log_paths)
    return path

def get_ref_paths(root_path=None):
    ref_path = get_ref_log_path(root_path=root_path)
    first_line = None

    try:
        with open(ref_path, 'r') as ref:
            first_line = ref.readline()
    except:
        return None

    if first_line is not None:
        matches = REF_LINE_REGEXP_OBJ.match(first_line)

    if matches is not None:
        results = matches.group('path') 

        paths = results.split('/')
        results = (results, paths, )

    return paths # results

def get_log_entry(root_path=None, git_log_paths=None):
    log_path = get_full_log_path(root_path=root_path, git_log_paths=git_log_paths)
    last_line = None
    matches = None
    results = None
    counter = None
    branch = git_log_paths[ -1 ]

    try:
        with open(log_path, 'r') as log:
            counter = 0
            for line in log:
                last_line = line
                counter += 1
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

        results = results + (long_, short_, now_long, now_short, counter, branch) 

    return results

def get_full_tex_path(root_path=None, tex_dir=BOOK_TEX_FOLDER, tex_name=BOOK_TEX_FILE_NAME):
    path = get_project_path(root_path=root_path)
    path = os.path.join(path, tex_dir, tex_name)
    return path

def change_line_if_marked(line, id_, long_, short, now_long, now_short, counter, branch):
    new = line
    if 'last-commit-id-place-marker' in line:
        new = '    %% %s \\\\ %% last-commit-id-place-marker\n' % id_
    elif 'last-commit-date-time-place-marker' in line:
        new = '    %% %s \\\\ %% last-commit-date-time-place-marker\n' % long_
    elif 'new-commit-count-date-time-branch-place-marker' in line:
        new = '    %i \\textperiodcentered \\textperiodcentered \\textperiodcentered ~%s \\textperiodcentered \\textperiodcentered \\textperiodcentered ~%s \\\\ [2.0em] %% new-commit-count-date-time-branch-place-marker\n' % (counter+1, now_long, branch)
    elif 'new-commit-date-place-marker' in line:
        new = '    %s \\\\ %% new-commit-date-place-marker\n' % now_short 
    elif 'new-commit-date-small-place-marker' in line:
        new = '    \small{%s} \\\\ [0.5em] %% new-commit-date-small-place-marker\n' % now_short 
    return new

def replace_log_entries(root_path=None):
    ref_paths = get_ref_paths(root_path=root_path) # :: [ path_str_1, ... ] # :: ( full_path_str, [ path_str_1, ... ] )
    if ref_paths is None:
        return

    log_results = get_log_entry(root_path=root_path, git_log_paths=ref_paths)
    if log_results is None:
        return

    parent, id_, commiter, time_, msg, long_, short, now_long, now_short, counter, branch = log_results
    orig_path = get_full_tex_path(root_path=root_path)
    ignore_path = get_full_tex_path(root_path=root_path, tex_name=BOOK_IGNORE_TEX_FILE_NAME)

    if os.path.exists(ignore_path):
        os.remove(ignore_path)

    os.rename(orig_path, ignore_path)

    with open(ignore_path, 'r') as old:
        with open(orig_path, 'w') as orig:
            for line in old:
                new = change_line_if_marked(line, id_, long_, short, now_long, now_short, counter, branch)
                orig.write(new)


if __name__ == '__main__':
    # Should be run from project root folder.
    replace_log_entries()
