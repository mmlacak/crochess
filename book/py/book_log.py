#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.

import os
import os.path
import re
import time
import subprocess


CMD_ARGS_GIT_COMMIT_COUNT = ['git', 'rev-list', '--count', 'HEAD'] # 'git rev-list --count HEAD', shell=True
CMD_ARGS_GIT_CURRENT_BRANCH = ['git', 'rev-parse', '--abbrev-ref', 'HEAD'] # 'git rev-parse --abbrev-ref HEAD', shell=True
CMD_ARGS_GIT_LAST_COMMIT_ID = ['git', 'log', '--format="%H"', '-n', '1'] # 'git log --format="%H" -n 1', shell=True
CMD_ARGS_GIT_LAST_COMMIT_DATE_TIME = ['git', 'log', '-1', '--format=%ct'] # 'git log -1 --format=%ci', shell=True

BOOK_TEX_FOLDER = 'book'
BOOK_TEX_FILE_NAME = 'crochess.tex'
BOOK_IGNORE_TEX_FILE_NAME = 'crochess.IGNORE.tex'

README_FILE_NAME = 'README.md'
README_IGNORE_FILE_NAME = 'README.IGNORE.md'


def run_process(cmd_args_list):
    output_str = None
    try:
        output_str = subprocess.check_output(cmd_args_list)
    except subprocess.CalledProcessError:
        pass
    return str(output_str, encoding='utf-8', errors='replace')

def get_commit_count():
    count_str = run_process(CMD_ARGS_GIT_COMMIT_COUNT)
    return int(count_str.strip()) if count_str is not None else None

def get_current_branch():
    branch_str = run_process(CMD_ARGS_GIT_CURRENT_BRANCH)
    return branch_str.strip() if branch_str is not None else None

def get_last_commit_id():
    id_str = run_process(CMD_ARGS_GIT_LAST_COMMIT_ID)
    return id_str.strip(' bn\\\"\'\n') if id_str is not None else None

def get_last_commit_date_time():
    date_time_str = run_process(CMD_ARGS_GIT_LAST_COMMIT_DATE_TIME)
    if date_time_str is None:
        return None

    date_time = time.gmtime( int( date_time_str.strip() ) )
    date_time_str = time.strftime('%Y-%m-%d %H:%M:%S UTC', date_time) # e.g. '2020-05-17 02:11:11 UTC'
    return date_time_str

def get_project_path(root_path=None):
    path = root_path
    if path is None:
        path = os.getcwd()
    path = os.path.abspath(path)
    path = os.path.normpath(path)
    path = os.path.normcase(path)
    return path

def get_current_times():
    now = time.gmtime()
    now_long = time.strftime('%Y-%m-%d %H:%M:%S UTC', now) # e.g. '2020-05-17 02:11:11 UTC'
    now_short = time.strftime('%Y-%m-%d', now) # e.g. '2020-05-17'
    return (now_long, now_short)

def get_full_tex_path(root_path=None, tex_dir=BOOK_TEX_FOLDER, tex_name=BOOK_TEX_FILE_NAME):
    path = get_project_path(root_path=root_path)
    path = os.path.join(path, tex_dir, tex_name)
    return path

def get_full_readme_path(root_path=None, readme_name=README_FILE_NAME):
    path = get_project_path(root_path=root_path)
    path = os.path.join(path, readme_name)
    return path

def change_log_line_if_marked(line, id_, last_commit_date_time, now_long, now_short, counter, branch):
    new = line
    if 'new-commit-count-date-time-branch-place-marker' in line:
        new = '    %i \\textperiodcentered \\textperiodcentered \\textperiodcentered ~%s \\textperiodcentered \\textperiodcentered \\textperiodcentered ~%s \\\\ [2.0em] %% new-commit-count-date-time-branch-place-marker\n' % (counter+1, now_long, branch)
    elif 'last-commit-id-place-marker' in line:
        new = '        %s \\\\ %% last-commit-id-place-marker\n' % id_
    # elif 'last-commit-pdfinfo-place-marker' in line:
    #     new = '            pdfkeywords={chess, variants, %s, %s}, %% last-commit-pdfinfo-place-marker\n' % (id_, last_commit_date_time)
    elif 'last-commit-date-time-place-marker' in line:
        new = '        %s \\\\ %% last-commit-date-time-place-marker\n' % last_commit_date_time
    elif 'new-commit-date-place-marker' in line:
        new = '    %s \\\\ %% new-commit-date-place-marker\n' % now_short
    elif 'new-commit-date-small-place-marker' in line:
        new = '    \small{%s} \\\\ [0.5em] %% new-commit-date-small-place-marker\n' % now_short
    return new

def change_readme_line_if_marked(line, id_, last_commit_date_time, now_long, now_short, counter, branch):
    new = line
    if line.count('···') == 2 and line.count('UTC') == 1 and line.count('-') == 2 and line.count(':') == 2:
        new = '%i ··· %s ··· %s\n' % (counter+1, now_long, branch)
    return new

def _raise_not_git_repo():
    raise RuntimeError( "Probably not a git repo, not updating book version info." )

def get_commit_info():
    id_ = get_last_commit_id() or _raise_not_git_repo()

    date_time_ = get_last_commit_date_time() or _raise_not_git_repo()

    counter = get_commit_count() or _raise_not_git_repo()

    branch = get_current_branch() or _raise_not_git_repo()

    return (id_, date_time_, counter, branch)

def replace_entries(now_long, now_short, orig_path, ignore_path, func_change_line_if, root_path=None):

    id_, date_time_, counter, branch = get_commit_info()

    if os.path.exists(ignore_path):
        os.remove(ignore_path)

    os.rename(orig_path, ignore_path)

    with open(ignore_path, 'r') as old:
        with open(orig_path, 'w') as orig:
            for line in old:
                new = func_change_line_if(line, id_, date_time_, now_long, now_short, counter, branch)
                orig.write(new)

def replace_log_entries(now_long, now_short, root_path=None):

    orig_path = get_full_tex_path(root_path=root_path)
    ignore_path = get_full_tex_path(root_path=root_path, tex_name=BOOK_IGNORE_TEX_FILE_NAME)

    replace_entries(now_long, now_short, orig_path, ignore_path, change_log_line_if_marked, root_path=root_path)

def replace_readme_entries(now_long, now_short, root_path=None):

    orig_path = get_full_readme_path(root_path=root_path)
    ignore_path = get_full_readme_path(root_path=root_path, readme_name=README_IGNORE_FILE_NAME)

    replace_entries(now_long, now_short, orig_path, ignore_path, change_readme_line_if_marked, root_path=root_path)


if __name__ == '__main__':
    # Should be run from project root folder.

    now_long, now_short = get_current_times()

    replace_log_entries(now_long, now_short)

    replace_readme_entries(now_long, now_short)
