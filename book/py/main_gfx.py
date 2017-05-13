#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2017 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import argparse

import fs
from gfx import GfxRender

import debug_

def main():
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter, \
                                     description='Generates images used in book.', \
                                     epilog='''Copyright (c) 2010 - 2017 Mario Mlačak, mmlacak@gmail.com
Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.''')

    size = parser.add_mutually_exclusive_group(required=True)
    size.add_argument('-f', '--final', action='store_true', default=False, help='render in final size (huge!)')
    size.add_argument('-d', '--draft', action='store_true', default=False, help='render in draft size')

    collections = parser.add_argument_group(title='collections', description='Define which collections will be rendered. Provide at least one of the options bellow.')
                # parser.add_mutually_exclusive_group(required=True)
    collections.add_argument('-a', '--all', action='store_true', default=False, help='render all collections')
    collections.add_argument('-b', '--boards', action='store_true', default=False, help='render initial position boards')
    collections.add_argument('-p', '--pieces', action='store_true', default=False, help='render newly introduced pieces')
    collections.add_argument('-r', '--recent_scenes', action='store_true', default=False, help='render recent scenes, move examples')
    collections.add_argument('-s', '--scenes', action='store_true', default=False, help='render all scenes, move examples')
    collections.add_argument('-c', '--castlings', action='store_true', default=False, help='render castling examples')
    collections.add_argument('-e', '--en_passant', action='store_true', default=False, help='render en passant examples')

    args = parser.parse_args() # :: argparse.Namespace
#     print
#     print "args:", args
#     print
#     print "vars:", vars(args)
#     print

    fs.create_subfolders('../gfx')

    render = GfxRender()

    if args.all or args.boards:
        render.render_all_boards()

    if args.all or args.pieces:
        render.render_all_newly_introduced_pieces()

    if args.all or args.scenes or args.recent_scenes:
        render.render_all_example_scenes(do_all_scenes=args.scenes)

    if args.all or args.castlings:
        render.render_all_castling_scenes()

    if args.all or args.en_passant:
        render.render_all_en_passant_scenes()

    print
    print "Finished all renderings."
    print

if __name__ == "__main__":
    main()
