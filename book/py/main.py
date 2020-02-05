#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


import argparse

import fs
from piece import PieceType
from def_render import RenderingSizeEnum
from save_scene import SaveScene


def main():
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter, \
                                     description='Generates images used in the book.', \
                                     epilog='''Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.''')

    size = parser.add_mutually_exclusive_group(required=True)
    size.add_argument('-f', '--final', action='store_true', default=False, help='render in final size, cca. 2400 dpi (slow!)')
    size.add_argument('-g', '--good', action='store_true', default=False, help='render in good size, cca. 1200 dpi')
    size.add_argument('-n', '--normal', action='store_true', default=False, help='render in normal size, cca. 600 dpi')
    size.add_argument('-d', '--draft', action='store_true', default=False, help='render in draft size, cca. 300 dpi')
    size.add_argument('-i', '--info', action='store_true', default=False, help='render nothing, just print info')

    collections = parser.add_argument_group(title='collections', description='Define which collections will be rendered. Provide at least one of the options bellow.')
                # parser.add_mutually_exclusive_group(required=True)
    collections.add_argument('-a', '--all', action='store_true', default=False, help='render all collections')
    collections.add_argument('-b', '--boards', action='store_true', default=False, help='render initial position boards')
    collections.add_argument('-p', '--pieces', action='store_true', default=False, help='render newly introduced pieces')
    collections.add_argument('-r', '--recent', action='store_true', default=False, help='render recent scenes, move examples')
    collections.add_argument('-x', '--examples', action='store_true', default=False, help='render all scenes, move examples')
    collections.add_argument('-c', '--castlings', action='store_true', default=False, help='render castling examples')
    collections.add_argument('-e', '--en_passant', action='store_true', default=False, help='render en passant examples')
    collections.add_argument('-u', '--rush', action='store_true', default=False, help='render rush examples')

    args = parser.parse_args() # :: argparse.Namespace

    rendering_size = RenderingSizeEnum.none
    if args.final:
        rendering_size = RenderingSizeEnum.Final
    elif args.good:
        rendering_size = RenderingSizeEnum.Good
    elif args.normal:
        rendering_size = RenderingSizeEnum.Normal
    elif args.draft:
        rendering_size = RenderingSizeEnum.Draft
    elif args.info:
        rendering_size = RenderingSizeEnum.Info

    if rendering_size == RenderingSizeEnum.none:
        print
        print "Rendering nothing, no info requested."
        print
        return

    rendering_size = RenderingSizeEnum(rendering_size)
    if rendering_size.needs_rendering():
        fs.create_subfolders('../gfx')

    render = SaveScene(rendering_size)

    if args.all or args.boards:
        render.render_all_boards()

    if args.all or args.pieces:
        render.render_all_pieces()
        render.render_all_pieces(piece_type=PieceType.Bishop)
        render.render_all_pieces(piece_type=PieceType.Star)

    if args.all or args.examples or args.recent:
        render.render_examples(do_all_examples=(args.all or args.examples), enforce_cot_in_bw=True)

    if args.all or args.castlings:
        render.render_all_castling_scenes()

    if args.all or args.en_passant:
        render.render_all_en_passant_scenes()

    if args.all or args.rush:
        render.render_all_rush_scenes()

    print
    print "Finished all renderings."
    print


if __name__ == "__main__":
    main()
