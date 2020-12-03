#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


import argparse

from consts import DEFAULT_IMAGE_FOLDER_REL_PATH
import fs
from piece import PieceType
from def_render import RenderingSizeEnum
from save_scene import SaveScene

from board import BoardType


def get_board_type_choices():
    bts = BoardType.get_all_list(include_none=False)
    lbls = [ bt.get_label() for bt in bts ]
    lbls.extend( ['all', 'even', 'odd'] )
    return lbls

def get_board_types(labels):
    if labels is None:
        return None

    bts = []

    if 'all' in labels:
        bts.extend( BoardType.get_all_list(include_none=False) )

    if 'even' in labels:
        bts.extend( BoardType.get_even_list() )

    if 'odd' in labels:
        bts.extend( BoardType.get_odd_list() )

    if not labels:
        bts.extend( BoardType.get_all_list(include_none=False) )
    else:
        btx = [ BoardType.get(lbl) for lbl in labels if lbl not in ['all', 'even', 'odd'] ]
        bts.extend( btx )

    bts = [ bt for bt in bts if bt != BoardType.none ]
    bts = sorted( list( set( bts ) ) )
    return bts

def get_board_labels(labels):
    if labels is None:
        return None

    bts = get_board_types(labels) # So that it's properly sorted.
    lbls = [ bt.get_label() for bt in bts ]
    return lbls

def main():
    parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter, \
                                     description='Generates images used in the book.', \
                                     epilog='''
Arguments -x and -I expect at least one of the following options:

all  - all variants will be rendered
even - even variants
odd  - odd variants

oc   - Odd Classical chess
c    - Classical chess
oct  - Odd Croatian Ties
ct   - Croatian Ties
oma  - Odd Mayan Ascendancy
ma   - Mayan Ascendancy
oaoa - Odd Age Of Aquarius
aoa  - Age Of Aquarius
omv  - Odd Mirandas Veil
mv   - Mirandas Veil
on   - Odd Nineteen
n    - Nineteen
ohd  - Odd Hemeras Dawn
hd   - Hemeras Dawn
otr  - Odd Tamoanchan Revisited
tr   - Tamoanchan Revisited
ocot - Odd Conquest Of Tlalocan
cot  - Conquest Of Tlalocan
od   - Odd Discovery
d    - Discovery
oo   - Odd One
o    - One

Any combination will work, multiple options separate by space, like so:
$ python3 main.py -d -x even oct oma

Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.''')

    size = parser.add_mutually_exclusive_group(required=True)
    size.add_argument('-f', '--final', action='store_true', default=False, help='render in final size, cca. 2400 dpi (slow!)')
    size.add_argument('-g', '--good', action='store_true', default=False, help='render in good size, cca. 1200 dpi')
    size.add_argument('-n', '--normal', action='store_true', default=False, help='render in normal size, cca. 600 dpi')
    size.add_argument('-d', '--draft', action='store_true', default=False, help='render in draft size, cca. 300 dpi')
    size.add_argument('-i', '--info', action='store_true', default=False, help='render nothing, just print info')

    collections = parser.add_argument_group(title='collections', description='Define which collections will be rendered. Provide at least one of the options bellow.')
                # parser.add_mutually_exclusive_group(required=True)
    collections.add_argument('-a', '--all', action='store_true', default=False, help='render all collections, except ISA')
    collections.add_argument('-b', '--boards', action='store_true', default=False, help='render initial position boards')
    collections.add_argument('-p', '--pieces', action='store_true', default=False, help='render newly introduced pieces')
    collections.add_argument('-r', '--recent', action='store_true', default=False, help='render recent scenes, move examples')
    collections.add_argument('-x', '--examples', action='extend', choices=get_board_type_choices(), nargs='+', help='render scenes, move examples') # Doesn't work with nargs='*': const=['all', ] :/
    collections.add_argument('-c', '--castlings', action='store_true', default=False, help='render castling examples')
    collections.add_argument('-e', '--en_passant', action='store_true', default=False, help='render en passant examples')
    collections.add_argument('-u', '--rush', action='store_true', default=False, help='render rush examples')

    collections.add_argument('-I', '--isa', action='extend', choices=get_board_type_choices(), nargs='+', help='render all examples of initial setup analysis') # Doesn't work with nargs='*': const=['all', ] :/
    collections.add_argument('-C', '--isa_centaur', action='store_true', default=False, help='do render Centaur ISA examples')
    collections.add_argument('-P', '--isa_patterns', action='store_true', default=False, help='do render ISA patterns (currently only Centaur)')

    collections.add_argument('-T', '--tests', action='store_true', default=False, help='render all tests')
    collections.add_argument('-t', '--recent_tests', action='store_true', default=False, help='render recent tests')

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
        print( "Rendering nothing, no info requested." )
        print
        return

    rendering_size = RenderingSizeEnum(rendering_size)
    if rendering_size.needs_rendering():
        fs.create_subfolders(DEFAULT_IMAGE_FOLDER_REL_PATH)

    render = SaveScene(rendering_size)

    if args.all or args.boards:
        render.render_all_boards()

    if args.all or args.pieces:
        render.render_all_pieces()
        render.render_all_pieces(piece_type=PieceType.Bishop)
        render.render_all_pieces(piece_type=PieceType.Star)

    if args.all or args.examples or args.recent:
        bts = BoardType.get_all_list() if args.all else get_board_types(args.examples)
        render.render_examples(do_all_examples=(args.all or (not args.recent)), board_types=bts, enforce_cot_in_bw=True)

    if args.all or args.castlings:
        render.render_all_castling_scenes()

    if args.all or args.en_passant:
        render.render_all_en_passant_scenes()

    if args.all or args.rush:
        render.render_all_rush_scenes()

    if args.isa: # Intentionally skipped on args.all.
        bts = get_board_types(args.isa)
        render.render_ISAs(do_centaur=args.isa_centaur, do_patterns=args.isa_patterns, board_types=bts, enforce_cot_in_bw=True)

    if args.tests or args.recent_tests:
        render.render_tests(do_all_tests=args.tests)

    print
    print( "Finished all renderings." )
    print


if __name__ == "__main__":
    main()
