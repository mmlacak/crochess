#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from corner import Corner
from scene import Scene
from scene_mixin import SceneMixin

import gen_steps as GS


class SceneIsa(SceneMixin):

    # overrides
    def _get_recent_scene_method_names(self):
        return  [
                    'isa_one', \
                ]

    def setup_board(self, bt, name):
        bt = BoardType(bt)
        scene = Scene(name, bt)

        scene.board.setup()
        return scene

    def find_piece(self, scene, piece_type, search_light=True, search_queen_side=True):
        pos_0, pos_max = scene.board.get_position_limits() # ((0, 0), (w, h))

        # pos +1 and -1, because range() does not include end limit into iteration.
        i_start, i_end, i_diff = (pos_0[0], pos_max[0]+1, 1) if search_queen_side else (pos_max[0], pos_0[0]-1, -1)
        j = pos_0[1] if search_light else pos_max[1]

        pt = PieceType(piece_type)
        pt = pt.get_light() if search_light else pt.get_dark()

        for i in range(i_start, i_end, i_diff):
            if scene.board.get_piece(i, j) == pt:
                return pt, i, j

        return None, None, None

    def check_field(self, scene, piece_type, i, j):
        pt = PieceType(piece_type)
        other = scene.board.get_piece(i, j)

        if pt.is_friend(other):
            return True
        elif pt.is_foe(other):
            return False
        else:
            return None # empty field

    def traverse_pegasus_dir(self, scene, piece_type, i, j):
        assert piece_type in [-PieceType.Pegasus, PieceType.Pegasus]

        pt = PieceType(piece_type)
        start = (i, j)

        for index, rel in enumerate( GS.DEFAULT_KNIGHT_REL_MOVES ):
            current = start
            while scene.board.is_on_board(*current):
                next_ = GS.add(rel, current)

                check = self.check_field(scene, pt, *next_)
                if check is True:
                    # own piece encountered
                    break
                elif check is False:
                    # opponent's piece encountered
                    for i, r in enumerate( GS.DEFAULT_KNIGHT_REL_MOVES ):
                        c = GS.add(r, current)
                        scene.append_text(str(i+1), *c, mark_type=MarkType.Legal)

                    scene.append_arrow( *(current + next_), mark_type=MarkType.Action )

                    for i, r in enumerate( GS.DEFAULT_KNIGHT_REL_MOVES ):
                        c = GS.add(r, next_)
                        scene.append_field_marker(*c, mark_type=MarkType.Action)

                    break
                else:
                    # empty field
                    if  scene.board.is_on_board(*next_):
                        scene.append_arrow( *(current + next_), mark_type=MarkType.Legal )

                current = next_

        return scene

    def traverse_shaman_dir(self, scene, piece_type, i, j):
        assert piece_type in [-PieceType.Shaman, PieceType.Shaman]

        pt = PieceType(piece_type)
        start = (i, j)
        rel_moves = GS.DEFAULT_KNIGHT_REL_MOVES if pt.is_light() else GS.DEFAULT_UNICORN_REL_LONG_MOVES
        rel_capture = GS.DEFAULT_UNICORN_REL_LONG_MOVES if pt.is_light() else GS.DEFAULT_KNIGHT_REL_MOVES

        for index, rel in enumerate( rel_moves ):
            previous = current = start
            while scene.board.is_on_board(*current):
                next_ = GS.add(rel, current)

                check = self.check_field(scene, pt, *next_)
                if check is True:
                    # own piece encountered
                    break
                elif check is False:
                    # opponent's piece encountered
                    for i, r in enumerate( rel_capture ):
                        c = GS.add(r, previous)
                        scene.append_text(str(i+1), *c, mark_type=MarkType.Legal)

                    for i, r in enumerate( rel_capture ):
                        c = GS.add(r, current)
                        scene.append_field_marker(*c, mark_type=MarkType.Action)

                    break
                else:
                    # empty field
                    if  scene.board.is_on_board(*next_):
                        scene.append_arrow( *(current + next_), mark_type=MarkType.Legal )
                    elif (pt.is_dark() and current[1] < 5) or (pt.is_light() and current[1] > scene.board.get_height() - 5):
                        for i, r in enumerate( rel_capture ):
                            c = GS.add(r, previous)
                            scene.append_text(str(i+1), *c, mark_type=MarkType.Legal)

                        for i, r in enumerate( rel_capture ):
                            c = GS.add(r, current)
                            scene.append_field_marker(*c, mark_type=MarkType.Action)

                previous = current
                current = next_

        return scene

    def traverse_centaur_dir(self, scene, piece_type, i, j):
        assert piece_type in [-PieceType.Centaur, PieceType.Centaur]

        pt = PieceType(piece_type)
        start = (i, j)


        return scene

    def get_traverse_func(self, piece_type):
        dct =   {
                    PieceType.Pegasus: self.traverse_pegasus_dir, \
                    -PieceType.Pegasus: self.traverse_pegasus_dir, \
                    PieceType.Shaman: self.traverse_shaman_dir, \
                    -PieceType.Shaman: self.traverse_shaman_dir, \
                    PieceType.Centaur: self.traverse_centaur_dir, \
                    -PieceType.Centaur: self.traverse_centaur_dir, \
                }
        if piece_type in dct:
            return dct[ piece_type ]
        else:
            return None

    def isa_one(self):
        for index, bt in enumerate( BoardType.iter(include_odd=True) ):
            for pt in [PieceType.Pegasus, PieceType.Shaman, ]:
                for sl in [True, False]:
                    for sqs in [True, False]:
                        scene = self.setup_board(bt, 'isa')

                        pos_G = self.find_piece(scene, pt, search_light=sl, search_queen_side=sqs)
                        if pos_G != (None, None, None):
                            print(pos_G)

                            scene.file_name = 'isa_%s_%02d_%s' % (bt.get_symbol().lower(), index, PieceType(pos_G[0]).get_label())
                            self.get_traverse_func(pt)(scene, *pos_G)

                            yield scene
