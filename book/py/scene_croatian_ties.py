#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2018 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


from util import in_range
# from gen_steps import DEFAULT_KNIGHT_REL_MOVES, , gen_steps, gen_steps, get_gen_multi_steps
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from scene import Corner, Scene


class SceneCroatianTiesMixin(Scene):

    def scn_ct_01_pegasus_initial(self, bt=BoardType.CroatianTies):
        # move_pegasus_initial

        self.init_scene(bt, width=5, height=5)

        start = (2, 2)
        self.board.set_piece(*start, piece=PieceType.Pegasus)

# TODO :: FIX ME !!!
#         gen_abs_pos = get_gen_multi_steps(start=start, rel_lst=GS.DEFAULT_KNIGHT_REL_MOVES, pos_bounds=self.board.get_position_limits())
        gen_abs_pos = GS.gen_multi_steps( [ ( GS.DEFAULT_KNIGHT_REL_MOVES, self.board.get_position_limits(), None ), ], start=start)

        i = 1
        for pos in gen_abs_pos():
            self.append_field_marker(*pos)
            self.append_text(str(i), *pos, corner=Corner.UpperLeft, rect=(0.15, 1.0, 0.7, 0.45))
            i += 1

        return 'scn_ct_01_pegasus_initial'

    def scn_ct_02_pegasus_direction(self, bt=BoardType.CroatianTies):
        # move_pegasus_direction

        self.init_scene(bt)

        start = (2, 1)
        self.board.set_piece(*start, piece=PieceType.Pegasus)

        # main direction, i.e. <1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())
        self.append_text("4", *coords())

        # direction 2a, i.e. <2, 1>
        self.append_arrow(4, 5, 6, 6, mark_type=MarkType.Blocked)
        self.append_text("2a", 6, 6, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        # direction 2b, i.e. <2, -1>
        self.append_arrow(4, 5, 6, 4, mark_type=MarkType.Blocked)
        self.append_text("2b", 6, 4, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        # direction 2c, i.e. <1, -2>
        self.append_arrow(4, 5, 5, 3, mark_type=MarkType.Blocked)
        self.append_text("2c", 5, 3, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        # direction 2d, i.e. <-2, -1>
        self.append_arrow(4, 5, 2, 4, mark_type=MarkType.Blocked)
        self.append_text("2d", 2, 4, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        # direction 2e, i.e. <-2, 1>
        self.append_arrow(4, 5, 2, 6, mark_type=MarkType.Blocked)
        self.append_text("2e", 2, 6, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        # direction 2f, i.e. <-1, 2>
        self.append_arrow(4, 5, 3, 7, mark_type=MarkType.Blocked)
        self.append_text("2f", 3, 7, corner=Corner.UpperRight, mark_type=MarkType.Blocked, rect=(0.05, 1.0, 0.6, 0.45))

        return 'scn_ct_02_pegasus_direction'

    def scn_ct_03_define_step_ply(self, bt=BoardType.CroatianTies):
        # move_pegasus_step_ply

        self.init_scene(bt)

        start = (2, 1)
        self.board.set_piece(*start, piece=PieceType.Pegasus)

        # direction 1, i.e. <2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(2, 1), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # main direction, i.e. <1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ]) )
        self.append_text("1", *coords())
        self.append_text("2", *coords())
        self.append_text("3", *coords())

        # direction 3, i.e. <-1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 2), ], include_prev=True) )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction 4, i.e. <-2, 1>
        self.append_arrow(2, 1, 0, 2, mark_type=MarkType.Blocked )

        # direction 5, i.e. <-2, -1>
        self.append_arrow(2, 1, 0, 0, mark_type=MarkType.Blocked )

        # direction 6, i.e. <2, -1>
        self.append_arrow(2, 1, 4, 0, mark_type=MarkType.Blocked )

        return 'scn_ct_03_define_step_ply'

    def scn_ct_04_pegasus_movement(self, bt=BoardType.CroatianTies):
        # move_pegasus

        self.init_scene(bt)

        start = (2, 1)
        self.board.set_piece(*start, piece=PieceType.Pegasus)

        self.board.set_piece(5, 7, piece=PieceType.Pawn)
        self.board.set_piece(6, 3, piece=-PieceType.Pawn)

        self.board.set_piece(3, 4, piece=-PieceType.Rook)
        self.board.set_piece(4, 4, piece=-PieceType.Rook)

        self.board.set_piece(5, 2, piece=PieceType.Rook)
        self.board.set_piece(5, 3, piece=PieceType.Rook)

        # direction 1, i.e. <2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(2, 1), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(2, 1), ]) )
        self.append_text("1", *coords())
        self.append_text("1", *coords())
        self.append_text("1", *coords(), mark_type=MarkType.Blocked)

        # direction 2, i.e. <1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )
        self.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ]) )
        self.append_text("2", *coords())
        self.append_text("2", *coords())
        self.append_text("2", *coords(), mark_type=MarkType.Blocked)
        self.append_text("2", *coords(), mark_type=MarkType.Blocked)

        # direction 3, i.e. <-1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 2), ], include_prev=True) )
        self.append_arrow( *coords() )
        self.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 2), ]) )
        self.append_text("3", *coords(), corner=Corner.UpperRight)
        self.append_text("3", *coords(), corner=Corner.UpperRight)

        # direction 4, i.e. <-2, 1>
        self.append_arrow(2, 1, 0, 2)
        self.append_text("4", 0, 2, corner=Corner.UpperRight)

        # direction 5, i.e. <-2, -1>
        self.append_arrow(2, 1, 0, 0)
        self.append_text("5", 0, 0)

        # direction 6, i.e. <2, -1>
        self.append_arrow(2, 1, 4, 0)
        self.append_text("6", 4, 0, corner=Corner.UpperRight)

        return 'scn_ct_04_pegasus_movement'
