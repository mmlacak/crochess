#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import in_range
import gen_steps as GS

from piece import PieceType
from board import BoardType, Board
from board_view import BoardView
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneCroatianTiesMixin:

    def scn_ct_01_pegasus_initial(self, bt=BoardType.CroatianTies):
        # move_pegasus_initial

        scene = Scene('scn_ct_01_pegasus_initial', bt, x=1.0, y=0.0, width=5, height=5)

        start = (3, 2)
        scene.board.set_piece(*start, piece=PieceType.Pegasus)

        gen_abs_pos = GS.gen_multi_steps(GS.DEFAULT_KNIGHT_MULTI_REL_MOVES, start=start, bounds=scene.board_view.get_position_limits())

        for i, pos in enumerate( gen_abs_pos() ):
            scene.append_field_marker(*pos)
            scene.append_text(str(i+1), *pos, corner=Corner.UpperLeftFieldMarker)

        return scene

    def scn_ct_02_pegasus_direction(self, bt=BoardType.CroatianTies):
        # move_pegasus_direction

        scene = Scene('scn_ct_02_pegasus_direction', bt)
        rect = (0.05, 0.75, 0.7, 0.1)

        start = (2, 1)
        scene.board.set_piece(*start, piece=PieceType.Pegasus)

        # main direction, i.e. <1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())
        scene.append_text("4", *coords())

        # direction 2a, i.e. <2, 1>
        scene.append_arrow(4, 5, 6, 6, mark_type=MarkType.Illegal)
        scene.append_text("2a", 6, 6, corner=Corner.UpperRight, mark_type=MarkType.Illegal, rect=rect)

        # direction 2b, i.e. <2, -1>
        scene.append_arrow(4, 5, 6, 4, mark_type=MarkType.Illegal)
        scene.append_text("2b", 6, 4, corner=Corner.UpperRight, mark_type=MarkType.Illegal, rect=rect)

        # direction 2c, i.e. <1, -2>
        scene.append_arrow(4, 5, 5, 3, mark_type=MarkType.Illegal)
        scene.append_text("2c", 5, 3, corner=Corner.UpperRight, mark_type=MarkType.Illegal, rect=rect)

        # direction 2d, i.e. <-2, -1>
        scene.append_arrow(4, 5, 2, 4, mark_type=MarkType.Illegal)
        scene.append_text("2d", 2, 4, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)

        # direction 2e, i.e. <-2, 1>
        scene.append_arrow(4, 5, 2, 6, mark_type=MarkType.Illegal)
        scene.append_text("2e", 2, 6, corner=Corner.UpperLeft, mark_type=MarkType.Illegal)

        # direction 2f, i.e. <-1, 2>
        scene.append_arrow(4, 5, 3, 7, mark_type=MarkType.Illegal)
        scene.append_text("2f", 3, 7, corner=Corner.UpperRight, mark_type=MarkType.Illegal, rect=rect)

        return scene

    def scn_ct_03_define_step_ply(self, bt=BoardType.CroatianTies):
        # move_pegasus_step_ply

        scene = Scene('scn_ct_03_define_step_ply', bt)

        start = (2, 1)
        scene.board.set_piece(*start, piece=PieceType.Pegasus)

        # direction 1, i.e. <2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(2, 1), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # main direction, i.e. <1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ]) )
        scene.append_text("1", *coords())
        scene.append_text("2", *coords())
        scene.append_text("3", *coords())

        # direction 3, i.e. <-1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 2), ], include_prev=True) )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        # direction 4, i.e. <-2, 1>
        scene.append_arrow(2, 1, 0, 2, mark_type=MarkType.Blocked )

        # direction 5, i.e. <-2, -1>
        scene.append_arrow(2, 1, 0, 0, mark_type=MarkType.Blocked )

        # direction 6, i.e. <2, -1>
        scene.append_arrow(2, 1, 4, 0, mark_type=MarkType.Blocked )

        return scene

    def scn_ct_04_pegasus_movement(self, bt=BoardType.CroatianTies):
        # move_pegasus

        scene = Scene('scn_ct_04_pegasus_movement', bt)

        start = (2, 1)
        scene.board.set_piece(*start, piece=PieceType.Pegasus)

        scene.board.set_piece(5, 7, piece=PieceType.Pawn)
        scene.board.set_piece(6, 3, piece=-PieceType.Pawn)

        scene.board.set_piece(3, 4, piece=-PieceType.Rook)
        scene.board.set_piece(4, 4, piece=-PieceType.Rook)

        scene.board.set_piece(5, 2, piece=PieceType.Rook)
        scene.board.set_piece(5, 3, piece=PieceType.Rook)

        # direction 1, i.e. <2, 1>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(2, 1), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Action )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(2, 1), ]) )
        scene.append_text("1", *coords())
        scene.append_text("1", *coords(), mark_type=MarkType.Action )
        scene.append_text("1", *coords(), mark_type=MarkType.Blocked)

        # direction 2, i.e. <1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )
        scene.append_arrow( *coords(), mark_type=MarkType.Blocked )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(1, 2), ]) )
        scene.append_text("2", *coords())
        scene.append_text("2", *coords())
        scene.append_text("2", *coords(), mark_type=MarkType.Blocked)
        scene.append_text("2", *coords(), mark_type=MarkType.Blocked)

        # direction 3, i.e. <-1, 2>
        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 2), ], include_prev=True) )
        scene.append_arrow( *coords() )
        scene.append_arrow( *coords() )

        coords = GS.gen_next( GS.gen_steps(start=start, rels=[(-1, 2), ]) )
        scene.append_text("3", *coords(), corner=Corner.UpperRight)
        scene.append_text("3", *coords(), corner=Corner.UpperRight)

        # direction 4, i.e. <-2, 1>
        scene.append_arrow(2, 1, 0, 2)
        scene.append_text("4", 0, 2, corner=Corner.UpperRight)

        # direction 5, i.e. <-2, -1>
        scene.append_arrow(2, 1, 0, 0)
        scene.append_text("5", 0, 0)

        # direction 6, i.e. <2, -1>
        scene.append_arrow(2, 1, 4, 0)
        scene.append_text("6", 4, 0, corner=Corner.UpperRight)

        return scene
