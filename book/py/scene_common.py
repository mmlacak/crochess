#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


from utils import in_range, iterate
from piece import PieceType
from board import BoardType, Board
from mark import MarkType
from corner import Corner
from scene import Scene


class SceneCommon:

    def intro_piece(self, bt, piece_type=None):
        bt = BoardType(bt)
        scene = Scene('intro_piece', bt, width=2, height=2)

        pts = piece_type or bt.get_newly_introduced_pieces()

        for pt in iterate( pts ):
            scene.board.set_pieces( [ ( 0, 0, PieceType(pt) ),
                                      ( 1, 0, PieceType(pt) ),
                                      ( 0, 1, PieceType(-pt) ),
                                      ( 1, 1, PieceType(-pt) ) ] )

        return scene

    def intro_board(self, bt):
        bt = BoardType(bt)
        scene = Scene('intro_board', bt)

        scene.board.setup()

        return scene

    def intro_castling(self, bt, move_king=0):
        assert isinstance(move_king, int)

        bt = BoardType(bt)

        offset = 1 if bt.does_contain(PieceType.Star) else 0

        pos_king_init = bt.get_size() // 2
        pos_rook_l_init = offset
        pos_rook_r_init = bt.get_size() - 1 - offset

        diff_min, diff_max = Board.get_castling_limits(bt)
        assert (move_king == 0) or in_range(abs(move_king), diff_min, diff_max)

        scene = Scene('intro_castling', bt, width=bt.get_size(), height=1.3)

        king_moved = (move_king != 0)
        king_moved_left = (move_king < 0)
        king_moved_right = (move_king > 0)

        pos_king = pos_king_init + move_king
        pos_rook_l = pos_king + 1 if king_moved_left else pos_rook_l_init
        pos_rook_r = pos_king - 1 if king_moved_right else pos_rook_r_init

        scene.board.set_piece(pos_king, 0, PieceType.King)
        scene.board.set_piece(pos_rook_l, 0, PieceType.Rook)
        scene.board.set_piece(pos_rook_r, 0, PieceType.Rook)

        if bt.does_contain(PieceType.Star):
            scene.board.set_piece(0, 0, PieceType.Star)
            scene.board.set_piece(bt.get_size() - 1, 0, -PieceType.Star)

        if king_moved:
            scene.append_text("K", pos_king_init, 0, corner=Corner.UpperLeft, mark_type=MarkType.Blocked)

        mt = MarkType.Blocked if king_moved else MarkType.Legal

        for i in range(diff_min, diff_max+1):
            # diff_max + 1, because upper boundary is not included

            pos_l = pos_king_init - i
            pos_r = pos_king_init + i

            scene.append_text(str(i-1), pos_l, 0, corner=Corner.UpperLeft, mark_type=mt)
            scene.append_text(str(i-1), pos_r, 0, corner=Corner.UpperLeft, mark_type=mt)

        return scene

    def intro_en_passant(self, bt):
        bt = BoardType(bt)
        is_classic = BoardType.is_classical( bt, include_old=True, include_new=True )

        width = 3.3 if is_classic else \
                None if bt > BoardType.Nineteen else \
                7.2 if bt > BoardType.MirandasVeil else \
                3.3
        rect = (0.15, 0.55, 0.5, 0.05)

        size = (bt.get_size() + 1) // 2
        height = size + 0.3 if is_classic else \
                 None if bt > BoardType.Nineteen else \
                 size + 0.3
        scene = Scene('intro_en_passant', bt, width=width, height=height)

        if bt > BoardType.MirandasVeil and not is_classic:
            scene.board.set_piece(1, 0, PieceType(PieceType.Rook))
            scene.board.set_piece(1, 1, PieceType(PieceType.Pawn))
            scene.board.set_piece(1, 2, PieceType(PieceType.Pawn))
            scene.append_text("A", 1, 2, corner=Corner.UpperLeft, rect=rect)

            scene.board.set_piece(4, 0, PieceType(PieceType.Pegasus))
            scene.board.set_piece(4, 1, PieceType(PieceType.Pawn))
            scene.append_text("B", 4, 1, corner=Corner.UpperLeft, rect=rect)

            scene.append_arrow(1, 2, 1, 3, mark_type=MarkType.Legal) # Pawn A
            scene.append_arrow(4, 1, 4, 2, mark_type=MarkType.Legal) # Pawn B

            for i in range(4, size):
                scene.append_arrow(1, i-1, 1, i, mark_type=MarkType.Legal)
                scene.append_text(str(i-3), 1, i, corner=Corner.UpperLeft, rect=rect)

            offset = ( size - 4 ) // 2
            j = 4 + offset
            scene.board.set_piece(2, j, PieceType(-PieceType.Pawn))
            scene.append_arrow(2, j, 1, j-1, mark_type=MarkType.Action)

            for i in range(3, size):
                scene.append_arrow(4, i-1, 4, i, mark_type=MarkType.Legal)
                scene.append_text(str(i-2), 4, i, corner=Corner.UpperLeft, rect=rect)

            offset = ( size - 3 ) // 2
            j = 3 + offset
            scene.board.set_piece(5, j, PieceType(-PieceType.Pawn))
            scene.append_arrow(5, j, 4, j-1, mark_type=MarkType.Action)

            if bt > BoardType.Nineteen:
                scene.board.set_piece(9, 0, PieceType(PieceType.Centaur))
                scene.board.set_piece(8, 4, PieceType(PieceType.Scout))
                scene.board.set_piece(11, 3, PieceType(PieceType.Scout))
                scene.append_text("C", 8, 4, corner=Corner.UpperLeft, rect=rect)
                scene.append_text("D", 11, 3, corner=Corner.UpperLeft, rect=rect)

                scene.append_arrow(8, 4, 8, 5, mark_type=MarkType.Legal) # Scout C
                scene.append_arrow(11, 3, 11, 4, mark_type=MarkType.Legal) # Scout D

                for i in range(6, size):
                    scene.append_arrow(8, i-1, 8, i, mark_type=MarkType.Legal)
                    scene.append_text(str(i-5), 8, i, corner=Corner.UpperLeft, rect=rect)

                offset = ( size - 6 ) // 2
                j = 6 + offset
                scene.board.set_piece(9, j, PieceType(-PieceType.Pawn))
                scene.append_arrow(9, j, 8, j-1, mark_type=MarkType.Action)

                for i in range(5, size):
                    scene.append_arrow(11, i-1, 11, i, mark_type=MarkType.Legal)
                    scene.append_text(str(i-4), 11, i, corner=Corner.UpperLeft, rect=rect)

                offset = ( size - 5 ) // 2
                j = 5 + offset
                scene.board.set_piece(12, j, PieceType(-PieceType.Pawn))
                scene.append_arrow(12, j, 11, j-1, mark_type=MarkType.Action)

                scene.board.set_piece(16, 0, PieceType(PieceType.Shaman))
                scene.board.set_piece(15, 1, PieceType(PieceType.Grenadier))
                scene.board.set_piece(15, 2, PieceType(PieceType.Grenadier))
                scene.board.set_piece(18, 1, PieceType(PieceType.Grenadier))
                scene.append_text("E", 15, 2, corner=Corner.UpperLeft, rect=rect)
                scene.append_text("F", 18, 1, corner=Corner.UpperLeft, rect=rect)

                scene.append_arrow(15, 2, 15, 3, mark_type=MarkType.Legal) # Grenadier E
                scene.append_arrow(18, 1, 18, 2, mark_type=MarkType.Legal) # Grenadier F

                for i in range(4, size):
                    scene.append_arrow(15, i-1, 15, i, mark_type=MarkType.Legal)
                    scene.append_text(str(i-3), 15, i, corner=Corner.UpperLeft, rect=rect)

                offset = ( size - 4 ) // 2
                j = 4 + offset
                scene.board.set_piece(16, j, PieceType(-PieceType.Pawn))
                scene.append_arrow(16, j, 15, j-1, mark_type=MarkType.Action)

                for i in range(3, size):
                    scene.append_arrow(18, i-1, 18, i, mark_type=MarkType.Legal)
                    scene.append_text(str(i-2), 18, i, corner=Corner.UpperLeft, rect=rect)

                offset = ( size - 3 ) // 2
                j = 3 + offset
                scene.board.set_piece(19, j, PieceType(-PieceType.Pawn))
                scene.append_arrow(19, j, 18, j-1, mark_type=MarkType.Action)

        else:
            scene.board.set_piece(1, 0, PieceType(PieceType.Knight))
            scene.board.set_piece(1, 1, PieceType(PieceType.Pawn))

            scene.append_arrow(1, 1, 1, 2, mark_type=MarkType.Legal) # Pawn

            for i in range(3, size):
                scene.append_arrow(1, i-1, 1, i, mark_type=MarkType.Legal)
                scene.append_text(str(i-2), 1, i, corner=Corner.UpperLeft, rect=rect)

            offset = ( size - 3 ) // 2
            j = 3 + offset
            scene.board.set_piece(2, j, PieceType(-PieceType.Pawn))
            scene.append_arrow(2, j, 1, j-1, mark_type=MarkType.Action)

        return scene

    def intro_rush(self, bt):
        bt = BoardType(bt)
        rect = (0.15, 0.55, 0.5, 0.05)

        size = (bt.get_size() + 1) // 2
        scene = Scene('intro_rush', bt, width=3.3, height=size + 0.3)

        scene.board.set_piece(1, 0, PieceType(PieceType.Knight))
        scene.board.set_piece(1, 1, PieceType(PieceType.Pawn))

        for i in range(2, size):
            scene.append_arrow(1, i-1, 1, i)

            if i > 2:
                scene.append_text(str(i-2), 1, i, corner=Corner.UpperLeft, rect=rect)

        return scene
