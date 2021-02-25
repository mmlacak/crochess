#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


from board import BoardType, Board
from mark import MarkType, Arrow, Text, FieldMarker
from scene import Scene
from def_render import RenderingSizeEnum
from save_scene import SaveScene


class TestColors(SaveScene):

    def save_colors(self, path_prefix=None):
        print
        print( "Test colors on all boards." )

        for bt in BoardType.iter():
            file_path = self.get_board_file_path(bt, path_prefix=path_prefix)
            print( file_path )

            if self.rendering_size.needs_rendering():
                board = Board(bt)
                board.setup()
                scene = Scene(board=board)

                for mt in MarkType.iter():
                    field_i = 2 * (mt - 1)
                    scene.append_field_marker(field_i, 2, mark_type=mt)
                    scene.append_field_marker(field_i, 5, mark_type=mt)

                    pos_i = mt - 1
                    scene.append_text("a", pos_i, 3, mark_type=mt)
                    scene.append_text("a", pos_i, 4, mark_type=mt)

                    start_i = mt + 2
                    end_i = mt + 3
                    scene.append_arrow(start_i, 3, end_i, 3, mark_type=mt)
                    scene.append_arrow(start_i, 4, end_i, 4, mark_type=mt)

                self.save_scene(scene, file_path)

        print( "Finished." )


def test_colors():
    tc = TestColors(RenderingSizeEnum.Draft)
    tc.save_colors(path_prefix='temp/')

if __name__ == '__main__':
    test_colors()
