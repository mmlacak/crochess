#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

import fs
from gfx import GfxRender

import debug_

def main():
    fs.create_subfolders('../gfx')

    render = GfxRender()
    # render.render_all_boards()
    # render.render_all_newly_introduced_pieces()
    render.render_all_example_scenes()
    # render.render_all_castling_scenes()
    # render.render_all_en_passant_scenes()

    print
    print "Finished all renderings."
    print

if __name__ == "__main__":
    main()
