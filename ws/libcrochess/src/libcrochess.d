// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


export immutable LIB_VERSION = "0.0.2.2+20210404.002054"; // source-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker


import io = std.stdio;

import pt = piece_type;
import bt = board_type;
import ct = chip_type;
import b = board;


export void test_lib() {
    io.writeln("Hello, library world!");

    pt.PieceType pt1 = pt.PieceType.LightCentaur;
    string s1 = pt.label( pt1 );
    bool b1 = pt.isLight( pt1 );
    io.writefln("Piece: %s %s.", b1, s1);

    bt.BoardType bt1 = bt.from_str( "d" );
    string bts1 = bt.label( bt1 );
    uint bts = bt.size( bt1 );
    io.writefln("Board type: %s, size: %d.", bts1, bts);

    b.Board board1 = new b.Board( bt1 );
    io.writefln("Board[3][2] piece: %d, chip: %d.", board1.getPiece(3, 2), board1.getChip(3, 2));
    board1.setPiece(3, 2, pt.PieceType.LightUnicorn, ct.ChipType.CanCastle);
    io.writefln("Board[3][2] piece: %d, chip: %d.", board1.getPiece(3, 2), board1.getChip(3, 2));

    io.writeln("Bye, library world!");
}
