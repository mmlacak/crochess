.. Copyright (c) 2023, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccsetupmisc:

Setup misc
==========

Documents ``cc_setup_misc.h`` and ``cc_setup_misc.c`` files, which contain
miscellaneous setup definitions and functions.

.. _lbl-libcc-ccsetupmisc-data:

Data
----

.. c:macro:: CC_KING_MIN_CASTLING_DISTANCE

    Minimum distance King can travel when castling, equals to ``2``.

    Does not depend on variant being played.

.. _lbl-libcc-ccsetupmisc-functions:

Functions
---------

.. c:function:: int cc_find_initial_figure_file( CcVariantType ve, CcPieceTagType pe, bool search_queen_side_first )

    Function returns file of a figure in an initial setup of a chessboard,
    for a given variant.

    Function returns valid results only for figures in first (or last) row
    in an initial setup of chessboard.

    Pawns, Scouts, Grenadiers, and Monoliths are not searched for, they'll
    always return invalid value (:c:macro:`CC_INVALID_COORD`).

    :param ve: A variant.
    :param pe: A figure.
    :param search_queen_side_first: Flag, whether to search Queen-side, or
        King-side first.
    :returns: Initial file of figure if found,
        :c:macro:`CC_INVALID_COORD` otherwise.

.. c:function:: int cc_get_kings_max_castling_distance( CcVariantType ve )

    Function returns maximum castling distance Kings can make in a
    given variant.

    :param ve: A variant.
    :returns: Maximum Kng's castling distance if valid variant is given,
        :c:macro:`CC_INVALID_COORD` otherwise.

.. c:function:: bool cc_check_pos_is_king_castling_step( CcVariantType ve, CcPieceTagType king, int pos_i, int pos_j, bool * is_queen_side__o, int * min_i__o, int * max_i__o )

    Function checks if position is valid step-field for castling King.

    :param ve: A variant.
    :param king: Piece, either light, or dark King.
    :param pos_i: Castling destination; file, position along horizontal axis.
    :param pos_j: Castling destination; rank, position along vertical axis.
    :param is_queen_side__o: *Output*, flag, whether castling is on Queen-,
        or King-side.
    :param min_i__o: *Output*, lower bound on King's castling file.
    :param max_i__o: *Output*, upper bound on King's castling file.
    :returns: :c:data:`true` if position is valid step-field for castling King,
        :c:data:`false` otherwise.

.. _lbl-libcc-ccsetupmisc-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_setup_misc.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_setup_misc.h
    :language: C
    :linenos:

.. _lbl-libcc-ccsetupmisc-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_setup_misc.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_setup_misc.c
    :language: C
    :linenos:
