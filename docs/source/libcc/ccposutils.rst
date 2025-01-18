.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccposutils:

Position utilities
==================

Documents ``cc_pos_utils.h`` and ``cc_pos_utils.c`` files, which contain various
position utilities.

.. _lbl-libcc-ccposutils-functions:

Functions
---------

.. c:function:: CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos )

    Function converts position to position descriptor (i.e. the one
    containing piece, and tag at that location on a chessboard).

    If chessboard is not given, piece and tag members are not updated,
    returned value still contains a given position.

    :param cb: A chessboard.
    :param pos: A position.
    :returns: Position descriptor.

.. c:function:: bool cc_calc_momentum( CcMomentumUsageEnum usage, cc_uint_t count, cc_uint_t * momentum__io )

    Function calculates next momentum value by adding or subtracting :c:var:`count`,
    based on :c:var:`usage` argument; momentum is given, and result is returned via
    *input/output* :c:var:`momentum__io` parameter.

    Function checks if momentum calculation will over- or under-flow before
    actual calculation takes place.

    :param usage: Flag, whether momentum is accumulated, spent, or unchanged
        while piece is moving; :c:enum:`CcMomentumUsageEnum` value.
    :param count: Count of steps.
    :param momentum__io: *Input/output*; momentum.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: bool cc_iter_piece_pos( CcChessboard * cb, CcPos expected__d, CcPieceType piece, bool include_opponent, CcPos * pos__io )

    Function iterates over all positions on a chessboards which contain a given
    piece, optionally also opponent's.

    Expected position can be disambiguation; if given, it filters which position(s)
    are returned.

    Position where a piece was found is returned via *input/output* argument,
    which also serves as a starting position for next iteration.

    Before calling this iterator, set *output* argument to invalid position
    (i.e. :c:data:`CC_POS_CAST_INVALID`), so that function starts searching
    from chessboard origin.

    After all positions has been exhausted, *output* argument is invalidated
    (reset to :c:data:`CC_POS_CAST_INVALID`), and function returns :c:data:`false`.

    Typical usage, in a loop:

    .. code-block:: C
        :force:

        // Storage for returned position.
        CcPos pos = CC_POS_CAST_INVALID;

        // Disambiguation, rank is not given; represents any position on a 'c' file.
        CcPos da = cc_pos( 2, CC_INVALID_COORD );

        while ( cc_iter_piece_pos( cb, da, CC_PE_LightRook, true, &pos ) ) {
            // Do stuff with all the Rooks, light and dark, found at positions ...
        }

    It can also be called separately (here expected position is not given):

    .. code-block:: C
        :force:

        // Storage for returned position.
        CcPos pos = CC_POS_CAST_INVALID;

        if ( !cc_iter_piece_pos( cb, CC_POS_CAST_INVALID, CC_PE_LightKing, false, &pos ) ) {
            // Light King not found --> no good!
        }

        if ( cc_iter_piece_pos( cb, CC_POS_CAST_INVALID, CC_PE_LightKing, false, &pos ) ) {
            // Light King found twice --> also no good!
        }

    :param cb: A chessboard.
    :param expected__d: *Optional*, an expected position, can be disambiguation; disregarded if invalid (i.e. :c:data:`CC_POS_CAST_INVALID`).
    :param piece: A piece to find.
    :param include_opponent: Flag, whether to also search for opponent's pieces
        (if :c:data:`true`), or not (if :c:data:`false`).
    :param pos__io: *Input/output*; a position where piece was found.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-ccposutils-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_pos_utils.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_pos_utils.h
    :language: C
    :linenos:

.. _lbl-libcc-ccposutils-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_pos_utils.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_pos_utils.c
    :language: C
    :linenos:
