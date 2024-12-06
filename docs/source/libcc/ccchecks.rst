.. Copyright (c) 2023, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccchecks:

Checks
======

Documents ``cc_checks.h`` and ``cc_checks.c`` files, which contain various
checks.

.. _lbl-libcc-ccchecks-data:

Data
----

.. c:macro:: CC_CHECK_STEPS_NO_LIMIT

    Macro constant for check steps function(s), represents no limit on how many
    steps to check; equals to ``0``.

    Calculated positions still has to be on a chessboard.

.. _lbl-libcc-ccchecks-functions:

Functions
---------

.. c:function:: CcMaybeBoolEnum cc_check_momentum_for_next_step( CcPieceType piece, CcTagType tag, cc_uint_t * momentum__io )

    Function calculates momentum for next step, returns if there is enough of
    it to actually make the step.

    :param piece: A piece.
    :param tag: A tag piece has.
    :param momentum__io: *Input/output*; momentum a :c:`piece` has.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if there is enough momentum for next step,
        * :c:enumerator:`CC_MBE_False` if there is not enough momentum,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

    :seealso: :c:func:`cc_calc_momentum_for_next_step()`

.. c:function:: bool cc_check_piece_can_lose_tag( CcPieceType piece, CcLosingTagType ltt )

    Function checks if a piece can lose given tag.

    :param piece: A piece.
    :param ltt: :c:type:`CcLosingTagType` value.
    :returns: :c:data:`true` if piece can lose given tag, :c:data:`false` otherwise.

.. c:function:: CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb, CcPieceType piece, CcPos pos )

    Function checks if piece is blocked at given position.

    :param cb: Chessboard.
    :param piece: A piece.
    :param pos: A position.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if piece is blocked at given position,
        * :c:enumerator:`CC_MBE_False` if piece is not blocked,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb, CcPieceType piece, CcPos pos )

    Function checks if a piece can capture at given position.

    :param cb: Chessboard.
    :param piece: A piece.
    :param pos: A position.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if a piece can capture at given position,
        * :c:enumerator:`CC_MBE_False` if no capture is possible,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb, CcPieceType piece, cc_uint_t momentum, CcPieceType activator, CcPos pos )

    Function checks if a piece can diverge from given position.

    :param cb: Chessboard.
    :param piece: A piece.
    :param momentum: Momentum.
    :param activator: An :term:`activator`.
    :param pos: A position.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if a piece can diverge from given position,
        * :c:enumerator:`CC_MBE_False` if no divergence is possible,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcMaybeBoolEnum cc_check_castling_step_fields( CcChessboard * cb, CcPos king_start, CcPos king_dest, CcPos rook_start, CcPos rook_dest )

    Function checks if pieces can castle from their given positions.

    :param cb: Chessboard.
    :param king_start: King's initial position.
    :param king_dest: King's destination field after castling.
    :param rook_start: Rook's initial position.
    :param rook_dest: Rook's destination field after castling.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if pieces can castle from given position,
        * :c:enumerator:`CC_MBE_False` if no castling is possible,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. _lbl-libcc-ccchecks-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_checks.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_checks.h
    :language: C
    :linenos:

.. _lbl-libcc-ccchecks-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_checks.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_checks.c
    :language: C
    :linenos:
