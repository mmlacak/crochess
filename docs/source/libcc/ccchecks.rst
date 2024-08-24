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

.. c:function:: CcMaybeBoolEnum cc_check_step_fields_are_empty( CcChessboard * cb, CcPos pos, CcPos step, cc_uint_t limit__d, bool check_pos )

    .. todo:: check transparency, or add specialized function for castling (?)

    Function checks if step-fields are empty.

    Step-fields are calculated by repeatedly adding :c:`step` to :c:`pos`.

    :param cb: Chessboard.
    :param pos: A position.
    :param step: A step.
    :param limit__d: *Optional*, count of steps to check, :c:macro:`CC_CHECK_STEPS_NO_LIMIT` otherwise.
    :param check_pos: Flag, whether given position should also be checked.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if fields are empty,
        * :c:enumerator:`CC_MBE_False` if at least one field is not empty,
        * :c:enumerator:`CC_MBE_Void` if no fields were checked (an error encountered, insufficient data given).

.. c:function:: bool cc_check_momentum_for_movement( CcPieceType piece, cc_uint_t momentum )

    Function checks if a piece has enough momentum for movement, or doesn't
    need it at all (i.e. Wave, Starchild).

    :param piece: A piece.
    :param momentum: Momentum a :c:`piece` has.
    :returns: :c:data:`true` if piece can move, :c:data:`false` otherwise.

.. c:function:: bool cc_check_losing_tag_for_piece( CcPieceType piece, CcLosingTagEnum lte )

    Function checks if a piece can lose given tag.

    :param piece: A piece.
    :param lte: :c:enum:`CcLosingTagEnum` value.
    :returns: :c:data:`true` if piece can lose given tag, :c:data:`false` otherwise.

.. c:function:: CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb, CcPieceType piece, cc_uint_t momentum, CcPos pos )

    Function checks if piece is blocked at given position.

    :param cb: Chessboard.
    :param piece: A piece.
    :param momentum: Momentum.
    :param pos: A position.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if piece is blocked at given position,
        * :c:enumerator:`CC_MBE_False` if piece is not blocked,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb, CcPieceType piece, cc_uint_t momentum, CcPos pos )

    Function checks if a piece can capture at given position.

    :param cb: Chessboard.
    :param piece: A piece.
    :param momentum: Momentum.
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
