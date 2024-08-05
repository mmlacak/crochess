.. Copyright (c) 2023, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccchecks:

Checks
======

Documents ``cc_checks.h`` and ``cc_checks.c`` files, which contain various
checks.

.. _lbl-libcc-ccchecks-functions:

Check functions
---------------

.. c:function:: CcMaybeBoolEnum cc_check_piece_is_blocked_at( CcChessboard * cb, cc_piece piece, cc_uint momentum, CcPos pos )

    Function checks if piece is blocked at given position.

    :param cb: Chessboard.
    :param piece: A piece.
    :param momentum: Momentum.
    :param pos: A position.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if piece is blocked at given position,
        * :c:enumerator:`CC_MBE_False` if piece is not blocked,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcMaybeBoolEnum cc_check_piece_can_capture_at( CcChessboard * cb, cc_piece piece, cc_uint momentum, CcPos pos )

    Function checks if a piece can capture at given position.

    :param cb: Chessboard.
    :param piece: A piece.
    :param momentum: Momentum.
    :param pos: A position.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if a piece can capture at given position,
        * :c:enumerator:`CC_MBE_False` if no capture is possible,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcMaybeBoolEnum cc_check_piece_can_diverge_at( CcChessboard * cb, cc_piece piece, cc_uint momentum, cc_piece activator, CcPos pos )

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

Checks source code header
-------------------------

Included source code file is ``cc_checks.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_checks.h
    :language: C
    :linenos:

.. _lbl-libcc-ccchecks-sourcecodefile:

Checks source code file
-----------------------

Included source code file is ``cc_checks.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_checks.c
    :language: C
    :linenos:
