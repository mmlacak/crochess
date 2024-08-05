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

Position utility functions
--------------------------

.. c:function:: CcPosDesc cc_convert_pos_to_pos_desc( CcChessboard * cb, CcPos pos, cc_uint_t momentum )

    Function converts position to position descriptor (i.e. the one
    containing piece, and tag at that location on a chessboard).

    If chessboard is not given, piece and tag members are not updated,
    returned value still contains a given position.

    :param cb: A chessboard.
    :param pos: A position.
    :param momentum: Momentum.
    :returns: Position descriptor.

.. todo::

    Document all other functions.

.. _lbl-libcc-ccposutils-sourcecodeheader:

Position utilities source code header
-------------------------------------

Included source code file is ``cc_pos_utils.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_pos_utils.h
    :language: C
    :linenos:

.. _lbl-libcc-ccposutils-sourcecodefile:

Position utilities source code file
-----------------------------------

Included source code file is ``cc_pos_utils.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_pos_utils.c
    :language: C
    :linenos:
