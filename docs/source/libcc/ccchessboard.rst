.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccchessboard:

Chessboard
==========

Documents ``cc_chessboard.h`` and ``cc_chessboard.c`` files, which contain
chessboard definitions and functions.

.. _lbl-libcc-ccchessboard-data:

Chessboard data
---------------

.. c:type:: char const CC_CHESSBOARD_SEPARATORS_SETUP_FROM_STRING[]

    Separators constant, used to tokenize chessboard setup string.

.. _lbl-libcc-ccchessboard-types:

Chessboard types
----------------

.. c:struct:: CcChessboard

    Chessboard :c:`struct`\ure, used for all variants.

    .. c:member:: CcVariantEnum type

        Chess variant to play.

    .. c:member:: uint size

        Actual size of a board used for a given variant.

    .. c:member:: CcPieceEnum board[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ]

        Holds pieces.

    .. c:member:: CcTagEnum tags[ CC_VARIANT_BOARD_SIZE_MAXIMUM ][ CC_VARIANT_BOARD_SIZE_MAXIMUM ]

        Holds tags for pieces at their respective position.

    :c:`CcChessboard` is tagged with the same :c:expr:`CcChessboard` name.

.. _lbl-libcc-ccchessboard-functions:

Chessboard functions
--------------------

.. c:function:: CcChessboard * cc_chessboard__new( CcVariantEnum ve, bool do_setup )

    Function returns a newly allocated chessboard, optionally initialized
    for a given variant.

    :param ve: Variant to play.
    :param do_setup: Whether to set-up pieces to their initial positions.
                     If :c:`false`, chessboard returned is empty.
    :returns: A newly allocated chessboard if successful, :c:`NULL` otherwise.








.. _lbl-libcc-ccchessboard-sourcecodeheader:

Chessboard source code header
-----------------------------

Included source code file is ``cc_chessboard.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_chessboard.h
    :language: C
    :linenos:

.. _lbl-libcc-ccchessboard-sourcecodefile:

Chessboard source code file
---------------------------

Included source code file is ``cc_chessboard.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_chessboard.c
    :language: C
    :linenos:
