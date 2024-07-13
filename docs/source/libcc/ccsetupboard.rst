.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccsetupboard:

Setup board
===========

Documents ``cc_setup_board.h`` and ``cc_setup_board.c`` files, which contain
board setup definitions and functions.

.. _lbl-libcc-ccsetupboard-data:

Setup board data
----------------

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_CLASSICAL_CHESS[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ]

    Classical Chess setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_CROATIAN_TIES[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ]

    Croatian Ties setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_MAYAN_ASCENDANCY[ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ][ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ]

    Mayan Ascendancy setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_AGE_OF_AQUARIUS[ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ][ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ]

    Age of Aquarius setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_MIRANDAS_VEIL[ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ][ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ]

    Miranda's Veil setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_NINETEEN[ CC_VARIANT_BOARD_SIZE_NINETEEN ][ CC_VARIANT_BOARD_SIZE_NINETEEN ]

    Nineteen setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_HEMERAS_DAWN[ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ][ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ]

    Hemera's Dawn setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_TAMOANCHAN_REVISITED[ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ][ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ]

    Tamoanchan Revisited setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_CONQUEST_OF_TLALOCAN[ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ]

    Conquest of Tlalocan setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_DISCOVERY[ CC_VARIANT_BOARD_SIZE_DISCOVERY ][ CC_VARIANT_BOARD_SIZE_DISCOVERY ]

    Discovery setup.

.. c:type:: CcPieceEnum const CC_SETUP_BOARD_ONE[ CC_VARIANT_BOARD_SIZE_ONE ][ CC_VARIANT_BOARD_SIZE_ONE ]

    One setup.

.. _lbl-libcc-ccsetupboard-functions:

Setup board functions
---------------------

.. c:function:: CcPieceEnum const * cc_setup_board_get( CcVariantEnum ve )

    Function returning setup for a board, based on a given variant.

    :param ve: A variant.
    :returns: Pointer to setup if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_setup_board_has_piece( CcVariantEnum ve, CcPieceEnum pe )

    Function checks if piece is present in an initial setup of a chessboard
    for a given variant.

    :param ve: A variant.
    :param pe: A piece.
    :returns: :c:data:`true` if piece is found in an initial setup,
              :c:data:`false` otherwise.

.. _lbl-libcc-ccsetupboard-sourcecodeheader:

Setup board source code header
------------------------------

Included source code file is ``cc_setup_board.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_setup_board.h
    :language: C
    :linenos:

.. _lbl-libcc-ccsetupboard-sourcecodefile:

Setup board source code file
----------------------------

Included source code file is ``cc_setup_board.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_setup_board.c
    :language: C
    :linenos:
