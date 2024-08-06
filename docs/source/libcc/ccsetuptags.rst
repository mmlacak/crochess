.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccsetuptags:

Setup tags
==========

Documents ``cc_setup_tags.h`` and ``cc_setup_tags.c`` files, which contain
tags setup definitions and functions.

.. _lbl-libcc-ccsetuptags-data:

Setup tags data
---------------

.. c:type:: CcTagType const CC_SETUP_TAGS_CLASSICAL_CHESS[ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ][ CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS ]

    Classical Chess initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_CROATIAN_TIES[ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ][ CC_VARIANT_BOARD_SIZE_CROATIAN_TIES ]

    Croatian Ties initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_MAYAN_ASCENDANCY[ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ][ CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY ]

    Mayan Ascendancy initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_AGE_OF_AQUARIUS[ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ][ CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS ]

    Age of Aquarius initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_MIRANDAS_VEIL[ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ][ CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL ]

    Miranda's Veil initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_NINETEEN[ CC_VARIANT_BOARD_SIZE_NINETEEN ][ CC_VARIANT_BOARD_SIZE_NINETEEN ]

    Nineteen initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_HEMERAS_DAWN[ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ][ CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN ]

    Hemera's Dawn initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_TAMOANCHAN_REVISITED[ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ][ CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED ]

    Tamoanchan Revisited initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_CONQUEST_OF_TLALOCAN[ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ][ CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN ]

    Conquest of Tlalocan initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_DISCOVERY[ CC_VARIANT_BOARD_SIZE_DISCOVERY ][ CC_VARIANT_BOARD_SIZE_DISCOVERY ]

    Discovery initial tags.

.. c:type:: CcTagType const CC_SETUP_TAGS_ONE[ CC_VARIANT_BOARD_SIZE_ONE ][ CC_VARIANT_BOARD_SIZE_ONE ]

    One initial tags.

.. _lbl-libcc-ccsetuptags-functions:

Setup tags functions
--------------------

.. c:function:: CcTagType const * cc_setup_tags_get( CcVariantEnum ve )

    Function returning setup for a tags, based on a given variant.

    :param ve: A variant.
    :returns: Pointer to setup if successful, :c:data:`NULL` otherwise.

.. _lbl-libcc-ccsetuptags-sourcecodeheader:

Setup tags source code header
-----------------------------

Included source code file is ``cc_setup_tags.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_setup_tags.h
    :language: C
    :linenos:

.. _lbl-libcc-ccsetuptags-sourcecodefile:

Setup tags source code file
---------------------------

Included source code file is ``cc_setup_tags.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_setup_tags.c
    :language: C
    :linenos:
