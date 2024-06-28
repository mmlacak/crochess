.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccvariant:

Variants
========

Documents ``cc_variant.h`` and ``cc_variant.c`` files, which contain
variants definitions and functions.

.. _lbl-libcc-ccvariant-boardsizes:

Variant board sizes
-------------------

The board sizes defined for each variant.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS

    Board size for Classical Chess, equals to :c:`8`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CROATIAN_TIES

    Board size for Croatian Ties, equals to :c:`10`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY

    Board size for Mayan Ascendancy, equals to :c:`12`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS

    Board size for Age of Aquarius, equals to :c:`14`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL

    Board size for Miranda's Veil, equals to :c:`16`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_NINETEEN

    Board size for Nineteen, equals to :c:`18`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN

    Board size for Hemera's Dawn, equals to :c:`20`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED

    Board size for Tamoanchan Revisited, equals to :c:`22`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN

    Board size for Conquest of Tlalocan, equals to :c:`24`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_DISCOVERY

    Board size for Discovery, equals to :c:`24`.

.. c:macro:: CC_VARIANT_BOARD_SIZE_ONE

    Board size for One, equals to :c:`26`.

.. _lbl-libcc-ccvariant-rushlimits:

Variant :term:`rush` limits
---------------------------

:term:`Rush` limits defined for each variant, and both players.

For light player, rush limits are maximum rank its private can
reach in a single ply.

For dark player, rush limits are minimum rank its private can
reach in a single ply.

When rushing their privates, both players can reach all fields on
their own side of a chessboard, but cannot cross onto opponent's
side.

So, rush limits for all variants are always in the middle of a
chessboard, difference is only in the size of a board, and
position of the middle line.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CLASSICAL_CHESS_LIGHT

    Rush limits for Classical Chess, light player, equals to :c:`3`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CLASSICAL_CHESS_DARK

    Rush limits for Classical Chess, dark player, equals to :c:`4`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CROATIAN_TIES_LIGHT

    Rush limits for Croatian Ties, light player, equals to :c:`4`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CROATIAN_TIES_DARK

    Rush limits for Croatian Ties, dark player, equals to :c:`5`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_MAYAN_ASCENDANCY_LIGHT

    Rush limits for Mayan Ascendancy, light player, equals to :c:`5`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_MAYAN_ASCENDANCY_DARK

    Rush limits for Mayan Ascendancy, dark player, equals to :c:`6`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_AGE_OF_AQUARIUS_LIGHT

    Rush limits for Age of Aquarius, light player, equals to :c:`6`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_AGE_OF_AQUARIUS_DARK

    Rush limits for Age of Aquarius, dark player, equals to :c:`7`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_MIRANDAS_VEIL_LIGHT

    Rush limits for Miranda's Veil, light player, equals to :c:`7`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_MIRANDAS_VEIL_DARK

    Rush limits for Miranda's Veil, dark player, equals to :c:`8`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_NINETEEN_LIGHT

    Rush limits for Nineteen, light player, equals to :c:`8`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_NINETEEN_DARK

    Rush limits for Nineteen, dark player, equals to :c:`9`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_HEMERAS_DAWN_LIGHT

    Rush limits for Hemera's Dawn, light player, equals to :c:`9`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_HEMERAS_DAWN_DARK

    Rush limits for Hemera's Dawn, dark player, equals to :c:`10`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_TAMOANCHAN_REVISITED_LIGHT

    Rush limits for Tamoanchan Revisited, light player, equals to :c:`10`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_TAMOANCHAN_REVISITED_DARK

    Rush limits for Tamoanchan Revisited, dark player, equals to :c:`11`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CONQUEST_OF_TLALOCAN_LIGHT

    Rush limits for Conquest of Tlalocan, light player, equals to :c:`11`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CONQUEST_OF_TLALOCAN_DARK

    Rush limits for Conquest of Tlalocan, dark player, equals to :c:`12`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_DISCOVERY_LIGHT

    Rush limits for Discovery, light player, equals to :c:`11`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_DISCOVERY_DARK

    Rush limits for Discovery, dark player, equals to :c:`12`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_ONE_LIGHT

    Rush limits for One, light player, equals to :c:`12`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_ONE_DARK

    Rush limits for One, dark player, equals to :c:`13`.


.. c:macro:: CC_VARIANT_RUSH_RANK_OFFSET

    For dark player, difference between board size and shortest rush rank.
    For light player, shortest rush rank.
    In both cases equals to :c:`3`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_LIGHT

    Minimum rush rank light private can reach, equals to :c:`3`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_LIGHT

    Maximum rush rank light private can reach, equals to :c:`12`.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_DARK

    Minimum rush rank dark private can reach, equals to :c:`4`.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_DARK

    Maximum rush rank dark private can reach, equals to :c:`22`.

.. _lbl-libcc-ccvariant-types:

Variants types
--------------

.. c:enum:: CcVariantEnum

    Enumeration of all variants.

    .. c:enumerator:: CC_VE_ClassicalChess

    .. c:enumerator:: CC_VE_CroatianTies

    .. c:enumerator:: CC_VE_MayanAscendancy

    .. c:enumerator:: CC_VE_AgeOfAquarius

    .. c:enumerator:: CC_VE_MirandasVeil

    .. c:enumerator:: CC_VE_Nineteen

    .. c:enumerator:: CC_VE_HemerasDawn

    .. c:enumerator:: CC_VE_TamoanchanRevisited

    .. c:enumerator:: CC_VE_ConquestOfTlalocan

    .. c:enumerator:: CC_VE_Discovery

    .. c:enumerator:: CC_VE_One

    :c:`Enum` is tagged with the same :c:expr:`CcVariantEnum` name.

.. _lbl-libcc-ccvariant-symbols:

Variants symbols
----------------

Abbreviated variant names, used to e.g. select variant to play.

.. c:macro:: CC_MAX_LEN_VARIANT_SYMBOL

    Maximum length of a symbol string (char array), used by any variant;
    equals to :c:`3`.

.. c:macro:: CC_LEN_VARIANT_SYMBOL_INVALID

    Length of an invalid symbol string (char array); equals to :c:`0`.

.. c:type:: char const * const CC_VARIANT_CLASSICAL_CHESS_SYMBOL

    Classical Chess symbol, equals to :c:`"cc"`.

.. c:type:: char const * const CC_VARIANT_CROATIAN_TIES_SYMBOL

    Croatian Ties symbol, equals to :c:`"ct"`.

.. c:type:: char const * const CC_VARIANT_MAYAN_ASCENDANCY_SYMBOL

    Mayaan Ascendancy symbol, equals to :c:`"ma"`.

.. c:type:: char const * const CC_VARIANT_AGE_OF_AQUARIUS_SYMBOL

    Age of Aquarius symbol, equals to :c:`"aoa"`.

.. c:type:: char const * const CC_VARIANT_MIRANDAS_VEIL_SYMBOL

    Miranda's Veil symbol, equals to :c:`"mv"`.

.. c:type:: char const * const CC_VARIANT_NINETEEN_SYMBOL

    Nineteen symbol, equals to :c:`"n"`.

.. c:type:: char const * const CC_VARIANT_HEMERAS_DAWN_SYMBOL

    Hemera's Dawn symbol, equals to :c:`"hd"`.

.. c:type:: char const * const CC_VARIANT_TAMOANCHAN_REVISITED_SYMBOL

    Tamoanchan Revisited symbol, equals to :c:`"tr"`.

.. c:type:: char const * const CC_VARIANT_CONQUEST_OF_TLALOCAN_SYMBOL

    Conquest of Tlalocan symbol, equals to :c:`"cot"`.

.. c:type:: char const * const CC_VARIANT_DISCOVERY_SYMBOL

    Discovery symbol, equals to :c:`"d"`.

.. c:type:: char const * const CC_VARIANT_ONE_SYMBOL

    One symbol, equals to :c:`"o"`.

.. c:type:: char const * const CC_VARIANT_SYMBOLS[]

    Array of all symbols, for all variants.







.. _lbl-libcc-ccvariant-sourcecodeheader:

Variants source code header
---------------------------

Included source code file is ``cc_variant.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_variant.h
    :language: C
    :linenos:

.. _lbl-libcc-ccvariant-sourcecodefile:

Variants source code file
-------------------------

Included source code file is ``cc_variant.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_variant.c
    :language: C
    :linenos:
