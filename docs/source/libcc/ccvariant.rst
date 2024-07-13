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

    Board size for Classical Chess, equals to ``8``.

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

    Board size for One, equals to ``26``.

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

    Rush limits for Miranda's Veil, dark player, equals to ``8``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_NINETEEN_LIGHT

    Rush limits for Nineteen, light player, equals to ``8``.

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

Variant types
-------------

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

    :c:`enum` is tagged with the same :c:enum:`CcVariantEnum` name.

.. _lbl-libcc-ccvariant-symbols:

Variant symbols
---------------

Abbreviated variant names, used to e.g. select variant to play.

.. c:macro:: CC_MAX_LEN_VARIANT_SYMBOL

    Maximum length of a symbol string (char array), used by any variant;
    equals to :c:`3`.

.. c:macro:: CC_LEN_VARIANT_SYMBOL_INVALID

    Length of an invalid symbol string (char array); equals to ``0``.

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

.. _lbl-libcc-ccvariant-functions:

Variant functions
-----------------

.. c:function:: size_t cc_variant_from_symbol( char const * str, CcVariantEnum * ve__o )

    Function returns variant :c:`enum`, based on a string.

    :param str: A string.
    :param ve__o: *Output*, variant :c:`enum`.
    :returns: Size of a symbol found in string if successful,
        :c:expr:`CC_LEN_VARIANT_SYMBOL_INVALID` otherwise.

.. c:function:: char const * cc_variant_symbol( CcVariantEnum ve )

    Function returning variant symbol, i.e. lowercase abbreviation of a variant name.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param ve: Variant :c:`enum`.
    :returns: Variant symbol string if successful, :c:data:`NULL` otherwise.

.. c:function:: char const * cc_variant_label( CcVariantEnum ve )

    Function returning variant label, i.e. capitalized name of a variant.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param ve: Variant :c:`enum`.
    :returns: Variant label string if successful, :c:data:`NULL` otherwise.

.. c:function:: uint cc_variant_board_size( CcVariantEnum ve )

    Function returning size of a board for a given variant.

    :param ve: Variant :c:`enum`.
    :returns: Size of a chessboard if successful, ``0`` otherwise.

.. c:function:: bool cc_variant_has_sideways_pawns( CcVariantEnum ve )

    Function returns if Pawns can move sideways in a given variant.

    :param ve: Variant :c:`enum`.
    :returns: :c:data:`true` if variant has sideways Pawns, :c:data:`false` otherwise.

.. c:function:: uint cc_variant_rush_rank_limit( CcVariantEnum ve, bool is_piece_light )

    Function returns rush limit, either maximum rank for light privates,
    or minimum rank for dark privates.

    :param ve: Variant :c:`enum`.
    :param is_piece_light: Flag, whether piece is light (:c:data:`true`) or dark (:c:data:`false`).
    :returns: Rush limit for known variants, ``0`` otherwise.

.. c:function:: bool cc_variant_is_rank_in_rush_limits( CcVariantEnum ve, bool is_piece_light, int rank )

    Function checks if given rank is within rush limits;
    both upper and lower rush limits are checked.

    :param ve: Variant :c:`enum`.
    :param is_piece_light: Flag, whether piece is light (:c:data:`true`) or dark (:c:data:`false`).
    :param rank: Rank, position along vertical axis.
    :returns: :c:data:`true` if within rush limits, :c:data:`false` otherwise.

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
