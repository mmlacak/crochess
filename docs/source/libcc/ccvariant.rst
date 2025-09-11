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

Board sizes
-----------

The board sizes defined for each variant.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS

    Board size for Classical Chess, equals to ``8``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CROATIAN_TIES

    Board size for Croatian Ties, equals to ``10``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_MAYAN_ASCENDANCY

    Board size for Mayan Ascendancy, equals to ``12``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_AGE_OF_AQUARIUS

    Board size for Age of Aquarius, equals to ``14``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_MIRANDAS_VEIL

    Board size for Miranda's Veil, equals to ``16``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_NINETEEN

    Board size for Nineteen, equals to ``18``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_HEMERAS_DAWN

    Board size for Hemera's Dawn, equals to ``20``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_TAMOANCHAN_REVISITED

    Board size for Tamoanchan Revisited, equals to ``22``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CONQUEST_OF_TLALOCAN

    Board size for Conquest of Tlalocan, equals to ``24``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_DISCOVERY

    Board size for Discovery, equals to ``24``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_ONE

    Board size for One, equals to ``26``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_14

    Board size for Classical Chess 14, equals to ``14``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_20

    Board size for Classical Chess 20, equals to ``20``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CLASSICAL_CHESS_26

    Board size for Classical Chess 26, equals to ``26``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_14

    Board size for Croatian Ties 14, equals to ``14``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_20

    Board size for Croatian Ties 20, equals to ``20``.

.. c:macro:: CC_VARIANT_BOARD_SIZE_CROATIAN_TIES_26

    Board size for Croatian Ties 26, equals to ``26``.


.. _lbl-libcc-ccvariant-rushlimits:

:term:`Rush` limits
-------------------

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

    Rush limits for Classical Chess, light player, equals to ``3``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CLASSICAL_CHESS_DARK

    Rush limits for Classical Chess, dark player, equals to ``4``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CROATIAN_TIES_LIGHT

    Rush limits for Croatian Ties, light player, equals to ``4``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CROATIAN_TIES_DARK

    Rush limits for Croatian Ties, dark player, equals to ``5``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_MAYAN_ASCENDANCY_LIGHT

    Rush limits for Mayan Ascendancy, light player, equals to ``5``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_MAYAN_ASCENDANCY_DARK

    Rush limits for Mayan Ascendancy, dark player, equals to ``6``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_AGE_OF_AQUARIUS_LIGHT

    Rush limits for Age of Aquarius, light player, equals to ``6``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_AGE_OF_AQUARIUS_DARK

    Rush limits for Age of Aquarius, dark player, equals to ``7``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_MIRANDAS_VEIL_LIGHT

    Rush limits for Miranda's Veil, light player, equals to ``7``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_MIRANDAS_VEIL_DARK

    Rush limits for Miranda's Veil, dark player, equals to ``8``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_NINETEEN_LIGHT

    Rush limits for Nineteen, light player, equals to ``8``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_NINETEEN_DARK

    Rush limits for Nineteen, dark player, equals to ``9``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_HEMERAS_DAWN_LIGHT

    Rush limits for Hemera's Dawn, light player, equals to ``9``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_HEMERAS_DAWN_DARK

    Rush limits for Hemera's Dawn, dark player, equals to ``10``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_TAMOANCHAN_REVISITED_LIGHT

    Rush limits for Tamoanchan Revisited, light player, equals to ``10``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_TAMOANCHAN_REVISITED_DARK

    Rush limits for Tamoanchan Revisited, dark player, equals to ``11``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CONQUEST_OF_TLALOCAN_LIGHT

    Rush limits for Conquest of Tlalocan, light player, equals to ``11``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CONQUEST_OF_TLALOCAN_DARK

    Rush limits for Conquest of Tlalocan, dark player, equals to ``12``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_DISCOVERY_LIGHT

    Rush limits for Discovery, light player, equals to ``11``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_DISCOVERY_DARK

    Rush limits for Discovery, dark player, equals to ``12``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_ONE_LIGHT

    Rush limits for One, light player, equals to ``12``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_ONE_DARK

    Rush limits for One, dark player, equals to ``13``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CLASSICAL_CHESS_14_LIGHT

    Rush limits for Classical Chess 14, light player, equals to ``6``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CLASSICAL_CHESS_14_DARK

    Rush limits for Classical Chess 14, dark player, equals to ``7``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CLASSICAL_CHESS_20_LIGHT

    Rush limits for Classical Chess 20, light player, equals to ``9``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CLASSICAL_CHESS_20_DARK

    Rush limits for Classical Chess 20, dark player, equals to ``10``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CLASSICAL_CHESS_26_LIGHT

    Rush limits for Classical Chess 26, light player, equals to ``12``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CLASSICAL_CHESS_26_DARK

    Rush limits for Classical Chess 26, dark player, equals to ``13``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CROATIAN_TIES_14_LIGHT

    Rush limits for Croatian Ties 14, light player, equals to ``6``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CROATIAN_TIES_14_DARK

    Rush limits for Croatian Ties 14, dark player, equals to ``7``.


.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CROATIAN_TIES_20_LIGHT

    Rush limits for Croatian Ties 20, light player, equals to ``9``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CROATIAN_TIES_20_DARK

    Rush limits for Croatian Ties 20, dark player, equals to ``10``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_CROATIAN_TIES_26_LIGHT

    Rush limits for Croatian Ties 26, light player, equals to ``12``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_CROATIAN_TIES_26_DARK

    Rush limits for Croatian Ties 26, dark player, equals to ``13``.


.. c:macro:: CC_VARIANT_RUSH_RANK_OFFSET

    For dark player, difference between board size and shortest rush rank.
    For light player, shortest rush rank.
    In both cases equals to ``3``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_LIGHT

    Minimum rush rank light private can reach, equals to ``3``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_LIGHT

    Maximum rush rank light private can reach, equals to ``12``.

.. c:macro:: CC_VARIANT_MIN_RUSH_RANK_DARK

    Minimum rush rank dark private can reach, equals to ``4``.

.. c:macro:: CC_VARIANT_MAX_RUSH_RANK_DARK

    Maximum rush rank dark private can reach, equals to ``22``.

.. _lbl-libcc-ccvariant-types:

Types
-----

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

    .. c:enumerator:: CC_VE_ClassicalChess_14

    .. c:enumerator:: CC_VE_ClassicalChess_20

    .. c:enumerator:: CC_VE_ClassicalChess_26

    .. c:enumerator:: CC_VE_CroatianTies_14

    .. c:enumerator:: CC_VE_CroatianTies_20

    .. c:enumerator:: CC_VE_CroatianTies_26

    :c:`enum` is tagged with the same :c:enum:`CcVariantEnum` name.

.. c:type:: unsigned char CcVariantType

    Actual storage type, as used in :c:struct:`CcChessboard` :c:member:`type`;
    contains only enumerations from :c:enum:`CcVariantEnum`.

.. c:macro:: CC_VARIANT_IS_ENUMERATOR(ve)

    Macro to check if given variant value is an enumerator, i.e. between
    :c:enumerator:`CC_VE_ClassicalChess` and :c:enumerator:`CC_VE_CroatianTies_26`
    values.

    :param ve: Variant (integer) value.
    :returns: :c:data:`true` if enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_VARIANT_IS_VALID(ve)

    Macro to check if given variant value is a valid enumerator, i.e. between
    :c:enumerator:`CC_VE_ClassicalChess` and :c:enumerator:`CC_VE_CroatianTies_26`
    values.

    This macro is the same as :c:macro:`CC_VARIANT_IS_ENUMERATOR`, since
    :c:enum:`CcVariantType` does not feature *null* (or *void*, or *empty*) value.

    :param ve: Variant (integer) value.
    :returns: :c:data:`true` if valid enumerator, :c:data:`false` otherwise.

.. c:macro:: CC_VARIANT_HAS_SIDEWAYS_PAWNS(ve)

    Macro to check if given variant has sideways Pawns, i.e. if it's Nineteen
    variant, or beyond, but not Classical Chess variant.

    :param ve: Variant (integer) value.
    :returns: :c:data:`true` if variant has sideways Pawns,
        :c:data:`false` otherwise.

.. _lbl-libcc-ccvariant-symbols:

Symbols
-------

Abbreviated variant names, used to e.g. select variant to play.

.. c:macro:: CC_MAX_LEN_VARIANT_SYMBOL

    Maximum length of a symbol string (char array), used by any variant;
    equals to ``3``.

.. c:macro:: CC_LEN_VARIANT_SYMBOL_INVALID

    Length of an invalid symbol string (char array); equals to ``0``.

.. c:type:: char const * const CC_VARIANT_CLASSICAL_CHESS_SYMBOL

    Classical Chess symbol, equals to ``"cc"``.

.. c:type:: char const * const CC_VARIANT_CROATIAN_TIES_SYMBOL

    Croatian Ties symbol, equals to ``"ct"``.

.. c:type:: char const * const CC_VARIANT_MAYAN_ASCENDANCY_SYMBOL

    Mayaan Ascendancy symbol, equals to ``"ma"``.

.. c:type:: char const * const CC_VARIANT_AGE_OF_AQUARIUS_SYMBOL

    Age of Aquarius symbol, equals to ``"aoa"``.

.. c:type:: char const * const CC_VARIANT_MIRANDAS_VEIL_SYMBOL

    Miranda's Veil symbol, equals to ``"mv"``.

.. c:type:: char const * const CC_VARIANT_NINETEEN_SYMBOL

    Nineteen symbol, equals to ``"n"``.

.. c:type:: char const * const CC_VARIANT_HEMERAS_DAWN_SYMBOL

    Hemera's Dawn symbol, equals to ``"hd"``.

.. c:type:: char const * const CC_VARIANT_TAMOANCHAN_REVISITED_SYMBOL

    Tamoanchan Revisited symbol, equals to ``"tr"``.

.. c:type:: char const * const CC_VARIANT_CONQUEST_OF_TLALOCAN_SYMBOL

    Conquest of Tlalocan symbol, equals to ``"cot"``.

.. c:type:: char const * const CC_VARIANT_DISCOVERY_SYMBOL

    Discovery symbol, equals to ``"d"``.

.. c:type:: char const * const CC_VARIANT_ONE_SYMBOL

    One symbol, equals to ``"o"``.

.. c:type:: char const * const CC_VARIANT_CLASSICAL_CHESS_14_SYMBOL

    Classical Chess 14 symbol, equals to ``"cc14"``.

.. c:type:: char const * const CC_VARIANT_CLASSICAL_CHESS_20_SYMBOL

    Classical Chess 20 symbol, equals to ``"cc20"``.

.. c:type:: char const * const CC_VARIANT_CLASSICAL_CHESS_26_SYMBOL

    Classical Chess 26 symbol, equals to ``"cc26"``.

.. c:type:: char const * const CC_VARIANT_CROATIAN_TIES_14_SYMBOL

    Croatian Ties 14 symbol, equals to ``"ct14"``.

.. c:type:: char const * const CC_VARIANT_CROATIAN_TIES_20_SYMBOL

    Croatian Ties 20 symbol, equals to ``"ct20"``.

.. c:type:: char const * const CC_VARIANT_CROATIAN_TIES_26_SYMBOL

    Croatian Ties 26 symbol, equals to ``"ct26"``.

.. c:type:: char const * const CC_VARIANT_SYMBOLS[]

    Array of all symbols, for all variants.

.. _lbl-libcc-ccvariant-functions:

Functions
---------

.. c:function:: size_t cc_variant_from_symbol( char const * str, CcVariantType * ve__o )

    Function returns variant :c:`enum`, based on a string.

    :param str: A string.
    :param ve__o: *Output*, variant :c:`enum` type.
    :returns: Size of a symbol found in string if successful,
        :c:macro:`CC_LEN_VARIANT_SYMBOL_INVALID` otherwise.

.. c:function:: char const * cc_variant_symbol( CcVariantType ve )

    Function returning variant symbol, i.e. lowercase abbreviation of a variant name.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param ve: Variant :c:`enum` type.
    :returns: Variant symbol string if successful, :c:data:`NULL` otherwise.

.. c:function:: char const * cc_variant_label( CcVariantType ve )

    Function returning variant label, i.e. capitalized name of a variant.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param ve: Variant :c:`enum` type.
    :returns: Variant label string if successful, :c:data:`NULL` otherwise.

.. c:function:: cc_uint_t cc_variant_board_size( CcVariantType ve )

    Function returning size of a board for a given variant.

    :param ve: Variant :c:`enum` type.
    :returns: Size of a chessboard if successful, ``0`` otherwise.

.. c:function:: cc_uint_t cc_variant_rush_rank_limit( CcVariantType ve, bool is_piece_light )

    Function returns rush limit, either maximum rank for light privates,
    or minimum rank for dark privates.

    :param ve: Variant :c:`enum` type.
    :param is_piece_light: Flag, whether piece is light (:c:data:`true`) or dark (:c:data:`false`).
    :returns: Rush limit for known variants, ``0`` otherwise.

.. c:function:: bool cc_variant_is_rank_in_rush_limits( CcVariantType ve, bool is_piece_light, int rank )

    Function checks if given rank is within rush limits;
    both upper and lower rush limits are checked.

    :param ve: Variant :c:`enum` type.
    :param is_piece_light: Flag, whether piece is light (:c:data:`true`) or dark (:c:data:`false`).
    :param rank: Rank, position along vertical axis.
    :returns: :c:data:`true` if within rush limits, :c:data:`false` otherwise.

.. c:function:: int cc_variant_promoting_rank( CcVariantType ve, bool is_light )

    Function returns rank of a promoting row.

    :param ve: Variant :c:`enum` type.
    :param is_light: Flag, whether it is for light or dark player.
    :returns: Rank of a promoting row if successful,
              :c:macro:`CC_INVALID_COORD` otherwise.

.. c:function:: int cc_variant_initial_figure_rank( CcVariantType ve, bool is_light )

    Function returns rank of a figure row.

    :param ve: Variant :c:`enum` type.
    :param is_light: Flag, whether it is for light or dark player.
    :returns: Rank of a figure row if successful,
              :c:macro:`CC_INVALID_COORD` otherwise.

.. _lbl-libcc-ccvariant-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_variant.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_variant.h
    :language: C
    :linenos:

.. _lbl-libcc-ccvariant-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_variant.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_variant.c
    :language: C
    :linenos:
