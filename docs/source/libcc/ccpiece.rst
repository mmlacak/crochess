.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccpiece:

Pieces
======

Documents ``cc_piece.h`` and ``cc_piece.c`` files, which contain piece
enumeration, and related functions.

Type of a piece is what remains after it has been stripped of color (or
shade). For instance, light and dark Rook are both Rooks. Similarly, dim
and bright Star are both Stars.

Piece validity
--------------

.. c:macro:: CC_PIECE_IS_VALID(pe)

    Macro to check if given piece is a valid,
    i.e. between :c:`CC_PE_DimStar` and :c:`CC_PE_Monolith` values.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if valid chess piece, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_EQUAL(pe1,pe2)

    Macro to check if given pieces are the same.

    :param pe1: :c:expr:`CcPieceEnum` value.
    :param pe2: :c:expr:`CcPieceEnum` value to compare.
    :returns: :c:`true` if pieces are the same, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_NONE(pe)

    Macro to check if given piece is :c:`CC_PE_None`.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is :c:`CC_PE_None`, :c:`false` otherwise.

Piece values
------------

.. c:macro:: CC_PIECE_IS_PAWN(pe)

    Macro to check if given piece is a Pawn.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Pawn, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_KNIGHT(pe)

    Macro to check if given piece is a Knight.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Knight, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_BISHOP(pe)

    Macro to check if given piece is a Bishop.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Bishop, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ROOK(pe)

    Macro to check if given piece is a Rook.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Rook, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_QUEEN(pe)

    Macro to check if given piece is a Queen.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Queen, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_KING(pe)

    Macro to check if given piece is a King.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a King, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PEGASUS(pe)

    Macro to check if given piece is a Pegasus.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Pegasus, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PYRAMID(pe)

    Macro to check if given piece is a Pyramid.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Pyramid, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_UNICORN(pe)

    Macro to check if given piece is a Unicorn.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Unicorn, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_WAVE(pe)

    Macro to check if given piece is a Wave.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Wave, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_STAR(pe)

    Macro to check if given piece is a Star.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Star, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_CENTAUR(pe)

    Macro to check if given piece is a Centaur.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Centaur, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SCOUT(pe)

    Macro to check if given piece is a Scout.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Scout, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_GRENADIER(pe)

    Macro to check if given piece is a Grenadier.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Grenadier, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SERPENT(pe)

    Macro to check if given piece is a Serpent.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Serpent, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SHAMAN(pe)

    Macro to check if given piece is a Shaman.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Shaman, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_MONOLITH(pe)

    Macro to check if given piece is a Monolith.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Monolith, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_STARCHILD(pe)

    Macro to check if given piece is a Starchild.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is a Starchild, :c:`false` otherwise.

Piece features
--------------

.. c:macro:: CC_PIECE_HAS_OWNER(pe)

    Macro to check if given piece has owner, i.e. if it's light or dark piece.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece has owner, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ACTIVATOR(pe)

    Macro to check if given piece is :term:`activator`.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is :term:`activator`, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_ACTIVATE(pe)

    Macro to check if given piece can activate other piece.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can activate other piece, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_ACTIVATE_STAR(pe)

    Macro to check if given piece can activate a Star.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can activate a Star, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_CAPTURE(pe)

    Macro to check if given piece can capture opponent's pieces.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can capture, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_CAPTURE_EN_PASSANT(pe)

    Macro to check if given piece can capture opponent's :term:`private`\s en passant.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can capture en passant, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT(pe)

    Macro to check if given piece can be captured en passant.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be captured en passant, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PASIVE(pe)

    Macro to check if given piece is passive.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is passive, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ACTIVE(pe)

    Macro to check if given piece is active.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is active, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_WEIGHTLESS(pe)

    Macro to check if given piece is a :term:`weightless piece`.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is weightless, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_ACTIVATED(pe)

    Macro to check if given piece can be actived.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be actived, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_CAPTURED(pe)

    Macro to check if given piece can be captured.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be captured, :c:`false` otherwise.

.. c:macro:: CC_PAWN_CAN_BE_PROMOTED_TO(pe)

    Macro to check if Pawn can be promoted to a given piece.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be promoted to, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DISPLACED(pe)

    Macro to check if given piece can be displaced.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be displaced, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_CONVERTED(pe)

    Macro to check if given piece can be converted, i.e. if piece can
    change its owner.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be converted, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DEMOTED(pe)

    Macro to check if given piece can be demoted to a Pawn.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be demoted, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_RESURRECTED(pe)

    Macro to check if given piece can be resurrected.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be resurrected, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TELEPORTER(pe)

    Macro to check if given piece can teleport other pieces.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can teleport other pieces, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_TELEPORTED(pe)

    Macro to check if given piece can be teleported.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be teleported, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_COMPLETELY_TRANSPARENT(pe)

    Macro to check if given piece is completely transparent to other pieces,
    including to Monolith.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is completely transparent, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TRANSPARENT(pe)

    Macro to check if given piece is transparent to any other semi-transparent piece.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is transparent, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SEMI_TRANSPARENT(pe)

    Macro to check if given piece is transparent to Wave.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is transparent to Wave, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SEMI_OPAQUE(pe)

    Macro to check if given piece is transparent to Wave, but
    not transparent to other semi-transparent (non-Wave) pieces.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is semi-opaque, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_OPAQUE(pe)

    Macro to check if given piece is opaque.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is opaque, :c:`false` otherwise.

.. c:macro:: CC_PIECE_IS_DIVERGENT(pe)

    Macro to check if given piece is divergent, i.e. can be diverged from.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece is divergent, :c:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DIVERGED(pe)

    Macro to check if given piece can be diverged.

    .. note::

        Some pieces can be diverged only sometimes. For instance, Wave can be
        diverged if it's not activated by e.g. Centaur.

    .. note::

        All activated pieces when diverging are also restricted by momentum.
        For instance, Rook normally can diverge, except if it has no momentum
        when it encounters own Shaman.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece can be diverged, :c:`false` otherwise.

.. c:macro:: CC_WAVE_CAN_BE_DIVERGED(activator)

    Macro to check if Wave can be diverged, based on its :term:`activator`.

    .. note::

        Wave can be diverged only if it's not activated by Unicorn, Centaur, or Serpent.

    :param activator: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if Wave can be diverged, :c:`false` otherwise.

.. c:macro:: CC_PIECE_HAS_NEW_STEP_AFTER_EACH(pe)

    Macro to check if given piece changes its direction after each step.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: :c:`true` if piece changes its direction after each step,
              :c:`false` otherwise.

Piece types
-----------

Light pieces belong to light player, dark pieces to dark player.

Dim, bright, and pieces with no color designation (i.e. Stars,
Monoliths) do not belong to any player.

.. c:enum:: CcPieceEnum

    Enumerates all pieces, used in all variants.

    .. c:enumerator:: CC_PE_DimStar

        Equals to :c:`-17`.

    .. c:enumerator:: CC_PE_DarkStarchild
    .. c:enumerator:: CC_PE_DarkShaman
    .. c:enumerator:: CC_PE_DarkSerpent
    .. c:enumerator:: CC_PE_DarkGrenadier
    .. c:enumerator:: CC_PE_DarkScout
    .. c:enumerator:: CC_PE_DarkCentaur
    .. c:enumerator:: CC_PE_DarkWave
    .. c:enumerator:: CC_PE_DarkUnicorn
    .. c:enumerator:: CC_PE_DarkPyramid
    .. c:enumerator:: CC_PE_DarkPegasus
    .. c:enumerator:: CC_PE_DarkKing
    .. c:enumerator:: CC_PE_DarkQueen
    .. c:enumerator:: CC_PE_DarkRook
    .. c:enumerator:: CC_PE_DarkBishop
    .. c:enumerator:: CC_PE_DarkKnight
    .. c:enumerator:: CC_PE_DarkPawn

    .. c:enumerator:: CC_TE_None

        No piece present, equals to :c:`0`.
        Used for e.g. empty on-board fields, any off-board field.

    .. c:enumerator:: CC_PE_LightPawn
    .. c:enumerator:: CC_PE_LightKnight
    .. c:enumerator:: CC_PE_LightBishop
    .. c:enumerator:: CC_PE_LightRook
    .. c:enumerator:: CC_PE_LightQueen
    .. c:enumerator:: CC_PE_LightKing
    .. c:enumerator:: CC_PE_LightPegasus
    .. c:enumerator:: CC_PE_LightPyramid
    .. c:enumerator:: CC_PE_LightUnicorn
    .. c:enumerator:: CC_PE_LightWave
    .. c:enumerator:: CC_PE_LightCentaur
    .. c:enumerator:: CC_PE_LightScout
    .. c:enumerator:: CC_PE_LightGrenadier
    .. c:enumerator:: CC_PE_LightSerpent
    .. c:enumerator:: CC_PE_LightShaman
    .. c:enumerator:: CC_PE_LightStarchild

    .. c:enumerator:: CC_PE_BrightStar

        Equals to :c:`17`.

    .. c:enumerator:: CC_PE_Monolith

Piece interfaces
----------------

.. c:type:: char (*cc_piece_fp_char_value_t)( CcPieceEnum pe )

    Function interface, i.e. function pointer type.

    :param pe: :c:expr:`CcPieceEnum` value.
    :returns: Char, either a piece symbol, or a piece char.

Piece functions
---------------

.. c:function:: CcPieceEnum cc_piece_from_symbol( char symbol, bool is_light )

    Function returning chess piece, based on a piece symbol, and a flag.

    :param symbol: Piece symbol, uppercase char. It is taken verbatim, i.e. not converted to uppercase char.
    :param is_light: Whether piece is light/bright (:c:`true`), or dark/dim (:c:`false`).
    :returns: Piece enum if valid piece symbol passed, otherwise :c:`CC_PE_None`.

.. c:function:: bool cc_piece_symbol_is_valid( char c )

    Function checks if given character is a valid chess piece symbol.

    :param c: A character.
    :returns: :c:`true` if given character is a valid chess piece symbol, :c:`false` otherwise.

.. c:function:: CcPieceEnum cc_piece_opposite( CcPieceEnum pe )

    Function returning piece in opposite color (owner) to given piece.

    Dark pieces are converted to light ones, and vice versa.
    The same applies to dim, bright pieces, i.e. Stars.

    If piece has no owner, function returns given piece back unmodified.

    :param pe: A piece.
    :returns: A given piece converted to its opposite color.

.. c:function:: char cc_piece_as_char( CcPieceEnum pe )

    Function returning piece char, based on piece enum.

    Character returned is lowercase if piece is dark (dim), uppercase if piece
    is light (bright).

    Monoliths are always returned uppercase.

    If there is no piece (i.e. :c:`CC_PE_None` was given) space is returned.

    In case no valid piece enum was given, question mark (:c:`'?'`) is returned.

    :param pe: A piece.
    :returns: A piece character.

.. c:function:: CcPieceEnum cc_piece_from_char( char piece )

    Function returning chess piece, based on a piece character.

    For lowercase :c:expr:`char` dark/dim piece is returned, otherwise light/bright one.

    Monolith is returned only for uppercase :c:`'M'` :c:expr:`char`.

    Space, unrecognized characters all yield :c:`CC_PE_None`.

    :param piece: A character.
    :returns: Piece enum if valid piece char passed, otherwise :c:`CC_PE_None`.

.. c:function:: char const * cc_piece_label( CcPieceEnum pe )

    Function returns a piece label.

    Piece label is capitalized name of a piece.

    Piece label is the same for dark (dim) and light (bright) pieces.

    For :c:`CC_PE_None` piece, label is empty string.

    All returned strings are zero-terminated.

    .. warning::

        Returned string is not allocated, do not :c:expr:`free()` it.

    :param pe: A piece.
    :returns: Pointer to string if successful, :c:expr:`CC_DEFAULT_ENTITY_STRING` otherwise.

.. c:function:: char cc_piece_symbol( CcPieceEnum pe )

    Function returns a piece symbol, i.e. an uppercase :c:expr:`char` for chess pieces.

    :param pe: A piece.
    :returns: A piece symbol if chess piece, space otherwise.
    :seealso: :c:expr:`cc_piece_as_char()`

.. c:function:: CcPieceEnum cc_piece_demoting_to( CcPieceEnum pe )

    Function returns a Pawn to which given piece can be demoted to,
    or :c:`CC_PE_None` if piece can't be demoted.

    Dark pieces can be demoted to dark Pawn, similarly light pieces can be
    demoted to light Pawn.

    Stars, Monoliths (which don't have an owner) cannot be demoted to a Pawn,
    so :c:`CC_PE_None` is returned instead.

    :param pe: A piece.
    :returns: Pawn to which given piece can be demoted to, otherwise :c:`CC_PE_None`.

.. c:function:: bool cc_piece_is_dark( CcPieceEnum pe )

    Function checks if given chess piece is dark.

    :param pe: A piece.
    :returns: :c:`true` if given piece is dark, :c:`false` otherwise.

.. c:function:: bool cc_piece_is_light( CcPieceEnum pe )

    Function checks if given chess piece is light.

    :param pe: A piece.
    :returns: :c:`true` if given piece is light, :c:`false` otherwise.

.. c:function:: bool cc_piece_has_color( CcPieceEnum pe )

    Function checks if given chess piece has color, i.e. if it's either light, or dark.

    :param pe: A piece.
    :returns: :c:`true` if given piece has color, :c:`false` otherwise.

.. c:function:: bool cc_piece_has_shade( CcPieceEnum pe )

    Function checks if given chess piece has shade, i.e. if it's either bright, or dim.

    :param pe: A piece.
    :returns: :c:`true` if given piece has shade, :c:`false` otherwise.

.. c:function:: bool cc_piece_has_prefix( CcPieceEnum pe )

    Function checks if given chess piece has prefix, i.e. if it has either a color, or a shade.

    :param pe: A piece.
    :returns: :c:`true` if given piece has prefix, :c:`false` otherwise.

.. c:function:: char const * cc_piece_prefix( CcPieceEnum pe, bool capitalize )

    Function returns prefix of a given chess piece.

    Piece prefix is either a color, or a shade of a given piece,
    depending what it has.

    For pieces without neither color nor shade (:c:`CC_PE_None`, and Monolith),
    prefix is empty string.

    .. warning::

        Returned string is not allocated, do not :c:expr:`free()` it.

    :param pe: A piece.
    :param capitalize: Flag, whether to return capitalized prefix.
    :returns: Pointer to string if successful, :c:expr:`CC_DEFAULT_ENTITY_STRING` otherwise.

.. c:function:: bool cc_piece_has_congruent_type( char symbol, CcPieceEnum pe )

    Function checks if given piece has the same type as a piece symbol.

    :param symbol: Piece symbol, uppercase :c:expr:`char`. It is taken verbatim, i.e. not converted to uppercase char.
    :param pe: A piece.
    :returns: :c:`true` if the same type, :c:`false` otherwise.

.. c:function:: bool cc_piece_is_equal( char symbol, bool is_light, CcPieceEnum pe )

    Function checks if given piece is equal to one produced by a piece symbol, and a flag.

    :param symbol: Piece symbol, uppercase :c:expr:`char`. It is taken verbatim, i.e. not converted to uppercase char.
    :param is_light: Flag, if piece is light/bright (:c:`true`), or dark/dim (:c:`false`).
    :param pe: A piece.
    :returns: :c:`true` if the same, :c:`false` otherwise.

.. c:function:: bool cc_piece_has_same_type( CcPieceEnum pe_1, CcPieceEnum pe_2 )

    Function checks if two given pieces are the same type.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:`true` if given pieces have the same type, :c:`false` otherwise.

.. c:function:: bool cc_piece_has_same_color( CcPieceEnum pe_1, CcPieceEnum pe_2 )

    Function checks if two given pieces are the same color, i.e. if
    both are light or dark.

    Stars have shade (bright, dim), not color; Monoliths don't have
    neither color, nor shade; so, this function returns :c:`false`
    if any given piece is a Star, or a Monolith.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:`true` if given pieces have the same color, :c:`false` otherwise.

.. c:function:: bool cc_piece_has_same_shade( CcPieceEnum pe_1, CcPieceEnum pe_2 )

    Function checks if two given pieces are the same shade, i.e. if
    both are bright or dim.

    Stars have shade (bright, dim); if any given piece is not a Star
    function returns :c:`false`.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:`true` if given pieces have the same shade, :c:`false` otherwise.

.. c:function:: bool cc_piece_is_opposite( CcPieceEnum pe_1, CcPieceEnum pe_2 )

    Function checks if two given pieces of the same type are in opposite
    color (dark, light), or shade (dim, bright) to each other.

    Pieces with no color and no shade always return :c:`false`;
    these are :c:`CC_PE_None`, Monolith pieces.

    Shades and colors belong to different types of pieces, so will always
    yield :c:`false`.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:`true` if given pieces have the same type, but opposite color or shade, :c:`false` otherwise.

.. c:function:: bool cc_piece_has_same_owner( CcPieceEnum pe_1, CcPieceEnum pe_2 )

    Function checks if two given pieces have the same owner, i.e.
    if pieces are of the same color (dark, or light), not necessarily
    the same type.

    If any given piece is without owner (i.e. with no color) it'll always
    return :c:`false`; these are :c:`CC_PE_None`, Monolith, and Stars.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:`true` if given pieces are both dark or light, :c:`false` otherwise.

.. c:function:: bool cc_piece_has_different_owner( CcPieceEnum pe_1, CcPieceEnum pe_2 )

    Function checks if two given pieces belongs to different players,
    i.e. if pieces are in opposite colors (one is light, the other is dark),
    not necessarily the same type.

    If any given piece is without owner (i.e. with no color) it'll always
    return :c:`false`; these are :c:`CC_PE_None`, Monolith, and Stars.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:`true` if given pieces are in opposite color, :c:`false` otherwise.

.. c:function:: bool cc_piece_is_owned_figure( CcPieceEnum pe )

    Function checks if given piece is an owned :term:`figure`.

    :param pe: A piece.
    :returns: :c:`true` if given piece is an owned :term:`figure`, :c:`false` otherwise.

.. c:function:: bool cc_piece_is_figure( CcPieceEnum pe )

    Function checks if given piece is a :term:`figure`.

    :param pe: A piece.
    :returns: :c:`true` if given piece is a :term:`figure`, :c:`false` otherwise.

.. c:function:: char const * cc_piece_as_string( CcPieceEnum pe, bool capitalize, bool empty_field )

    Function returns string, containing piece prefix and label.

    .. warning::

        Returned string is not allocated, do not :c:expr:`free()` it.

    :param pe: A piece.
    :param capitalize: Flag, whether to return capitalized string.
    :param empty_field: Flag, whether to return :c:`"empty field"`, or empty string.
    :returns: Pointer to string if successful, :c:expr:`CC_DEFAULT_ENTITY_STRING` otherwise.
    :seealso: :c:expr:`cc_piece_prefix()`, :c:expr:`cc_piece_label()`, :c:expr:`CC_DEFAULT_ENTITY_STRING`
