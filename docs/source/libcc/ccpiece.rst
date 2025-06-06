.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccpiece:

Pieces
======

Documents ``cc_piece.h`` and ``cc_piece.c`` files, which contain piece
enumeration, and related functions.

Type of a piece is what remains after it has been stripped of color (or
shade). For instance, light and dark Rook are both Rooks. Similarly, dim
and bright Star are both Stars.

In this text, piece symbol is uppercase char, representing piece type, e.g. ``N``.

Piece symbol on its own does not contain information if a particular piece is
light/bright or dark/dim.

Piece char is representation of a piece type, it is lowercase if piece is dark/dim
pieces, otherwise it's uppercase.

For example, piece char for dark Knight is ``n``, while for light Knight it is ``N``;
piece symbol for both is the same, ``N``.

.. _lbl-libcc-ccpiece-validity:

Validity
--------

.. c:macro:: CC_PIECE_IS_ENUMERATOR(pe)

    Macro to check if given piece is :c:type:`CcPieceTagType` enumerator, i.e. between
    :c:enumerator:`CC_PTE_DimStar` and :c:enumerator:`CC_PTE_Monolith` values.

    :param pe: A piece, integer value.
    :returns: :c:data:`true` if :c:type:`CcPieceTagType` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_VALID(pe)

    Macro to check if given piece is a valid :c:type:`CcPieceTagType` enumerator,
    i.e. between :c:enumerator:`CC_PTE_DimStar` and :c:enumerator:`CC_PTE_Monolith`
    values, but not :c:enumerator:`CC_PTE_None`.

    :param pe: A piece, integer value.
    :returns: :c:data:`true` if valid :c:type:`CcPieceTagType` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_NONE(pe)

    Macro to check if given piece is :c:enumerator:`CC_PTE_None`.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is :c:enumerator:`CC_PTE_None`, :c:data:`false` otherwise.

.. _lbl-libcc-ccpiece-values:

Values
------

.. c:macro:: CC_PIECE_IS_PAWN(pe)

    Macro to check if given piece is a Pawn.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pawn, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_KNIGHT(pe)

    Macro to check if given piece is a Knight.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Knight, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_BISHOP(pe)

    Macro to check if given piece is a Bishop.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Bishop, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ROOK(pe)

    Macro to check if given piece is a Rook.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Rook, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_QUEEN(pe)

    Macro to check if given piece is a Queen.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Queen, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_KING(pe)

    Macro to check if given piece is a King.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a King, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PEGASUS(pe)

    Macro to check if given piece is a Pegasus.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pegasus, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PYRAMID(pe)

    Macro to check if given piece is a Pyramid.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pyramid, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_UNICORN(pe)

    Macro to check if given piece is a Unicorn.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Unicorn, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_WAVE(pe)

    Macro to check if given piece is a Wave.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Wave, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_STAR(pe)

    Macro to check if given piece is a Star.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Star, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_CENTAUR(pe)

    Macro to check if given piece is a Centaur.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Centaur, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SCOUT(pe)

    Macro to check if given piece is a Scout.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Scout, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_GRENADIER(pe)

    Macro to check if given piece is a Grenadier.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Grenadier, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SERPENT(pe)

    Macro to check if given piece is a Serpent.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Serpent, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SHAMAN(pe)

    Macro to check if given piece is a Shaman.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Shaman, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_MONOLITH(pe)

    Macro to check if given piece is a Monolith.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Monolith, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_STARCHILD(pe)

    Macro to check if given piece is a Starchild.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Starchild, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TROOPER(pe)

    Macro to check if given piece is a trooper, i.e. Scout or Grenadier.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a trooper, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PRIVATE(pe)

    Macro to check if given piece is a private, i.e. Pawn, Scout, or Grenadier.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a private, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_MATERIAL(pe)

    Macro to check if given piece is a valid material piece, i.e. not Wave.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is material, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_CELESTIAL(pe)

    Macro to check if given piece is a valid celestial piece, i.e. a Star,
    Starchild or Monolith.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is celestial, :c:data:`false` otherwise.

.. _lbl-libcc-ccpiece-features:

Features
--------

.. c:macro:: CC_PIECE_HAS_OWNER(pe)

    Macro to check if given piece has owner, i.e. if it's light or dark piece.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece has owner, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ACTIVATOR(pe)

    Macro to check if given piece is :term:`activator`.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is :term:`activator`, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_ACTIVATE(pe)

    Macro to check if given piece can activate other piece.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can activate other piece, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_ACTIVATE_STAR(pe)

    Macro to check if given piece can activate a Star.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can activate a Star, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_CAPTURE(pe)

    Macro to check if given piece can capture opponent's pieces.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can capture, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_CAPTURE_EN_PASSANT(pe)

    Macro to check if given piece can capture opponent's :term:`private`\s en passant.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can capture en passant, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT(pe)

    Macro to check if given piece can be captured en passant.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be captured en passant, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PASIVE(pe)

    Macro to check if given piece is passive.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is passive, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ACTIVE(pe)

    Macro to check if given piece is active.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is active, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_WEIGHTLESS(pe)

    Macro to check if given piece is a :term:`weightless piece`.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is weightless, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_ACTIVATED(pe)

    Macro to check if given piece can be actived.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be actived, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_CAPTURED(pe)

    Macro to check if given piece can be captured.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be captured, :c:data:`false` otherwise.

.. c:macro:: CC_PAWN_CAN_BE_PROMOTED_TO(pe)

    Macro to check if Pawn can be promoted to a given piece.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be promoted to, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_CASTLE(pe)

    Macro to check if a given piece can be castle, i.e. if it's Rook or a King.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can castle, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_DISPLACE(pe)

    Macro to check if a given piece can displace Pawns, i.e. if it's Serpent.

    Displacement checked here refers to one during normal ply, and not in
    trance-journey.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can displace Pawns, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY()`

.. c:macro:: CC_PIECE_CAN_BE_DISPLACED(pe)

    Macro to check if a given piece can be displaced by a Serpent, i.e. if it's
    a Pawn.

    Displacement checked here refers to one during normal ply, and not in
    trance-journey.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be promoted to, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY(pe)

    Macro to check if a given piece can be displaced during trance-journey.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be displaced, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_PIECE_CAN_DISPLACE()`

.. c:macro:: CC_PIECE_CAN_BE_CONVERTED(pe)

    Macro to check if given piece can be converted, i.e. if piece can
    change its owner.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be converted, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DEMOTED(pe)

    Macro to check if given piece can be demoted to a Pawn.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be demoted, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_RESURRECTED(pe)

    Macro to check if given piece can be resurrected.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be resurrected, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TELEPORTER(pe)

    Macro to check if given piece can teleport other pieces.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can teleport other pieces, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_TELEPORTED(pe)

    Macro to check if given piece can be teleported.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be teleported, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_COMPLETELY_TRANSPARENT(pe)

    Macro to check if given piece is completely transparent to other pieces,
    including to Monolith.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is completely transparent, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TRANSPARENT(pe)

    Macro to check if given piece is transparent to any other semi-transparent piece.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is transparent, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SEMI_TRANSPARENT(pe)

    Macro to check if given piece is transparent to Wave.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is transparent to Wave, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SEMI_OPAQUE(pe)

    Macro to check if given piece is transparent to Wave, but
    not transparent to other semi-transparent (non-Wave) pieces.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is semi-opaque, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_OPAQUE(pe)

    Macro to check if given piece is opaque.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is opaque, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_DIVERGENT(pe)

    Macro to check if given piece is divergent, i.e. can be diverged from.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is divergent, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DIVERGED(pe)

    Macro to check if given piece can be diverged.

    .. note::

        Some pieces can be diverged only sometimes. For instance, Wave can be
        diverged if it's not activated by e.g. Centaur.

    .. note::

        All activated pieces when diverging are also restricted by momentum.
        For instance, Rook normally can diverge, except if it has no momentum
        when it encounters own Shaman.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be diverged, :c:data:`false` otherwise.

.. c:macro:: CC_WAVE_CAN_BE_DIVERGED(activator)

    Macro to check if Wave can be diverged, based on its :term:`activator`.

    .. note::

        Wave can be diverged only if it's not activated by Unicorn, Centaur, or Serpent.

    :param activator: An :term:`activator`, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if Wave can be diverged, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SINGLE_STEP(pe)

    Macro to check if given piece has a single step, i.e. Knight.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can make a single step in a ply,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ONE_STEP(pe)

    Macro to check if given piece moves in one step, i.e. Bishop.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can choose one step in a ply,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TWO_STEP(pe)

    Macro to check if given piece moves in two alternating steps, i.e. Centaur.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece has two alternating steps,
              :c:data:`false` otherwise.

.. c:macro:: CC_WAVE_IS_TWO_STEP(activator)

    Macro to check if Wave moves in two alternating steps, which depends on
    :term:`activator`, i.e. if Wave is activated by Unicorn, Centaur, or Serpent.

    :param activator: An :term:`activator`, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece changes has two alternating steps,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_HAS_NEW_STEP_AFTER_EACH(pe)

    Macro to check if given piece can change its direction after each step.

    :param pe: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can change its direction after each step,
              :c:data:`false` otherwise.

.. _lbl-libcc-ccpiece-types:

Types
-----

Light pieces belong to light player, dark pieces to dark player.

Dim, bright, and pieces with no color designation (i.e. Stars,
Monoliths) do not belong to any player.

.. c:enum:: CcPieceTagEnum

    Enumerates all pieces, used in all variants.

    .. c:enumerator:: CC_PTE_DimStar

        Equals to :c:`-17`.

    .. c:enumerator:: CC_PTE_DarkStarchild
    .. c:enumerator:: CC_PTE_DarkShaman
    .. c:enumerator:: CC_PTE_DarkSerpent
    .. c:enumerator:: CC_PTE_DarkGrenadier
    .. c:enumerator:: CC_PTE_DarkScout
    .. c:enumerator:: CC_PTE_DarkCentaur
    .. c:enumerator:: CC_PTE_DarkWave
    .. c:enumerator:: CC_PTE_DarkUnicorn
    .. c:enumerator:: CC_PTE_DarkPyramid
    .. c:enumerator:: CC_PTE_DarkPegasus
    .. c:enumerator:: CC_PTE_DarkKing
    .. c:enumerator:: CC_PTE_DarkQueen
    .. c:enumerator:: CC_PTE_DarkRook
    .. c:enumerator:: CC_PTE_DarkBishop
    .. c:enumerator:: CC_PTE_DarkKnight
    .. c:enumerator:: CC_PTE_DarkPawn

    .. c:enumerator:: CC_PTE_None

        No piece present, equals to ``0``.
        Used for e.g. empty on-board fields, any off-board field.

    .. c:enumerator:: CC_PTE_LightPawn
    .. c:enumerator:: CC_PTE_LightKnight
    .. c:enumerator:: CC_PTE_LightBishop
    .. c:enumerator:: CC_PTE_LightRook
    .. c:enumerator:: CC_PTE_LightQueen
    .. c:enumerator:: CC_PTE_LightKing
    .. c:enumerator:: CC_PTE_LightPegasus
    .. c:enumerator:: CC_PTE_LightPyramid
    .. c:enumerator:: CC_PTE_LightUnicorn
    .. c:enumerator:: CC_PTE_LightWave
    .. c:enumerator:: CC_PTE_LightCentaur
    .. c:enumerator:: CC_PTE_LightScout
    .. c:enumerator:: CC_PTE_LightGrenadier
    .. c:enumerator:: CC_PTE_LightSerpent
    .. c:enumerator:: CC_PTE_LightShaman
    .. c:enumerator:: CC_PTE_LightStarchild

    .. c:enumerator:: CC_PTE_BrightStar

        Equals to ``17``.

    .. c:enumerator:: CC_PTE_Monolith

    :c:`enum` is tagged with the same :c:enum:`CcPieceTagEnum` name.

.. c:type:: signed char CcPieceTagType

    Actual storage type, as used in :c:struct:`CcChessboard` :c:member:`board`;
    contains only enumerations from :c:enum:`CcPieceTagEnum`.

.. _lbl-libcc-ccpiece-interfaces:

Interfaces
----------

.. c:type:: char (*cc_piece_fp_char_value_t)( CcPieceTagType pe )

    Function interface, i.e. function pointer type.

    :param pe: :c:type:`CcPieceTagType` value.
    :returns: Char, either a piece symbol, or a piece char.

.. _lbl-libcc-ccpiece-functions:

Functions
---------

.. c:function:: CcPieceTagType cc_piece_from_symbol( char symbol, bool is_light )

    Function returning chess piece, based on a piece symbol, and a flag.

    :param symbol: Piece symbol, uppercase char. It is taken verbatim, i.e. not converted to uppercase char.
    :param is_light: Whether piece is light/bright (:c:data:`true`), or dark/dim (:c:data:`false`).
    :returns: Piece enum if valid piece symbol passed, otherwise :c:enumerator:`CC_PTE_None`.

.. c:function:: bool cc_piece_symbol_is_valid( char c )

    Function checks if given character is a valid chess piece symbol.

    :param c: A character.
    :returns: :c:data:`true` if given character is a valid chess piece symbol, :c:data:`false` otherwise.

.. c:function:: CcPieceTagType cc_piece_opposite( CcPieceTagType pe )

    Function returning piece in opposite color (owner) to given piece.

    Dark pieces are converted to light ones, and vice versa.
    The same applies to dim, bright pieces, i.e. Stars.

    If piece has no owner, function returns given piece back unmodified.

    :param pe: A piece.
    :returns: A given piece converted to its opposite color.

.. c:function:: char cc_piece_as_char( CcPieceTagType pe )

    Function returning piece char, based on piece enum.

    Character returned is lowercase if piece is dark (dim), uppercase if piece
    is light (bright).

    Monoliths are always returned uppercase.

    If there is no piece (i.e. :c:enumerator:`CC_PTE_None` was given) space is returned.

    In case no valid piece enum was given, question mark (:c:`'?'`) is returned.

    :param pe: A piece.
    :returns: A piece character.

.. c:function:: CcPieceTagType cc_piece_from_char( char piece )

    Function returning chess piece, based on a piece character.

    For lowercase :c:`char` dark/dim piece is returned, otherwise light/bright one.

    Monolith is returned only for uppercase ``'M'`` :c:`char`.

    Space, unrecognized characters all yield :c:enumerator:`CC_PTE_None`.

    :param piece: A character.
    :returns: Piece enum if valid piece char passed, otherwise :c:enumerator:`CC_PTE_None`.

.. c:function:: char const * cc_piece_label( CcPieceTagType pe, bool capitalize, bool empty_field )

    Function returns a piece label.

    Piece label is capitalized name of a piece.

    Piece label is the same for dark (dim) and light (bright) pieces.

    For :c:enumerator:`CC_PTE_None` piece, label depends on :c:var:`capitalize`
    and :c:var:`empty_field` flags.

    All returned strings are null-terminated.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param pe: A piece.
    :param capitalize: Flag, whether to return capitalized string; affects only
        :c:enumerator:`CC_PTE_None` piece, i.e. empty field.
    :param empty_field: Flag, whether to return :c:`"empty field"`, or empty string.
    :returns: Pointer to string if successful, :c:macro:`CC_DEFAULT_VALUE_STRING` otherwise.

.. c:function:: char cc_piece_symbol( CcPieceTagType pe )

    Function returns a piece symbol, i.e. an uppercase :c:`char` for chess pieces.

    :param pe: A piece.
    :returns: A piece symbol if chess piece, space otherwise.
    :seealso: :c:func:`cc_piece_as_char()`

.. c:function:: CcPieceTagType cc_piece_demoting_to( CcPieceTagType pe )

    Function returns a Pawn to which given piece can be demoted to,
    or :c:enumerator:`CC_PTE_None` if piece can't be demoted.

    Dark pieces can be demoted to dark Pawn, similarly light pieces can be
    demoted to light Pawn.

    Stars, Monoliths (which don't have an owner) cannot be demoted to a Pawn,
    so :c:enumerator:`CC_PTE_None` is returned instead.

    :param pe: A piece.
    :returns: Pawn to which given piece can be demoted to, otherwise :c:enumerator:`CC_PTE_None`.

.. c:function:: bool cc_piece_is_dark( CcPieceTagType pe )

    Function checks if given chess piece is dark.

    :param pe: A piece.
    :returns: :c:data:`true` if given piece is dark, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_light( CcPieceTagType pe )

    Function checks if given chess piece is light.

    :param pe: A piece.
    :returns: :c:data:`true` if given piece is light, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_color( CcPieceTagType pe )

    Function checks if given chess piece has color, i.e. if it's either light, or dark.

    :param pe: A piece.
    :returns: :c:data:`true` if given piece has color, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_shade( CcPieceTagType pe )

    Function checks if given chess piece has shade, i.e. if it's either bright, or dim.

    :param pe: A piece.
    :returns: :c:data:`true` if given piece has shade, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_prefix( CcPieceTagType pe )

    Function checks if given chess piece has prefix, i.e. if it has either a color, or a shade.

    :param pe: A piece.
    :returns: :c:data:`true` if given piece has prefix, :c:data:`false` otherwise.

.. c:function:: char const * cc_piece_prefix( CcPieceTagType pe, bool capitalize )

    Function returns prefix of a given chess piece.

    Piece prefix is either a color, or a shade of a given piece,
    depending what it has.

    For pieces without neither color nor shade (:c:enumerator:`CC_PTE_None`, and Monolith),
    prefix is empty string.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param pe: A piece.
    :param capitalize: Flag, whether to return capitalized prefix.
    :returns: Pointer to string if successful, :c:macro:`CC_DEFAULT_VALUE_STRING` otherwise.

.. c:function:: bool cc_piece_has_congruent_type( char symbol, CcPieceTagType pe )

    Function checks if given piece has the same type as a piece symbol.

    :param symbol: Piece symbol, uppercase :c:`char`. It is taken verbatim, i.e. not converted to uppercase char.
    :param pe: A piece.
    :returns: :c:data:`true` if the same type, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_equal( char symbol, bool is_light, CcPieceTagType pe )

    Function checks if given piece is equal to one produced by a piece symbol, and a flag.

    :param symbol: Piece symbol, uppercase :c:`char`. It is taken verbatim, i.e. not converted to uppercase char.
    :param is_light: Flag, if piece is light/bright (:c:data:`true`), or dark/dim (:c:data:`false`).
    :param pe: A piece.
    :returns: :c:data:`true` if the same, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_same_type( CcPieceTagType pe_1, CcPieceTagType pe_2 )

    Function checks if two given pieces are the same type.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces have the same type, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_same_color( CcPieceTagType pe_1, CcPieceTagType pe_2 )

    Function checks if two given pieces are the same color, i.e. if
    both are light or dark.

    Stars have shade (bright, dim), not color; Monoliths don't have
    neither color, nor shade; so, this function returns :c:data:`false`
    if any given piece is a Star, or a Monolith.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces have the same color, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_same_shade( CcPieceTagType pe_1, CcPieceTagType pe_2 )

    Function checks if two given pieces are the same shade, i.e. if
    both are bright or dim.

    Stars have shade (bright, dim); if any given piece is not a Star
    function returns :c:data:`false`.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces have the same shade, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_opposite( CcPieceTagType pe_1, CcPieceTagType pe_2 )

    Function checks if two given pieces of the same type are in opposite
    color (dark, light), or shade (dim, bright) to each other.

    Pieces with no color and no shade always return :c:data:`false`;
    these are empty fields (i.e. :c:enumerator:`CC_PTE_None`), Monolith pieces.

    Shades and colors belong to different types of pieces, so will always
    yield :c:data:`false`.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces have the same type, but opposite color or shade, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_same_owner( CcPieceTagType pe_1, CcPieceTagType pe_2 )

    Function checks if two given pieces have the same owner, i.e.
    if pieces are of the same color (dark, or light), not necessarily
    the same type.

    If any given piece is without owner (i.e. with no color) it'll always
    return :c:data:`false`; these are empty fields (i.e. :c:enumerator:`CC_PTE_None`),
    Monolith, and Stars.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces are both dark or light, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_different_owner( CcPieceTagType pe_1, CcPieceTagType pe_2 )

    Function checks if two given pieces belongs to different players,
    i.e. if pieces are in opposite colors (one is light, the other is dark),
    not necessarily the same type.

    If any given piece is without owner (i.e. with no color) it'll always
    return :c:data:`false`; these are empty fields (i.e. :c:enumerator:`CC_PTE_None`),
    Monolith, and Stars.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces are in opposite color, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_owned_figure( CcPieceTagType pe )

    Function checks if given piece is an owned :term:`figure`.

    :param pe: A piece.
    :returns: :c:data:`true` if given piece is an owned :term:`figure`, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_figure( CcPieceTagType pe )

    Function checks if given piece is a :term:`figure`.

    :param pe: A piece.
    :returns: :c:data:`true` if given piece is a :term:`figure`, :c:data:`false` otherwise.

.. c:function:: char const * cc_piece_as_string( CcPieceTagType pe, bool capitalize, bool empty_field )

    Function returns string, containing piece prefix and label.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param pe: A piece.
    :param capitalize: Flag, whether to return capitalized string.
    :param empty_field: Flag, whether to return :c:`"empty field"`, or empty string.
    :returns: Pointer to string if successful, :c:macro:`CC_DEFAULT_VALUE_STRING` otherwise.
    :seealso: :c:func:`cc_piece_prefix()`, :c:func:`cc_piece_label()`, :c:macro:`CC_DEFAULT_VALUE_STRING`

.. _lbl-libcc-ccpiece-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_piece.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_piece.h
    :language: C
    :linenos:

.. _lbl-libcc-ccpiece-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_piece.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_piece.c
    :language: C
    :linenos:
