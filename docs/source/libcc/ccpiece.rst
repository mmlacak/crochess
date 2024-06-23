.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccpieces:

Pieces
======

Documents ``cc_piece.h`` and ``cc_piece.c`` files, which contain piece enumeration, and related functions.

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

    Function returning piece enum, based on a piece symbol, and a flag.

    :param symbol: Piece symbol, uppercase char. It is taken verbatim, i.e. not converted to uppercase char.
    :param is_light: Whether piece is light/bright (:c:`true`), or dark/dim (:c:`false`).
    :returns: Piece enum if valid piece symbol passed, otherwise :c:`CC_PE_None`.
