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

.. c:macro:: CC_PIECE_IS_ENUMERATOR(pte)

    Macro to check if given piece is :c:type:`CcPieceTagType` enumerator, i.e. between
    :c:enumerator:`CC_PTE_DimStar` and :c:enumerator:`CC_PTE_Monolith` values.

    :param pte: A piece, integer value.
    :returns: :c:data:`true` if :c:type:`CcPieceTagType` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_VALID(pte)

    Macro to check if given piece is a valid :c:type:`CcPieceTagType` enumerator,
    i.e. between :c:enumerator:`CC_PTE_DimStar` and :c:enumerator:`CC_PTE_Monolith`
    values, but not :c:enumerator:`CC_PTE_None`.

    :param pte: A piece, integer value.
    :returns: :c:data:`true` if valid :c:type:`CcPieceTagType` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_NONE(pte)

    Macro to check if given piece is :c:enumerator:`CC_PTE_None`.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is :c:enumerator:`CC_PTE_None`, :c:data:`false` otherwise.

.. _lbl-libcc-ccpiece-values:

Values
------

.. c:macro:: CC_PIECE_IS_JUST_PAWN(pte)

    Macro to check if given piece is just a Pawn, without any tags.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is just a Pawn, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PAWN_CAN_RUSH(pte)

    Macro to check if given piece is a Pawn, which can also rush.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pawn which can rush, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PAWN_RUSHED_PREVIOUS(pte)

    Macro to check if given piece is a Pawn, which rushed in a previous turn.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pawn which rushed in previous turn,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PAWN_RUSHED_CURRENT(pte)

    Macro to check if given piece is a Pawn, which rushed in current turn.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pawn which rushed in current turn,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PAWN_DELAYED_PROMOTION(pte)

    Macro to check if given piece is a Pawn, with delayed promotion tag.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pawn which can be promoted later,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PAWN(pte)

    Macro to check if given piece is any Pawn, with or without an applicable tag.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pawn, regardless of its tag,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_KNIGHT(pte)

    Macro to check if given piece is a Knight.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Knight, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_BISHOP(pte)

    Macro to check if given piece is a Bishop.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Bishop, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_JUST_ROOK(pte)

    Macro to check if given piece is just a Rook, without any tag.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is just a Rook, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ROOK_CAN_CASTLE(pte)

    Macro to check if given piece is a Rook, which can also castle.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Rook which can castle, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ROOK(pte)

    Macro to check if given piece is a Rook, which might (or not) castle.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is any Rook, regardless of its tag,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_QUEEN(pte)

    Macro to check if given piece is a Queen.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Queen, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_JUST_KING(pte)

    Macro to check if given piece is just a King, without any tags.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is just a King, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_KING_CAN_CASTLE(pte)

    Macro to check if given piece is a King, which can also castle.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a King which can castle,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_KING(pte)

    Macro to check if given piece is any King, which might (or not) castle.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is any King, regardless of its tag,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PEGASUS(pte)

    Macro to check if given piece is a Pegasus.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pegasus, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PYRAMID(pte)

    Macro to check if given piece is a Pyramid.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Pyramid, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_UNICORN(pte)

    Macro to check if given piece is a Unicorn.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Unicorn, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_WAVE(pte)

    Macro to check if given piece is a Wave.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Wave, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_STAR(pte)

    Macro to check if given piece is a Star.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Star, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_CENTAUR(pte)

    Macro to check if given piece is a Centaur.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Centaur, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_JUST_SCOUT(pte)

    Macro to check if given piece is just a Scout, without any tag.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is just a Scout, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SCOUT_CAN_RUSH(pte)

    Macro to check if given piece is a Scout, which can also rush.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Scout which can rush, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SCOUT_RUSHED_PREVIOUS(pte)

    Macro to check if given piece is a Scout, which rushed in a previous turn.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Scout which rushed in a previous turn,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SCOUT_RUSHED_CURRENT(pte)

    Macro to check if given piece is a Scout, which rushed in a current turn.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Scout which rushed in a current turn,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SCOUT(pte)

    Macro to check if given piece is any Scout, with or without a tag.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is any Scout, regardless of its tag,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_JUST_GRENADIER(pte)

    Macro to check if given piece is just a Grenadier, without any tag.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is just a Grenadier, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_GRENADIER_CAN_RUSH(pte)

    Macro to check if given piece is a Grenadier, which can also rush.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Grenadier which can rush, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_GRENADIER_RUSHED_PREVIOUS(pte)

    Macro to check if given piece is a Grenadier, which rushed in a previous turn.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Grenadier, which rushed in a previous turn,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_GRENADIER_RUSHED_CURRENT(pte)

    Macro to check if given piece is a Grenadier, which rushed in current turn.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Grenadier, which rushed in current turn,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_GRENADIER(pte)

    Macro to check if given piece is any Grenadier, with or without a tag.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Grenadier, regardless of its tag,
        :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SERPENT(pte)

    Macro to check if given piece is a Serpent.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Serpent, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SHAMAN(pte)

    Macro to check if given piece is a Shaman.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Shaman, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_MONOLITH(pte)

    Macro to check if given piece is a Monolith.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Monolith, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_STARCHILD(pte)

    Macro to check if given piece is a Starchild.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a Starchild, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TROOPER(pte)

    Macro to check if given piece is a trooper, i.e. Scout or Grenadier.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a trooper, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PRIVATE(pte)

    Macro to check if given piece is a private, i.e. Pawn, Scout, or Grenadier.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is a private, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_MATERIAL(pte)

    Macro to check if given piece is a valid material piece, i.e. not Wave.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is material, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_CELESTIAL(pte)

    Macro to check if given piece is a valid celestial piece, i.e. a Star,
    Starchild or Monolith.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is celestial, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_DARK(pte)

    Macro to check if given piece is a dark piece.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is dark, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_LIGHT(pte)

    Macro to check if given piece is a light piece.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is light, :c:data:`false` otherwise.

.. _lbl-libcc-ccpiece-features:

Features
--------

.. c:macro:: CC_PIECE_HAS_OPPOSITE(pte)

    Macro to check if given piece has its opposite, i.e. a piece in opposite
    (light or dark) color.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece has opposite, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_HAS_OWNER(pte)

    Macro to check if given piece has owner, i.e. if it's light or dark piece.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece has owner, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ACTIVATOR(pte)

    Macro to check if given piece is :term:`activator`.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is :term:`activator`, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_ACTIVATE(pte)

    Macro to check if given piece can activate other piece.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can activate other piece, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_ACTIVATE_STAR(pte)

    Macro to check if given piece can activate a Star.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can activate a Star, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_CAPTURE(pte)

    Macro to check if given piece can capture opponent's pieces.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can capture, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_CAPTURE_EN_PASSANT(pte)

    Macro to check if given piece can capture opponent's :term:`private`\s en passant.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can capture en passant, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT(pte)

    Macro to check if given piece can be captured en passant.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be captured en passant, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_PASIVE(pte)

    Macro to check if given piece is passive.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is passive, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ACTIVE(pte)

    Macro to check if given piece is active.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is active, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_WEIGHTLESS(pte)

    Macro to check if given piece is a :term:`weightless piece`.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is weightless, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_ACTIVATED(pte)

    Macro to check if given piece can be actived.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be actived, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_CAPTURED(pte)

    Macro to check if given piece can be captured.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be captured, :c:data:`false` otherwise.

.. c:macro:: CC_PAWN_CAN_BE_PROMOTED_TO(pte)

    Macro to check if Pawn can be promoted to a given piece.

    .. note::

        Pawn can only be promoted to a piece without any tags.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be promoted to, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_CASTLE(pte)

    .. todo::

        TODO :: FIX :: DOCS :: also check CC_TAG_IS_CAN_CASTLE

    Macro to check if a given piece can be castle, i.e. if it's Rook or a King.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can castle, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_DISPLACE(pte)

    Macro to check if a given piece can displace Pawns, i.e. if it's Serpent.

    Displacement checked here refers to one during normal ply, and not in
    trance-journey.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can displace Pawns, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY()`

.. c:macro:: CC_PIECE_CAN_BE_DISPLACED(pte)

    Macro to check if a given piece can be displaced by a Serpent, i.e. if it's
    a Pawn.

    Displacement checked here refers to one during normal ply, and not in
    trance-journey.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be promoted to, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DISPLACED_TRANCE_JOURNEY(pte)

    Macro to check if a given piece can be displaced during trance-journey.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be displaced, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_PIECE_CAN_DISPLACE()`

.. c:macro:: CC_PIECE_CAN_BE_CONVERTED(pte)

    Macro to check if given piece can be converted, i.e. if piece can
    change its owner.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be converted, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DEMOTED(pte)

    Macro to check if given piece can be demoted to a Pawn.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be demoted, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_RESURRECTED(pte)

    Macro to check if given piece can be resurrected.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be resurrected, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TELEPORTER(pte)

    Macro to check if given piece can teleport other pieces.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can teleport other pieces, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_TELEPORTED(pte)

    Macro to check if given piece can be teleported.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be teleported, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_COMPLETELY_TRANSPARENT(pte)

    Macro to check if given piece is completely transparent to other pieces,
    including to Monolith.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is completely transparent, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TRANSPARENT(pte)

    Macro to check if given piece is transparent to any other semi-transparent piece.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is transparent, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SEMI_TRANSPARENT(pte)

    Macro to check if given piece is transparent to Wave.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is transparent to Wave, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SEMI_OPAQUE(pte)

    Macro to check if given piece is transparent to Wave, but
    not transparent to other semi-transparent (non-Wave) pieces.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is semi-opaque, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_OPAQUE(pte)

    Macro to check if given piece is opaque.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is opaque, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_DIVERGENT(pte)

    Macro to check if given piece is divergent, i.e. can be diverged from.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece is divergent, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_CAN_BE_DIVERGED(pte)

    Macro to check if given piece can be diverged.

    .. note::

        Some pieces can be diverged only sometimes. For instance, Wave can be
        diverged if it's not activated by e.g. Centaur.

    .. note::

        All activated pieces when diverging are also restricted by momentum.
        For instance, Rook normally can diverge, except if it has no momentum
        when it encounters own Shaman.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can be diverged, :c:data:`false` otherwise.

.. c:macro:: CC_WAVE_CAN_BE_DIVERGED(activator)

    Macro to check if Wave can be diverged, based on its :term:`activator`.

    .. note::

        Wave can be diverged only if it's not activated by Unicorn, Centaur, or Serpent.

    :param activator: An :term:`activator`, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if Wave can be diverged, :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_SINGLE_STEP(pte)

    Macro to check if given piece has a single step, i.e. Knight.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can make a single step in a ply,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_ONE_STEP(pte)

    Macro to check if given piece moves in one step, i.e. Bishop.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece can choose one step in a ply,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_IS_TWO_STEP(pte)

    Macro to check if given piece moves in two alternating steps, i.e. Centaur.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece has two alternating steps,
              :c:data:`false` otherwise.

.. c:macro:: CC_WAVE_IS_TWO_STEP(activator)

    Macro to check if Wave moves in two alternating steps, which depends on
    :term:`activator`, i.e. if Wave is activated by Unicorn, Centaur, or Serpent.

    :param activator: An :term:`activator`, :c:type:`CcPieceTagType` value.
    :returns: :c:data:`true` if piece changes has two alternating steps,
              :c:data:`false` otherwise.

.. c:macro:: CC_PIECE_HAS_NEW_STEP_AFTER_EACH(pte)

    Macro to check if given piece can change its direction after each step.

    :param pte: A piece, :c:type:`CcPieceTagType` value.
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

        Equals to :c:`-29`.

    .. c:enumerator:: CC_PTE_DarkStarchild
    .. c:enumerator:: CC_PTE_DarkShaman
    .. c:enumerator:: CC_PTE_DarkSerpent

    .. c:enumerator:: CC_PTE_DarkGrenadier_RushedCurrent
    .. c:enumerator:: CC_PTE_DarkGrenadier_RushedPrevious
    .. c:enumerator:: CC_PTE_DarkGrenadier_CanRush
    .. c:enumerator:: CC_PTE_DarkGrenadier

    .. c:enumerator:: CC_PTE_DarkScout_RushedCurrent
    .. c:enumerator:: CC_PTE_DarkScout_RushedPrevious
    .. c:enumerator:: CC_PTE_DarkScout_CanRush
    .. c:enumerator:: CC_PTE_DarkScout

    .. c:enumerator:: CC_PTE_DarkCentaur
    .. c:enumerator:: CC_PTE_DarkWave
    .. c:enumerator:: CC_PTE_DarkUnicorn
    .. c:enumerator:: CC_PTE_DarkPyramid
    .. c:enumerator:: CC_PTE_DarkPegasus

    .. c:enumerator:: CC_PTE_DarkKing_CanCastle
    .. c:enumerator:: CC_PTE_DarkKing

    .. c:enumerator:: CC_PTE_DarkQueen

    .. c:enumerator:: CC_PTE_DarkRook_CanCastle
    .. c:enumerator:: CC_PTE_DarkRook

    .. c:enumerator:: CC_PTE_DarkBishop
    .. c:enumerator:: CC_PTE_DarkKnight

    .. c:enumerator:: CC_PTE_DarkPawn_DelayedPromotion
    .. c:enumerator:: CC_PTE_DarkPawn_RushedCurrent
    .. c:enumerator:: CC_PTE_DarkPawn_RushedPrevious
    .. c:enumerator:: CC_PTE_DarkPawn_CanRush
    .. c:enumerator:: CC_PTE_DarkPawn

    .. c:enumerator:: CC_PTE_None

        No piece present, equals to ``0``.
        Used for e.g. empty on-board fields, any off-board field.

    .. c:enumerator:: CC_PTE_LightPawn
    .. c:enumerator:: CC_PTE_LightPawn_CanRush
    .. c:enumerator:: CC_PTE_LightPawn_RushedPrevious
    .. c:enumerator:: CC_PTE_LightPawn_RushedCurrent
    .. c:enumerator:: CC_PTE_LightPawn_DelayedPromotion

    .. c:enumerator:: CC_PTE_LightKnight
    .. c:enumerator:: CC_PTE_LightBishop

    .. c:enumerator:: CC_PTE_LightRook
    .. c:enumerator:: CC_PTE_LightRook_CanCastle

    .. c:enumerator:: CC_PTE_LightQueen

    .. c:enumerator:: CC_PTE_LightKing
    .. c:enumerator:: CC_PTE_LightKing_CanCastle

    .. c:enumerator:: CC_PTE_LightPegasus
    .. c:enumerator:: CC_PTE_LightPyramid
    .. c:enumerator:: CC_PTE_LightUnicorn
    .. c:enumerator:: CC_PTE_LightWave
    .. c:enumerator:: CC_PTE_LightCentaur

    .. c:enumerator:: CC_PTE_LightScout
    .. c:enumerator:: CC_PTE_LightScout_CanRush
    .. c:enumerator:: CC_PTE_LightScout_RushedPrevious
    .. c:enumerator:: CC_PTE_LightScout_RushedCurrent

    .. c:enumerator:: CC_PTE_LightGrenadier
    .. c:enumerator:: CC_PTE_LightGrenadier_CanRush
    .. c:enumerator:: CC_PTE_LightGrenadier_RushedPrevious
    .. c:enumerator:: CC_PTE_LightGrenadier_RushedCurrent

    .. c:enumerator:: CC_PTE_LightSerpent
    .. c:enumerator:: CC_PTE_LightShaman
    .. c:enumerator:: CC_PTE_LightStarchild

    .. c:enumerator:: CC_PTE_BrightStar

        Equals to ``29``.

    .. c:enumerator:: CC_PTE_Monolith

    :c:`enum` is tagged with the same :c:enum:`CcPieceTagEnum` name.

.. c:type:: signed char CcPieceTagType

    Actual storage type, as used in :c:struct:`CcChessboard` :c:member:`board`;
    contains only enumerations from :c:enum:`CcPieceTagEnum`.

.. _lbl-libcc-ccpiece-interfaces:

Interfaces
----------

.. c:type:: char (*cc_piece_fp_char_value_t)( CcPieceTagType ptt )

    Function interface, i.e. function pointer type.

    :param ptt: :c:type:`CcPieceTagType` value.
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

.. c:function:: CcPieceTagType cc_piece_opposite( CcPieceTagType ptt )

    Function returning piece in opposite color (owner) to given piece.

    Dark pieces are converted to light ones, and vice versa.
    The same applies to dim, bright pieces, i.e. Stars.

    If piece has no owner, function returns given piece back unmodified.

    :param ptt: A piece.
    :returns: A given piece converted to its opposite color.

.. c:function:: char cc_piece_as_char( CcPieceTagType ptt )

    Function returning piece char, based on piece enum.

    Character returned is lowercase if piece is dark (dim), uppercase if piece
    is light (bright).

    Monoliths are always returned uppercase.

    If there is no piece (i.e. :c:enumerator:`CC_PTE_None` was given) space is returned.

    In case no valid piece enum was given, question mark (:c:`'?'`) is returned.

    :param ptt: A piece.
    :returns: A piece character.

.. c:function:: CcPieceTagType cc_piece_from_char( char piece, char tag )

    Function returning chess piece, based on a given piece and tag characters.

    For lowercase :c:`char` dark/dim piece is returned, otherwise light/bright one.

    Monolith is returned only for uppercase ``'M'`` :c:`char`.

    Space, unrecognized characters all yield :c:enumerator:`CC_PTE_None`.

    Tag characters are described in :ref:`lbl-libcc-cctags-characters`.
    Used tag :c:`char`\s are :c:macro:`CC_TAG_CHAR_CAN_RUSH`,
    :c:macro:`CC_TAG_CHAR_CAN_CASTLE`, :c:macro:`CC_TAG_CHAR_DELAYED_PROMOTION`,
    :c:macro:`CC_TAG_CHAR_RUSHED_PREVIOUS` and :c:macro:`CC_TAG_CHAR_RUSHED_CURRENT`;
    depending on a piece given, all other values yields just a piece, with no tag.

    :param piece: A piece character.
    :param tag: A tag character.
    :returns: Piece enumeration if valid piece and tag :c:`char`\s are given,
        otherwise :c:enumerator:`CC_PTE_None`.

.. c:function:: char const * cc_piece_label( CcPieceTagType ptt, bool capitalize, bool empty_field )

    Function returns a piece label.

    Piece label is capitalized name of a piece.

    Piece label is the same for dark (dim) and light (bright) pieces.

    For :c:enumerator:`CC_PTE_None` piece, label depends on :c:var:`capitalize`
    and :c:var:`empty_field` flags.

    All returned strings are null-terminated.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param ptt: A piece.
    :param capitalize: Flag, whether to return capitalized string; affects only
        :c:enumerator:`CC_PTE_None` piece, i.e. empty field.
    :param empty_field: Flag, whether to return :c:`"empty field"`, or empty string.
    :returns: Pointer to string if successful, :c:macro:`CC_DEFAULT_VALUE_STRING` otherwise.

.. c:function:: char cc_piece_symbol( CcPieceTagType ptt )

    Function returns a piece symbol, i.e. an uppercase :c:`char` for chess pieces.

    :param ptt: A piece.
    :returns: A piece symbol if chess piece, space otherwise.
    :seealso: :c:func:`cc_piece_as_char()`

.. c:function:: CcPieceTagType cc_piece_demoting_to( CcPieceTagType ptt )

    Function returns a Pawn to which given piece can be demoted to,
    or :c:enumerator:`CC_PTE_None` if piece can't be demoted.

    Dark pieces can be demoted to dark Pawn, similarly light pieces can be
    demoted to light Pawn.

    Stars, Monoliths (which don't have an owner) cannot be demoted to a Pawn,
    so :c:enumerator:`CC_PTE_None` is returned instead.

    :param ptt: A piece.
    :returns: Pawn to which given piece can be demoted to, otherwise :c:enumerator:`CC_PTE_None`.

.. c:function:: bool cc_piece_is_dark( CcPieceTagType ptt )

    Function checks if given chess piece is dark.

    :param ptt: A piece.
    :returns: :c:data:`true` if given piece is dark, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_light( CcPieceTagType ptt )

    Function checks if given chess piece is light.

    :param ptt: A piece.
    :returns: :c:data:`true` if given piece is light, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_color( CcPieceTagType ptt )

    Function checks if given chess piece has color, i.e. if it's either light, or dark.

    :param ptt: A piece.
    :returns: :c:data:`true` if given piece has color, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_shade( CcPieceTagType ptt )

    Function checks if given chess piece has shade, i.e. if it's either bright, or dim.

    :param ptt: A piece.
    :returns: :c:data:`true` if given piece has shade, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_prefix( CcPieceTagType ptt )

    Function checks if given chess piece has prefix, i.e. if it has either a color, or a shade.

    :param ptt: A piece.
    :returns: :c:data:`true` if given piece has prefix, :c:data:`false` otherwise.

.. c:function:: char const * cc_piece_prefix( CcPieceTagType ptt, bool capitalize )

    Function returns prefix of a given chess piece.

    Piece prefix is either a color, or a shade of a given piece,
    depending what it has.

    For pieces without neither color nor shade (:c:enumerator:`CC_PTE_None`, and Monolith),
    prefix is empty string.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param ptt: A piece.
    :param capitalize: Flag, whether to return capitalized prefix.
    :returns: Pointer to string if successful, :c:macro:`CC_DEFAULT_VALUE_STRING` otherwise.

.. c:function:: bool cc_piece_has_congruent_type( char symbol, CcPieceTagType ptt )

    Function checks if given piece has the same type as a piece symbol.

    :param symbol: Piece symbol, uppercase :c:`char`. It is taken verbatim, i.e. not converted to uppercase char.
    :param ptt: A piece.
    :returns: :c:data:`true` if the same type, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_equal( char symbol, bool is_light, CcPieceTagType ptt )

    Function checks if given piece is equal to one produced by a piece symbol, and a flag.

    :param symbol: Piece symbol, uppercase :c:`char`. It is taken verbatim, i.e. not converted to uppercase char.
    :param is_light: Flag, if piece is light/bright (:c:data:`true`), or dark/dim (:c:data:`false`).
    :param ptt: A piece.
    :returns: :c:data:`true` if the same, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_same_type( CcPieceTagType ptt_1, CcPieceTagType ptt_2 )

    Function checks if two given pieces are the same type.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces have the same type, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_same_color( CcPieceTagType ptt_1, CcPieceTagType ptt_2 )

    Function checks if two given pieces are the same color, i.e. if
    both are light or dark.

    Stars have shade (bright, dim), not color; Monoliths don't have
    neither color, nor shade; so, this function returns :c:data:`false`
    if any given piece is a Star, or a Monolith.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces have the same color, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_same_shade( CcPieceTagType ptt_1, CcPieceTagType ptt_2 )

    Function checks if two given pieces are the same shade, i.e. if
    both are bright or dim.

    Stars have shade (bright, dim); if any given piece is not a Star
    function returns :c:data:`false`.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces have the same shade, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_opposite( CcPieceTagType ptt_1, CcPieceTagType ptt_2 )

    Function checks if two given pieces of the same type are in opposite
    color (dark, light), or shade (dim, bright) to each other.

    Pieces with no color and no shade always return :c:data:`false`;
    these are empty fields (i.e. :c:enumerator:`CC_PTE_None`), Monolith pieces.

    Shades and colors belong to different types of pieces, so will always
    yield :c:data:`false`.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces have the same type, but opposite color or shade, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_same_owner( CcPieceTagType ptt_1, CcPieceTagType ptt_2 )

    Function checks if two given pieces have the same owner, i.e.
    if pieces are of the same color (dark, or light), not necessarily
    the same type.

    If any given piece is without owner (i.e. with no color) it'll always
    return :c:data:`false`; these are empty fields (i.e. :c:enumerator:`CC_PTE_None`),
    Monolith, and Stars.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces are both dark or light, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_has_different_owner( CcPieceTagType ptt_1, CcPieceTagType ptt_2 )

    Function checks if two given pieces belongs to different players,
    i.e. if pieces are in opposite colors (one is light, the other is dark),
    not necessarily the same type.

    If any given piece is without owner (i.e. with no color) it'll always
    return :c:data:`false`; these are empty fields (i.e. :c:enumerator:`CC_PTE_None`),
    Monolith, and Stars.

    :param pe_1: A piece.
    :param pe_2: The other piece.
    :returns: :c:data:`true` if given pieces are in opposite color, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_owned_figure( CcPieceTagType ptt )

    Function checks if given piece is an owned :term:`figure`.

    :param ptt: A piece.
    :returns: :c:data:`true` if given piece is an owned :term:`figure`, :c:data:`false` otherwise.

.. c:function:: bool cc_piece_is_figure( CcPieceTagType ptt )

    Function checks if given piece is a :term:`figure`.

    :param ptt: A piece.
    :returns: :c:data:`true` if given piece is a :term:`figure`, :c:data:`false` otherwise.

.. c:function:: char const * cc_piece_as_string( CcPieceTagType ptt, bool capitalize, bool empty_field )

    Function returns string, containing piece prefix and label.

    .. warning::

        Returned string is not allocated, do not :c:func:`free()` it.

    :param ptt: A piece.
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
