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
