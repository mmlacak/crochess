.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsedsideeffect:

Parsed side-effect
==================

Documents ``cc_parsed_side_effect.h`` and ``cc_parsed_side_effect.c`` files,
which contain various parsed side-effect definitions and functions.

.. _lbl-libcc-ccparsedsideeffect-data:

Parsed side-effect data
-----------------------

.. c:enum:: CcParsedSideEffectEnum

    Step side-effect enumeration.

    .. c:enumerator:: CC_PSEE_None

        No side effects.

    .. c:enumerator:: CC_PSEE_Capture

        Corresponds to ``*``.

    .. c:enumerator:: CC_PSEE_Displacement

        Corresponds to ``<``.

    .. c:enumerator:: CC_PSEE_EnPassant

        Corresponds to ``:``.

    .. c:enumerator:: CC_PSEE_Castle

        Corresponds to ``&``.

    .. c:enumerator:: CC_PSEE_Promotion

        Corresponds to ``=``, can be omitted.

    .. c:enumerator:: CC_PSEE_TagForPromotion

        Corresponds to ``=``, it's mandatory.

    .. c:enumerator:: CC_PSEE_Conversion

        Corresponds to ``%``.

    .. c:enumerator:: CC_PSEE_FailedConversion

        Corresponds to ``%%``.

    .. c:enumerator:: CC_PSEE_Transparency

        Corresponds to ``^``.

    .. c:enumerator:: CC_PSEE_Divergence

        Corresponds to ``/``.

    .. c:enumerator:: CC_PSEE_DemoteToPawn

        Corresponds to ``>``.

    .. c:enumerator:: CC_PSEE_Resurrection

        Corresponds to ``$``.

    .. c:enumerator:: CC_PSEE_ResurrectingOpponent

        Corresponds to ``$$``.

    .. c:enumerator:: CC_PSEE_FailedResurrection

        Corresponds to ``$$$``.

    :c:`enum` is tagged with the same :c:enum:`CcParsedSideEffectEnum` name.

.. c:macro:: CC_PARSED_SIDE_EFFECT_ENUM_IS_CASTLING(see)

    Macro to check if given side-effect enum is castling.

    :param see: A side-effect enumeration, i.e. one of :c:enum:`CcParsedSideEffectEnum` values.
    :returns: :c:data:`true` if castling, :c:data:`false` otherwise.

.. c:macro:: CC_MAX_LEN_PARSED_SIDE_EFFECT_SYMBOL

    Maximum length of a side-effect symbol; equals to ``3``.

.. c:struct:: CcParsedSideEffect

    Step side-effect structure.

    .. c:member:: CcParsedSideEffectEnum type

        Type of side-effect.

    .. c:union:: @data

        Union of all substructures used by different step side-effects.

        .. c:struct:: capture

            Capture.

            .. c:member:: CcPieceEnum piece

                Piece which has been captured.

            .. c:member:: CcLosingTagEnum lost_tag

                Tag lost by captured piece.

        .. c:struct:: displacement

            Displacement, used during light Shaman's trance-journey.

            .. c:member:: CcPieceEnum piece

                Piece which has been displaced.

            .. c:member:: CcLosingTagEnum lost_tag

                Tag lost by displaced piece.

            .. c:member:: CcPos destination

                Displacement destination.

        .. c:struct:: en_passant

            En passant.

            .. c:member:: CcPieceEnum pawn

                Pawn which has been captured.

            .. c:member:: CcPos distant

                Position at which Pawn has been captured.

        .. c:struct:: castle

            Castling.

            .. c:member:: CcPieceEnum rook

                Rook which castled.

            .. c:member:: CcPos start

                Starting position of the Rook.

            .. c:member:: CcPos destination

                Castling Rook destination.

        .. c:struct:: promote

            Promotion.

            .. c:member:: CcPieceEnum captured

                Piece which has been captured, if any.

            .. c:member:: CcLosingTagEnum lost_tag

                Tag lost by captured piece.

            .. c:member:: CcPieceEnum promoted_to

                Piece to which Pawn has been promoted.

        .. c:struct:: tag_for_promotion

            Tag for promotion.

            .. c:member:: CcPieceEnum captured

                Piece which has been captured, if any.

            .. c:member:: CcLosingTagEnum lost_tag

                Tag lost by captured piece.

        .. c:struct:: convert

            Conversion.

            .. c:member:: CcPieceEnum piece

                Piece which has been converted.

            .. c:member:: CcLosingTagEnum lost_tag

                Tag lost by converted piece.

        .. c:struct:: transparency

            Transparency.

            .. c:member:: CcPieceEnum piece

                Piece which has been "passed-over".

        .. c:struct:: diversion

            Divergence.

            .. c:member:: CcPieceEnum piece

                Piece from which currently moving piece diverted.

        .. c:struct:: demote

            Demoting.

            .. c:member:: CcPieceEnum piece

                Piece which has been demoted to Pawn.

            .. c:member:: CcLosingTagEnum lost_tag

                Tag lost by demoted piece.

            .. c:member:: CcPos distant

                Position at which piece has been demoted.

        .. c:struct:: resurrect

            Resurrection.

            .. c:member:: CcPieceEnum piece

                Piece which has been resurrected.

            .. c:member:: CcPos destination

                Position at which Wave, Starchild has been resurrected.

    :c:`struct` is tagged with the same :c:struct:`CcParsedSideEffect` name.

.. _lbl-libcc-ccparsedsideeffect-functions:

Parsed side-effect functions
----------------------------

.. c:function:: char const * cc_parsed_side_effect_symbol( CcParsedSideEffectEnum see )

    Function returns string symbol, as used in algebraic notation, for a given side-effect.

    Returned string is not allocated, so do not :c:func:`free()` it.

    :param see: A side-effect enum.
    :returns: String symbol if side-effect enum is valid, ``"?"`` otherwise.

.. c:function:: CcParsedSideEffect cc_parsed_side_effect( CcParsedSideEffectEnum type, CcPieceEnum piece, CcLosingTagEnum lost_tag, CcPos start, CcPos destination, CcPieceEnum promoted_to )

    Function returns step side-effect :c:`struct`\ure.

    :param type: Type of side-effect.
    :param piece: A piece.
    :param lost_tag: Tag lost by a piece.
    :param start: Starting position.
    :param destination: Destination position.
    :param promoted_to: Piece to which Pawn has been promoted.
    :returns: Step side-effect :c:`struct`\ure.

.. c:function:: CcPieceEnum cc_parsed_side_effect_piece( CcParsedSideEffect se )

    Function returns piece affected by a given side-effect.

    :param se: A side-effect.
    :returns: A piece affected by a side-effect.






.. _lbl-libcc-ccparsedsideeffect-sourcecodeheader:

Parsed side-effect source code header
-------------------------------------

Included source code file is ``cc_parsed_side_effect.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parsed_side_effect.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsedsideeffect-sourcecodefile:

Parsed side-effect source code file
-----------------------------------

Included source code file is ``cc_parsed_side_effect.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parsed_side_effect.c
    :language: C
    :linenos:
