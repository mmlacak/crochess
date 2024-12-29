.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccsideeffect:

Side-effect
===========

Documents ``cc_side_effect.h`` and ``cc_side_effect.c`` files,
which contain various parsed side-effect definitions and functions.

.. _lbl-libcc-ccsideeffect-macros:

Macros
------

.. c:macro:: CC_SIDE_EFFECT_TYPE_IS_ENUMERATOR(sete)

    Macro to check if given side-effect type is enumeration in :c:enum:`CcSideEffectTypeEnum`,
    i.e. between :c:enumerator:`CC_SETE_None` and :c:enumerator:`CC_SETE_FailedResurrection`
    values.

    :param sete: A side-effect type, integer value.
    :returns: :c:data:`true` if :c:type:`CcSideEffectTypeEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_SIDE_EFFECT_TYPE_IS_VALID(sete)

    Macro to check if given side-effect type is valid :c:enum:`CcSideEffectTypeEnum`
    enumerator, and not :c:enumerator:`CC_SETE_None`.

    :param sete: A side-effect type, integer value.
    :returns: :c:data:`true` if valid :c:type:`CcSideEffectTypeEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_SIDE_EFFECT_TYPE_MUST_BE_FOLLOWED_BY_STEP(sete)

    Macro to check if given side-effect must be followed by at least one step,
    i.e. if side-effect is transparency or divergence.

    :param sete: A side-effect type, integer value.
    :returns: :c:data:`true` if side-effect must be followed by a step,
              :c:data:`false` otherwise.

.. c:macro:: CC_MAX_LEN_SIDE_EFFECT_TYPE_SYMBOL

    Maximum length of a side-effect symbol; equals to ``3``.

.. _lbl-libcc-ccsideeffect-data:

Data
----

.. c:enum:: CcSideEffectTypeEnum

    Step side-effect enumeration.

    .. c:enumerator:: CC_SETE_None

        No side effects.

    .. c:enumerator:: CC_SETE_Capture

        Corresponds to ``*``.

    .. c:enumerator:: CC_SETE_Displacement

        Corresponds to ``<``.

    .. c:enumerator:: CC_SETE_EnPassant

        Corresponds to ``:``.

    .. c:enumerator:: CC_SETE_Castle

        Corresponds to ``&``.

    .. c:enumerator:: CC_SETE_Promotion

        Corresponds to ``=``, can be omitted.

    .. c:enumerator:: CC_SETE_TagForPromotion

        Corresponds to ``=``, it's mandatory.

    .. c:enumerator:: CC_SETE_Conversion

        Corresponds to ``%``.

    .. c:enumerator:: CC_SETE_FailedConversion

        Corresponds to ``%%``.

    .. c:enumerator:: CC_SETE_Transparency

        Corresponds to ``^``.

    .. c:enumerator:: CC_SETE_Divergence

        Corresponds to ``/``.

    .. c:enumerator:: CC_SETE_DemoteToPawn

        Corresponds to ``>``.

    .. c:enumerator:: CC_SETE_Resurrection

        Corresponds to ``$``.

    .. c:enumerator:: CC_SETE_ResurrectingOpponent

        Corresponds to ``$$``.

    .. c:enumerator:: CC_SETE_FailedResurrection

        Corresponds to ``$$$``.

    :c:`enum` is tagged with the same :c:enum:`CcSideEffectTypeEnum` name.

.. c:struct:: CcSideEffect

    Step side-effect structure.

    .. c:member:: CcSideEffectTypeEnum type

        Type of side-effect.

    .. c:union:: @data

        Union of all substructures used by different step side-effects.

        .. c:struct:: capture

            Capture.

            .. c:member:: CcPieceType piece

                Piece which has been captured.

            .. c:member:: CcLosingTagType lost_tag

                Tag lost by captured piece.

        .. c:struct:: displacement

            Displacement, used during light Shaman's trance-journey.

            .. c:member:: CcPieceType piece

                Piece which has been displaced.

            .. c:member:: CcLosingTagType lost_tag

                Tag lost by displaced piece.

            .. c:member:: CcPos destination

                Displacement destination.

        .. c:struct:: en_passant

            En passant.

            .. c:member:: CcPieceType pawn

                Pawn which has been captured.

            .. c:member:: CcPos distant

                Position at which Pawn has been captured.

        .. c:struct:: castle

            Castling.

            .. c:member:: CcPieceType rook

                Rook which castled.

            .. c:member:: CcPos start

                Starting position of the Rook.

            .. c:member:: CcPos destination

                Castling Rook destination.

        .. c:struct:: promote

            Promotion.

            .. c:member:: CcPieceType captured

                Piece which has been captured, if any.

            .. c:member:: CcLosingTagType lost_tag

                Tag lost by captured piece.

            .. c:member:: CcPieceType promoted_to

                Piece to which Pawn has been promoted.

        .. c:struct:: tag_for_promotion

            Tag for promotion.

            .. c:member:: CcPieceType captured

                Piece which has been captured, if any.

            .. c:member:: CcLosingTagType lost_tag

                Tag lost by captured piece.

        .. c:struct:: convert

            Conversion.

            .. c:member:: CcPieceType piece

                Piece which has been converted.

            .. c:member:: CcLosingTagType lost_tag

                Tag lost by converted piece.

        .. c:struct:: transparency

            Transparency.

            .. c:member:: CcPieceType piece

                Piece which has been "passed-over".

        .. c:struct:: diversion

            Divergence.

            .. c:member:: CcPieceType piece

                Piece from which currently moving piece diverted.

        .. c:struct:: demote

            Demoting.

            .. c:member:: CcPieceType piece

                Piece which has been demoted to Pawn.

            .. c:member:: CcLosingTagType lost_tag

                Tag lost by demoted piece.

            .. c:member:: CcPos distant

                Position at which piece has been demoted.

        .. c:struct:: resurrect

            Resurrection.

            .. c:member:: CcPieceType piece

                Piece which has been resurrected.

            .. c:member:: CcPos destination

                Position at which Wave, Starchild has been resurrected.

    :c:`struct` is tagged with the same :c:struct:`CcSideEffect` name.

.. _lbl-libcc-ccsideeffect-functions:

Functions
---------

.. c:function:: char const * cc_side_effect_type_symbol( CcSideEffectTypeEnum see )

    Function returns string symbol, as used in algebraic notation, for a given side-effect.

    Returned string is not allocated, so do not :c:func:`free()` it.

    :param see: A side-effect enum.
    :returns: String symbol if side-effect enum is valid, ``"?"`` otherwise.

.. c:function:: CcSideEffect cc_side_effect( CcSideEffectTypeEnum type, CcPieceType piece, CcLosingTagType lost_tag, CcPos start, CcPos destination, CcPieceType promoted_to )

    Function returns step side-effect :c:`struct`\ure.

    :param type: Type of side-effect.
    :param piece: A piece.
    :param lost_tag: Tag lost by a piece.
    :param start: Starting position.
    :param destination: Destination position.
    :param promoted_to: Piece to which Pawn has been promoted.
    :returns: Step side-effect :c:`struct`\ure.

.. c:function:: CcPieceType cc_side_effect_piece( CcSideEffect se )

    Function returns piece affected by a given side-effect.

    :param se: A side-effect.
    :returns: A piece affected by a side-effect.

.. c:function:: CcPos cc_side_effect_destination( CcSideEffect se )

    Function returns position affected by a given side-effect.

    :param se: A side-effect.
    :returns: A position affected by a side-effect.

.. c:function:: bool cc_side_effect_to_short_str( CcSideEffect se, cc_char_16 * se_str__o )

    Function returns string, containing user-readable representation
    of a given side-effect.

    :param se: A side-effect.
    :param se_str__o: *Output* parameter; pointer to array which will hold string.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-ccsideeffect-structfunctions:

:c:`struct` functions
^^^^^^^^^^^^^^^^^^^^^

    The side-effect convenience functions are meant to be used instead
    of :c:func:`cc_side_effect()`.

    They have minimal set of arguments required by the type of a side-effect,
    otherwise they behave exactly as their generic progenitor.

.. c:function:: CcSideEffect cc_side_effect_none( void )

.. c:function:: CcSideEffect cc_side_effect_capture( CcPieceType piece, CcLosingTagType lost_tag )

.. c:function:: CcSideEffect cc_side_effect_displacement( CcPieceType piece, CcLosingTagType lost_tag, CcPos destination )

.. c:function:: CcSideEffect cc_side_effect_en_passant( CcPieceType pawn, CcPos distant )

.. c:function:: CcSideEffect cc_side_effect_castle( CcPieceType rook, CcPos start, CcPos destination )

.. c:function:: CcSideEffect cc_side_effect_promote( CcPieceType captured, CcLosingTagType lost_tag, CcPieceType promoted_to )

.. c:function:: CcSideEffect cc_side_effect_tag_for_promotion( CcPieceType captured, CcLosingTagType lost_tag )

.. c:function:: CcSideEffect cc_side_effect_convert( CcPieceType piece, CcLosingTagType lost_tag )

.. c:function:: CcSideEffect cc_side_effect_failed_conversion( void )

.. c:function:: CcSideEffect cc_side_effect_transparency( CcPieceType piece )

.. c:function:: CcSideEffect cc_side_effect_diversion( CcPieceType piece )

.. c:function:: CcSideEffect cc_side_effect_demote( CcPieceType piece, CcLosingTagType lost_tag, CcPos distant )

.. c:function:: CcSideEffect cc_side_effect_resurrect( CcPieceType piece, CcPos destination )

.. c:function:: CcSideEffect cc_side_effect_failed_resurrection( void )

.. _lbl-libcc-ccsideeffect-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_side_effect.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_side_effect.h
    :language: C
    :linenos:

.. _lbl-libcc-ccsideeffect-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_side_effect.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_side_effect.c
    :language: C
    :linenos:
