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

.. c:macro:: CC_SIDE_EFFECT_TYPE_DOES_NOT_TERMINATE_PLY(sete)

    Macro to check if given side-effect does not terminate ply, and must be
    followed by at least one step, i.e. if it's transparency or divergence.

    :param sete: A side-effect type, integer value.
    :returns: :c:data:`true` if side-effect must be followed by a step,
              :c:data:`false` otherwise.

.. c:macro:: CC_SIDE_EFFECT_TYPE_MAY_TERMINATE_PLY(sete)

    Macro to check if a ply may end with a given side-effect type, i.e. if
    side-effect is terminal in most situations, but not all.

    :param sete: A side-effect type, integer value.
    :returns: :c:data:`true` if side-effect may terminate a ply,
              :c:data:`false` otherwise.

.. c:macro:: CC_SIDE_EFFECT_TYPE_TERMINATES_PLY(sete)

    Macro to check if a given side-effect type always ends a ply.

    :param sete: A side-effect type, integer value.
    :returns: :c:data:`true` if side-effect can terminate a ply,
              :c:data:`false` otherwise.

.. c:macro:: CC_SIDE_EFFECT_TYPE_CAN_TERMINATE_PLY(sete)

    Macro to check if a given side-effect is either definitely or maybe terminating
    a ply.

    :param sete: A side-effect type, integer value.
    :returns: :c:data:`true` if side-effect can terminate a ply,
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

            .. c:member:: CcPieceTagType piece

                Piece which has been captured, and its (lost) tag.

        .. c:struct:: displacement

            Displacement, used during light Shaman's trance-journey.

            .. c:member:: CcPieceTagType piece

                Piece which has been displaced, and its (lost) tag.

            .. c:member:: CcPos destination

                Displacement destination.

        .. c:struct:: en_passant

            En passant.

            .. c:member:: CcPieceTagType pawn

                Pawn which has been captured.

            .. c:member:: CcPos distant

                Position at which Pawn has been captured.

        .. c:struct:: castle

            Castling.

            .. c:member:: CcPieceTagType rook

                Rook which castled.

            .. c:member:: CcPos start

                Starting position of the Rook.

            .. c:member:: CcPos destination

                Castling Rook destination.

        .. c:struct:: promote

            Promotion.

            .. c:member:: CcPieceTagType captured

                Piece which has been captured, if any, and its (lost) tag.

            .. c:member:: CcPieceTagType promoted_to

                Piece to which Pawn has been promoted.

        .. c:struct:: tag_for_promotion

            Tag for promotion.

            .. c:member:: CcPieceTagType captured

                Piece which has been captured, if any, and its (lost) tag.

        .. c:struct:: convert

            Conversion.

            .. c:member:: CcPieceTagType piece

                Piece which has been converted, and its (lost) tag.

        .. c:struct:: transparency

            Transparency.

            .. c:member:: CcPieceTagType piece

                Piece which has been "passed-over".

        .. c:struct:: diversion

            Divergence.

            .. c:member:: CcPieceTagType piece

                Piece from which currently moving piece diverted.

        .. c:struct:: demote

            Demoting.

            .. c:member:: CcPieceTagType piece

                Piece which has been demoted to Pawn, and its (lost) tag.

            .. c:member:: CcPos distant

                Position at which piece has been demoted.

        .. c:struct:: resurrect

            Resurrection.

            .. c:member:: CcPieceTagType piece

                Piece which has been resurrected.

            .. c:member:: CcPos destination

                Position at which Wave, Starchild has been resurrected.

    :c:`struct` is tagged with the same :c:struct:`CcSideEffect` name.

.. _lbl-libcc-ccsideeffect-functions:

Functions
---------

.. c:function:: char const * cc_side_effect_type_symbol( CcSideEffectTypeEnum sete )

    Function returns string symbol, as used in algebraic notation, for a given side-effect.

    Returned string is not allocated, so do not :c:func:`free()` it.

    :param sete: A side-effect enum.
    :returns: String symbol if side-effect enum is valid, ``"?"`` otherwise.

.. c:function:: CcMaybeBoolEnum cc_side_effect_type_is_terminating( CcPieceTagType piece, CcSideEffectTypeEnum sete )

    Function returns if a given side-effect is terminating ply of a given piece.

    :param piece: A piece.
    :param sete: A side-effect enum.
    :returns: One of :c:enum:`CcMaybeBoolEnum` values:

        * :c:enumerator:`CC_MBE_True` if piece terminates its ply after a given side-effect,
        * :c:enumerator:`CC_MBE_False` if piece can continue its ply after a given side-effect,
        * :c:enumerator:`CC_MBE_Void` in case of an error, insufficient data given.

.. c:function:: CcSideEffect cc_side_effect( CcSideEffectTypeEnum type, CcPieceTagType piece, CcLosingTagType lost_tag, CcPos start, CcPos destination, CcPieceTagType promoted_to )

    Function returns step side-effect :c:`struct`\ure.

    :param type: Type of side-effect.
    :param piece: A piece.
    :param lost_tag: Tag lost by a piece.
    :param start: Starting position.
    :param destination: Destination position.
    :param promoted_to: Piece to which Pawn has been promoted.
    :returns: Step side-effect :c:`struct`\ure.

.. c:function:: CcPieceTagType cc_side_effect_piece( CcSideEffect se )

    Function returns piece affected by a given side-effect.

    :param se: A side-effect.
    :returns: A piece affected by a side-effect.

.. c:function:: CcPos cc_side_effect_destination( CcSideEffect se )

    Function returns position affected by a given side-effect.

    :param se: A side-effect.
    :returns: A position affected by a side-effect.

.. c:function:: bool cc_side_effect_has_destination( CcSideEffect se )

    Function returns if a given side-effect has a destination.

    :param se: A side-effect.
    :returns: :c:data:`true` if destination is present, :c:data:`false` otherwise.

.. c:function:: bool cc_side_effect_to_str( CcSideEffect se, cc_char_16 * se_str__o )

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

.. c:function:: CcSideEffect cc_side_effect_capture( CcPieceTagType piece )

.. c:function:: CcSideEffect cc_side_effect_displacement( CcPieceTagType piece, CcPos destination )

.. c:function:: CcSideEffect cc_side_effect_en_passant( CcPieceTagType pawn, CcPos distant )

.. c:function:: CcSideEffect cc_side_effect_castle( CcPieceTagType rook, CcPos start, CcPos destination )

.. c:function:: CcSideEffect cc_side_effect_promote( CcPieceTagType captured, CcPieceTagType promoted_to )

.. c:function:: CcSideEffect cc_side_effect_tag_for_promotion( CcPieceTagType captured )

.. c:function:: CcSideEffect cc_side_effect_convert( CcPieceTagType piece )

.. c:function:: CcSideEffect cc_side_effect_failed_conversion( void )

.. c:function:: CcSideEffect cc_side_effect_transparency( CcPieceTagType piece )

.. c:function:: CcSideEffect cc_side_effect_diversion( CcPieceTagType piece )

.. c:function:: CcSideEffect cc_side_effect_demote( CcPieceTagType piece, CcPos distant )

.. c:function:: CcSideEffect cc_side_effect_resurrect( CcPieceTagType piece, CcPos destination )

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
