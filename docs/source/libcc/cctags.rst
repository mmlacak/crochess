.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-cctags:

Tags
====

Documents ``cc_tag.h`` and ``cc_tag.c`` files, which contain :term:`tag`
enumeration, and related functions.

:c:term:`Tag` is a link between a piece and field at which it stands.
Every piece can have only one tag applied at any given time.

Persistent :c:term:`tag`\s are valid until used or lost, e.g. until
piece is moved, activated, converted, captured, displaced, teleported,
demoted (if figure), promoted (if Pawn).

Values enumerated in losing tag are the same as in ordinary :c:term:`tag`,
but only for :c:term:`tag`\s that can be lost.

.. _lbl-libcc-cctags-validity:

Validity
--------

.. c:macro:: CC_TAG_IS_ENUMERATOR(te)

    Macro to check if given :term:`tag` is enumeration in :c:enum:`CcTagEnum`,
    i.e. between :c:enumerator:`CC_TE_None` and :c:enumerator:`CC_TE_DelayedPromotion`
    values.

    :param te: A tag, integer value.
    :returns: :c:data:`true` if :c:type:`CcTagEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_TAG_IS_VALID(te)

    Macro to check if given :term:`tag` is valid :c:enum:`CcTagEnum` enumerator,
    and not :c:enumerator:`CC_TE_None`.

    :param te: A tag, integer value.
    :returns: :c:data:`true` if valid :c:type:`CcTagEnum` enumerator,
              :c:data:`false` otherwise.

.. _lbl-libcc-cctags-values:

Values
------

.. c:macro:: CC_TAG_IS_PERSISTENT(te)

    Macro to check if given :term:`tag` is persistent,
    i.e. if it lasts until used or lost.

    :param te: :c:type:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_RUSHED(te)

    Macro to check if given :term:`tag` is en passant, i.e. either
    :c:enumerator:`CC_TE_EnPassant_Previous` or
    :c:enumerator:`CC_TE_EnPassant_Current`.

    :param te: :c:type:`CcTagEnum` value.
    :returns: :c:`bool` value, :c:data:`true` if en passant tag, :c:data:`false`
        otherwise.

.. _lbl-libcc-cctags-characters:

Characters
----------

All :c:`CC_TAG_CHAR_*` macro constants are used to render tag board on a
console.

.. c:macro:: CC_TAG_CHAR_NONE

    Equals to :c:`' '`.

.. c:macro:: CC_TAG_CHAR_INVALID

    Equals to :c:`'?'`.

.. c:macro:: CC_TAG_CHAR_CAN_RUSH

    Equals to ``'R'``.

.. c:macro:: CC_TAG_CHAR_CAN_CASTLE

    Equals to ``'C'``.

.. c:macro:: CC_TAG_CHAR_DELAYED_PROMOTION

    Equals to ``'P'``.

.. c:macro:: CC_TAG_CHAR_RUSHED_PREVIOUS

    Equals to :c:`'E'`.

.. c:macro:: CC_TAG_CHAR_RUSHED_CURRENT

    Equals to :c:`'e'`.

.. _lbl-libcc-cctags-types:

Types
-----

.. c:enum:: CcTagEnum

    Enumerates all :term:`tag`\s, used in all variants.

    .. c:enumerator:: CC_TE_None

        No :term:`tag` applies, equals to ``0``.
        Used for e.g. empty on-board fields, any off-board field.

    .. c:enumerator:: CC_TE_CanRush

        Pawn can rush. Persistent :term:`tag`, equals to ``1``.

    .. c:enumerator:: CC_TE_CanCastle

        Rooks, Kings can castle. Persistent :term:`tag`, equals to ``2``.

    .. c:enumerator:: CC_TE_DelayedPromotion

        Pawn delayed promotion. Persistent :term:`tag`, equals to ``3``.

    .. c:enumerator:: CC_TE_EnPassant_Previous

        A private rushed in previous turn, this is its en passant opportunity tag;
        deleted at the start of the very next move.
        Semi-persistent :term:`tag`, equals to ``4``.

    .. c:enumerator:: CC_TE_EnPassant_Current

        A private rushed in current turn (in a previous ply), this is its en passant
        opportunity tag; becomes :c:enumerator:`CC_TE_EnPassant_Previous` at the
        start of the very next move.
        Semi-persistent :term:`tag`, equals to ``5``.

    :c:`enum` is tagged with the same :c:enum:`CcTagEnum` name.

.. c:type:: unsigned char CcTagType

    Actual storage type, as used in :c:struct:`CcChessboard` :c:member:`tags`;
    contains only enumerations from :c:enum:`CcTagEnum`.

.. _lbl-libcc-cctags-functions:

Functions
---------

.. c:function:: char cc_tag_as_char( CcTagType ct )

    Function returning :term:`tag` char, based on tag enum.

    :param ct: :c:type:`CcTagType` value.
    :returns: Tag char, one of :c:`CC_TAG_CHAR_*` constants.

.. c:function:: CcTagType cc_tag_from_char( char c )

    Function returning :term:`tag` enum, based on :term:`tag` char.

    :param c: A char, expected to be one of :c:`CC_TAG_CHAR_*` constants.
    :returns: :c:type:`CcTagType` value if valid :term:`tag` character was given,
              :c:enumerator:`CC_TE_None` otherwise.

.. c:function:: bool cc_tag_is_congruent( CcTagType ct_1, CcTagType ct_2 )

    Function checks if two tags are congruent.

    For tags to be congruent, at least both has to be enumerations.
    If both are valid enumerations, they also have to be the same.

    So, if any tag is :c:enumerator:`CC_TE_None`, the other just have
    to be any :c:type:`CcTagType` enumeration.

    :param ct_1: A tag, :c:type:`CcTagType` value.
    :param ct_2: Other tag, :c:type:`CcTagType` value.
    :returns: :c:data:`true` if tags are congruent, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_TAG_IS_ENUMERATOR()`, :c:macro:`CC_TAG_IS_VALID()`

.. _lbl-libcc-cctags-losingtagvalidity:

Losing tag validity
-------------------

.. c:macro:: CC_LOSING_TAG_IS_ENUMERATOR(ltt)

    Macro to check if given :term:`tag` is enumeration in :c:enum:`CcLosingTagEnum`,
    i.e. between :c:enumerator:`CC_LTE_NoneLost` and :c:enumerator:`CC_LTE_DelayedPromotionLost`
    values.

    :param ltt: A tag, integer value.
    :returns: :c:data:`true` if :c:enum:`CcLosingTagEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_LOSING_TAG_IS_VALID(ltt)

    Macro to check if given :term:`tag` is valid :c:enum:`CcLosingTagEnum` enumerator,
    and not :c:enumerator:`CC_LTE_NoneLost`.

    :param ltt: A tag, integer value.
    :returns: :c:data:`true` if valid :c:enum:`CcLosingTagEnum` enumerator,
              :c:data:`false` otherwise.

.. _lbl-libcc-cctags-losingtagtypes:

Losing tag types
----------------

.. c:enum:: CcLosingTagEnum

    Enumerates only :term:`tag`\s that can be lost, used in all variants.

    Values enumerated in losing tag are the same as in ordinary tag,
    but doesn't contain move-starter flag.

    When converting from ordinary tag enum, :c:enumerator:`CC_LTE_NoneLost`
    is used for all values not enumerated here.

    .. c:enumerator:: CC_LTE_NoneLost

        No :term:`tag` was lost, equals to :c:enumerator:`CC_TE_None`.

    .. c:enumerator:: CC_LTE_RushingTagLost

        Pawn lost ability to rush, equals to :c:enumerator:`CC_TE_CanRush`.

    .. c:enumerator:: CC_LTE_CastlingTagLost

        Rook or King lost ability to castle, equals to :c:enumerator:`CC_TE_CanCastle`.

    .. c:enumerator:: CC_LTE_DelayedPromotionLost

        Pawn lost delayed promotion :term:`tag`, equals to :c:enumerator:`CC_TE_DelayedPromotion`.

    :c:`enum` is tagged with the same :c:enum:`CcLosingTagEnum` name.

.. c:macro:: CC_MAX_LEN_LOSING_TAG_SYMBOL

    Maximum length of a losing-tag symbol, equals to ``2``.

.. c:type:: CcTagType CcLosingTagType

    Actual (storage) type, used for :c:enum:`CcLosingTagEnum` values;
    equals to :c:`unsigned char`.

.. _lbl-libcc-cctags-losingtagfunctions:

Losing tag functions
--------------------

.. c:function:: char const * cc_losing_tag_symbol( CcLosingTagType ltt )

    Function returns losing tag symbol as used in :term:`AN`, based on lost tag.

    :param ltt: :c:type:`CcLosingTagType` value.
    :returns: Valid pointer to null-terminated string literal,
              do not try to :c:func:`free()` it.
              String can be empty, if tag cannot be lost.

.. c:function:: char const * cc_losing_tag_as_string( CcLosingTagType ltt, bool capitalize, bool no_tag )

    Function returning descriptive string as used in user messages,
    based on lost tag.

    :param ltt: :c:type:`CcLosingTagType` value.
    :param capitalize: Flag, whether string should be capitalized.
    :param no_tag: Flag, whether should also describe no-tag value.
    :returns: Valid pointer to null-terminated string literal,
              do not try to :c:func:`free()` it.
              String can be empty, if tag cannot be lost.

.. c:function:: CcLosingTagType cc_convert_tag_to_losing( CcTagType te )

    Converts ordinary tag into lost tag.

    Ordinary tag values without equivalent losing tag value are converted
    into :c:enumerator:`CC_LTE_NoneLost` instead.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:type:`CcLosingTagType` value.

.. c:function:: CcTagType cc_convert_tag_from_losing( CcLosingTagType ltt )

    Converts losing tag into ordinary tag.

    :param ltt: :c:type:`CcLosingTagType` value.
    :returns: :c:type:`CcTagType` value.

.. c:function:: bool cc_losing_tag_is_congruent( CcLosingTagType ltt_1, CcLosingTagType ltt_2 )

    Function checks if two losing tags are congruent.

    For losing tags to be congruent, at least both has to be enumerations.
    If both are valid enumerations, they also have to be the same.

    So, if any losing tag is :c:enumerator:`CC_LTE_NoneLost`, the other just have
    to be any :c:type:`CcLosingTagType` enumeration.

    :param ltt_1: A losing tag, :c:type:`CcLosingTagType` value.
    :param ltt_2: Other losing tag, :c:type:`CcLosingTagType` value.
    :returns: :c:data:`true` if losing tags are congruent, :c:data:`false` otherwise.
    :seealso: :c:macro:`CC_LOSING_TAG_IS_ENUMERATOR()`, :c:macro:`CC_LOSING_TAG_IS_VALID()`

.. _lbl-libcc-cctags-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_tag.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_tag.h
    :language: C
    :linenos:

.. _lbl-libcc-cctags-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_tag.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_tag.c
    :language: C
    :linenos:
