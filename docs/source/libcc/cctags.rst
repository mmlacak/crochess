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

.. _lbl-libcc-cctags-characters:

Characters
----------

All :c:`CC_TAG_CHAR_*` macro constants are used to render tag board on a
console.

.. c:macro:: CC_TAG_CHAR_NONE

    Equals to :c:`' '`, used when there is no tag. When displaying tags, it's used
    for empty fields.

.. c:macro:: CC_TAG_CHAR_INVALID

    Equals to :c:`'?'`, used for displaying chessboard, if invalid piece and/or
    tag encountered.

.. c:macro:: CC_TAG_CHAR_PIECE

    Equals to :c:`'!'`; when displaying tags, used to denote piece without tag.

.. c:macro:: CC_TAG_CHAR_CAN_RUSH

    Equals to ``'^'``, sets-up private which can rush. When displaying tags, it's
    used for privates with can rush tag.

.. c:macro:: CC_TAG_CHAR_CAN_CASTLE

    Equals to ``'&'``, sets-up Rook or King which can castle. When displaying tags,
    it's used for Rooks and Kings which can castle.

.. c:macro:: CC_TAG_CHAR_DELAYED_PROMOTION

    Equals to ``'='``, sets-up Pawn which can be promoted later. When displaying
    tags, it's used for Pawn with delayed promotion tag.

.. c:macro:: CC_TAG_CHAR_RUSHED_PREVIOUS

    Equals to :c:`':'`, sets-up private which rushed in previous turn. When
    displaying tags, it's used for privates with rushed in previous turn tag.

.. c:macro:: CC_TAG_CHAR_RUSHED_CURRENT

    Equals to :c:`';'`, sets-up private which rushed in current turn. When
    displaying tags, it's used for privates with rushed in current turn tag.

.. _lbl-libcc-cctags-functions:

Functions
---------

.. c:function:: char cc_tag_as_char( CcPieceTagType ptt )

    Function returning :term:`tag` char, based on tag enum.

    :param ptt: :c:type:`CcPieceTagType` value.
    :returns: Tag char, one of :c:`CC_TAG_CHAR_*` constants.

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

.. c:type:: unsigned char CcLosingTagType

    Actual (storage) type, used for :c:enum:`CcLosingTagEnum` values.

.. _lbl-libcc-cctags-losingtagfunctions:

Losing tag functions
--------------------

.. c:function:: char const * cc_losing_tag_symbol( CcLosingTagType ltt )

    Function returns losing tag symbol as used in :term:`AN`, based on lost tag.

    :param ltt: A lost tag, :c:type:`CcLosingTagType` value.
    :returns: Valid pointer to null-terminated string literal,
              do not try to :c:func:`free()` it.
              String can be empty, if tag cannot be lost.

.. c:function:: char const * cc_losing_tag_as_string( CcLosingTagType ltt, bool capitalize, bool no_tag )

    Function returning descriptive string as used in user messages,
    based on lost tag.

    :param ltt: A lost tag, :c:type:`CcLosingTagType` value.
    :param capitalize: Flag, whether string should be capitalized.
    :param no_tag: Flag, whether should also describe no-tag value.
    :returns: Valid pointer to null-terminated string literal,
              do not try to :c:func:`free()` it.
              String can be empty, if tag cannot be lost.

.. c:function:: CcLosingTagType cc_losing_tag_from_piece( CcPieceTagType ptt )

    Converts ordinary tag accompanying a given piece into a lost tag.

    Ordinary tag values without equivalent losing tag value are converted
    into :c:enumerator:`CC_LTE_NoneLost` instead.

    :param ptt: A piece and its tag, :c:type:`CcPieceTagType` value.
    :returns: :c:type:`CcLosingTagType` value.

.. c:function:: CcPieceTagType cc_set_piece_tag_from_losing( CcPieceTagType ptt, CcLosingTagType ltt, bool override_conflicting_tag )

    Sets ordinary tag to a given piece, based on a given lost tag.

    A given piece can have its tag already set, flag :c:var:`override_conflicting_tag`
    controls whether a given tag would be overwritten.

    :param ptt: A piece and its tag, :c:type:`CcPieceTagType` value.
    :param ltt: A lost tag, :c:type:`CcLosingTagType` value.
    :param override_conflicting_tag: Flag, whether to also override any conflicting tag,
        if it exists.
    :returns: A piece with its new tag set if successful, a given piece (and its
        original tag) otherwise.

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
