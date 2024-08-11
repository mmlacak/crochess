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

Each :term:`tag` can be also combined with a flag, indicating that a holding
piece has started a move.

Persistent :c:term:`tag`\s are valid until used or lost, e.g. until
piece is moved, activated, converted, captured, displaced, teleported,
demoted (if figure), promoted (if Pawn).

Values enumerated in losing tag are the same as in ordinary :c:term:`tag`,
but they don't feature move-starter flag.

.. _lbl-libcc-cctags-masks:

Masks
-----

.. c:macro:: CC_TAG_VALUE_MASK

    Macro constant to filter out move-starter flag, leaving only a tag;
    equal to ``0x07``.

.. c:macro:: CC_MOVE_STARTER_MASK

    Macro constant to filter out tag, leaving only move-starter flag;
    equal to ``0x80``.

.. c:macro:: CC_TAG_VALUE(te)

    Macro to filter out move-starter flag, leaving only a tag.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:type:`CcTagType` value containing only given tag, and with
              move-starter flag removed.

.. c:macro:: CC_MOVE_STARTER_FLAG(te)

    Macro to filter out tag, leaving only move-starter flag.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:type:`CcTagType` value containing only given move-starter flag,
              and with tag removed.

.. c:macro:: CC_SET_MOVE_STARTER_FLAG(te)

    Macro to set move-starter flag, for a given tag.

    :param te: :c:type:`CcTagType` value.
    :returns: Given :c:type:`CcTagType` value, with move-starter flag set.

.. _lbl-libcc-cctags-validity:

Validity
--------

.. c:macro:: CC_TAG_IS_ENUMERATOR(te)

    Macro to check if given :term:`tag` is enumeration in :c:enum:`CcTagEnum`,
    i.e. between :c:enumerator:`CC_TE_None` and :c:enumerator:`CC_TE_PawnSacrifice`
    values.

    Move-starter flag is not tested, its 1-bit storage exactly covers both
    states it can be in.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_VALID(te)

    Macro to check if given :term:`tag` is valid, and not :c:enumerator:`CC_TE_None`.

    Move-starter flag is not tested, its 1-bit storage exactly covers both
    states it can be in.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_EQUAL(te1,te2)

    Macro to check if given :term:`tag`\s are the same.

    :param te1: :c:type:`CcTagType` value.
    :param te2: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_EQUIVALENT(te1,te2)

    Macro to check if given :term:`tag`\s are equivalent.

    Tags are equivalent, if they are the same when stripped of move-starter flag.

    :param te1: :c:type:`CcTagType` value.
    :param te2: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_NONE(te)

    Macro to check if given :term:`tag` is :c:enumerator:`CC_TE_None`.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. _lbl-libcc-cctags-values:

Values
------

.. c:macro:: CC_TAG_CAN_RUSH(te)

    Macro to check if given :term:`tag` is :c:enumerator:`CC_TE_CanRush`.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_CASTLE(te)

    Macro to check if given :term:`tag` is :c:enumerator:`CC_TE_CanCastle`.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_PROMOTE(te)

    Macro to check if given :term:`tag` is :c:enumerator:`CC_TE_DelayedPromotion`.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_EN_PASSANT(te)

    Macro to check if given :term:`tag` is :c:enumerator:`CC_TE_EnPassant`.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_PAWN_SACRIFICE(te)

    Macro to check if given :term:`tag` is :c:enumerator:`CC_TE_PawnSacrifice`.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_PERSISTENT(te)

    Macro to check if given :term:`tag` is persistent,
    i.e. if it lasts until used or lost.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_TEMPORARY(te)

    Macro to check if given :term:`tag` is temporary,
    i.e. lasts at most a single move.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. _lbl-libcc-cctags-characters:

Characters
----------

All :c:`CC_TAG_CHAR_*` macro constants are used to render tag board on a
console.

Move-starter macros (i.e. of the form :c:`CC_TAG_CHAR_MOVE_STARTER_*`) are
used for tags linked to pieces which started a move, see
:c:enumerator:`CC_TE_MoveStarterFlag` for details.

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

.. c:macro:: CC_TAG_CHAR_EN_PASSANT

    Equals to ``'E'``.

.. c:macro:: CC_TAG_CHAR_PAWN_SACRIFICE

    Equals to ``'S'``.

.. c:macro:: CC_TAG_CHAR_MOVE_STARTER_CAN_RUSH

    Equals to :c:`'r'`.

.. c:macro:: CC_TAG_CHAR_MOVE_STARTER_CAN_CASTLE

    Equals to :c:`'c'`.

.. c:macro:: CC_TAG_CHAR_MOVE_STARTER_DELAYED_PROMOTION

    Equals to :c:`'p'`.

.. c:macro:: CC_TAG_CHAR_MOVE_STARTER_EN_PASSANT

    Equals to :c:`'e'`.

.. c:macro:: CC_TAG_CHAR_MOVE_STARTER_PAWN_SACRIFICE

    Equals to :c:`'s'`.

.. _lbl-libcc-cctags-types:

Types
-----

.. c:enum:: CcTagEnum

    Enumerates all :term:`tag`\s, used in all variants; also includes flag for
    pieces that started a move.

    .. c:enumerator:: CC_TE_None

        No :term:`tag` applies, equals to ``0``.
        Used for e.g. empty on-board fields, any off-board field.

    .. c:enumerator:: CC_TE_CanRush

        Pawn can rush. Persistent :term:`tag`, equals to ``1``.

    .. c:enumerator:: CC_TE_CanCastle

        Rooks, Kings can castle. Persistent :term:`tag`, equals to ``2``.

    .. c:enumerator:: CC_TE_DelayedPromotion

        Pawn delayed promotion. Persistent :term:`tag`, equals to ``3``.

    .. c:enumerator:: CC_TE_EnPassant

        Pawn can be captured en passant. Semi-persistent, equals to ``4``.
        Gained in a move, used or lost in the very next one.

    .. c:enumerator:: CC_TE_PawnSacrifice

        Pawn was sacrificed. Non-persistent :term:`tag`, equals to ``5``.
        Gained in a move, used or lost in the very same move.

    .. c:enumerator:: CC_TE_MoveStarterFlag

        Flag to mark a piece which started a move, i.e. the one that cannot
        return to its starting position; equals to ``0x80``.

        This house-keeping flag is obtained after the first ply is finished,
        and follows the piece for the remainder of the cascade.

        This flag can be combined with any previous enumerator, e.g. a Serpent
        starting a move can also obtain Pawn-sacrifice tag, thus yielding
        ``0x85`` before continuing cascade with its new ply.

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

.. c:macro:: CC_MAX_LEN_LOSING_TAG

    Maximum length of a losing-tag symbol, equals to ``2``.

.. _lbl-libcc-cctags-losingtagfunctions:

Losing tag functions
--------------------

.. c:function:: char const * cc_losing_tag_symbol( CcLosingTagEnum lte )

    Function returns losing tag symbol as used in :term:`AN`, based on lost tag.

    :param lte: :c:enum:`CcLosingTagEnum` value.
    :returns: Valid pointer to zero-terminated string literal,
              do not try to :c:func:`free()` it.
              String can be empty, if tag cannot be lost.

.. c:function:: char const * cc_losing_tag_as_string( CcLosingTagEnum lte, bool capitalize, bool no_tag )

    Function returning descriptive string as used in user messages,
    based on lost tag.

    :param lte: :c:enum:`CcLosingTagEnum` value.
    :param capitalize: Flag, whether string should be capitalized.
    :param no_tag: Flag, whether should also describe no-tag value.
    :returns: Valid pointer to zero-terminated string literal,
              do not try to :c:func:`free()` it.
              String can be empty, if tag cannot be lost.

.. c:function:: CcLosingTagEnum cc_tag_to_losing( CcTagType te )

    Converts ordinary tag into lost tag.

    Ordinary tag values without equivalent losing tag value are converted
    into :c:enumerator:`CC_LTE_NoneLost` instead.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:enum:`CcLosingTagEnum` value.

.. c:function:: CcTagType cc_tag_from_losing( CcLosingTagEnum lte )

    Converts losing tag into ordinary tag.

    :param lte: :c:enum:`CcLosingTagEnum` value.
    :returns: :c:type:`CcTagType` value.

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
