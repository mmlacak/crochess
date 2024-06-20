.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-cctags:

Tags
====

Documents ``cc_tags.h`` and ``cc_tags.c`` files, which contain :term:`tag` enumeration, and related functions.

.. c:macro:: CC_TAG_IS_VALID(te)

    Macro to check if given :term:`tag` is a valid,
    i.e. between :c:expr:`CC_TE_None` and :c:expr:`CC_TE_PawnSacrifice` values.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_EQUAL(te1,te2)

    Macro to check if given :term:`tag`\s are the same.

    :param te1: :c:expr:`CcTagEnum` value.
    :param te2: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_NONE(te)

    Macro to check if given :term:`tag` is :c:expr:`CC_TE_None`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_EXISTS(te)

    Macro to check if given :term:`tag` is valid, and not :c:expr:`CC_TE_None`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_RUSH(te)

    Macro to check if given :term:`tag` is :c:expr:`CC_TE_CanRush`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_CASTLE(te)

    Macro to check if given :term:`tag` is :c:expr:`CC_TE_CanCastle`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_PROMOTE(te)

    Macro to check if given :term:`tag` is :c:expr:`CC_TE_DelayedPromotion`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_EN_PASSANT(te)

    Macro to check if given :term:`tag` is :c:expr:`CC_TE_EnPassant`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_PAWN_SACRIFICE(te)

    Macro to check if given :term:`tag` is :c:expr:`CC_TE_PawnSacrifice`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_PERSISTENT(te)

    Macro to check if given :term:`tag` is persistent,
    i.e. if it lasts until used or lost.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_TEMPORARY(te)

    Macro to check if given :term:`tag` is temporary,
    i.e. lasts at most a single move.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. topic:: Tag character macro constants

    All :c:`CC_TAG_CHAR_*` macro constants are used to render tag board
    on a console.

    .. c:macro:: CC_TAG_CHAR_NONE

        Equals to :c:`' '`.

    .. c:macro:: CC_TAG_CHAR_INVALID

        Equals to :c:`'?'`.

    .. c:macro:: CC_TAG_CHAR_CAN_RUSH

        Equals to :c:`'R'`.

    .. c:macro:: CC_TAG_CHAR_CAN_CASTLE

        Equals to :c:`'C'`.

    .. c:macro:: CC_TAG_CHAR_DELAYED_PROMOTION

        Equals to :c:`'P'`.

    .. c:macro:: CC_TAG_CHAR_EN_PASSANT

        Equals to :c:`'E'`.

    .. c:macro:: CC_TAG_CHAR_PAWN_SACRIFICE

        Equals to :c:`'S'`.

.. c:enum:: CcTagEnum

    Enumerates all :term:`tag`\s, used in all variants.

    .. c:enumerator:: CC_TE_None

        No :term:`tag` applies, equals to :c:`0`.
        Used for e.g. empty on-board fields, any off-board field.

    .. c:enumerator:: CC_TE_CanRush

        Pawn can rush. Persistent :term:`tag`, equals to :c:`1`.

    .. c:enumerator:: CC_TE_CanCastle

        Rooks, Kings can castle. Persistent :term:`tag`, equals to :c:`2`.

    .. c:enumerator:: CC_TE_DelayedPromotion

        Pawn delayed promotion. Persistent :term:`tag`, equals to :c:`3`.

    .. c:enumerator:: CC_TE_EnPassant

        Pawn can capture en passant. Semi-persistent, equals to :c:`4`.
        Gained in a move, used or lost in the very next one.

    .. c:enumerator:: CC_TE_PawnSacrifice

        Pawn was sacrificed. Non-persistent :term:`tag`, equals to :c:`5`.
        Gained in a move, used or lost in the very same move.

.. c:function:: char cc_tag_as_char( CcTagEnum ct )

    Function returning :term:`tag` char, based on tag enum.

    :param ct: :c:expr:`CcTagEnum` value.
    :returns: Tag char, one of :c:`CC_TAG_CHAR_*` constants.

.. c:function:: CcTagEnum cc_tag_from_char( char c )

    Function returning :term:`tag` enum, based on :term:`tag` char.

    :param c: A char, expected to be one of :c:`CC_TAG_CHAR_*` constants.
    :returns: :c:expr:`CcTagEnum` value if valid :term:`tag` character was given,
              :c:`CC_TE_None` otherwise.

.. c:enum:: CcLosingTagEnum

    Enumerates only :term:`tag`\s that can be lost, used in all variants.

    Values enumerated in losing tag are the same as in ordinary tag.
    So, conversion between tags changes just type, not value.

    When converting from ordinary tag enum, `CC_LTE_None` is used for
    all values not enumerated here.

    .. c:enumerator:: CC_LTE_None

        No :term:`tag` was lost, equals to :c:`0`.

    .. c:enumerator:: CC_LTE_CanRush

        Pawn lost ability to rush, equals to :c:`1`.

    .. c:enumerator:: CC_LTE_CanCastle

        Rook or King lost ability to castle, equals to :c:`2`.

    .. c:enumerator:: CC_LTE_DelayedPromotion

        Pawn lost delayed promotion :term:`tag`, equals to :c:`3`.
