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

.. c:macro:: CC_TAG_EXISTS(te)

    Macro to check if given :term:`tag` is valid, i.e. it's not :c:expr:`CC_TE_None`.

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
