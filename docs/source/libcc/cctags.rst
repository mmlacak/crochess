.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-cctags:

Tags
====

Documents ``cc_tag.h`` and ``cc_tag.c`` files, which contain :term:`tag` enumeration, and related functions.

.. _lbl-libcc-cctags-validity:

Tag validity
------------

.. c:macro:: CC_TAG_IS_VALID(te)

    Macro to check if given :term:`tag` is a valid,
    i.e. between :c:`CC_TE_None` and :c:`CC_TE_PawnSacrifice` values.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_EQUAL(te1,te2)

    Macro to check if given :term:`tag`\s are the same.

    :param te1: :c:expr:`CcTagEnum` value.
    :param te2: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_NONE(te)

    Macro to check if given :term:`tag` is :c:`CC_TE_None`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_EXISTS(te)

    Macro to check if given :term:`tag` is valid, and not :c:`CC_TE_None`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. _lbl-libcc-cctags-values:

Tag values
----------

.. c:macro:: CC_TAG_CAN_RUSH(te)

    Macro to check if given :term:`tag` is :c:`CC_TE_CanRush`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_CASTLE(te)

    Macro to check if given :term:`tag` is :c:`CC_TE_CanCastle`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_PROMOTE(te)

    Macro to check if given :term:`tag` is :c:`CC_TE_DelayedPromotion`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_EN_PASSANT(te)

    Macro to check if given :term:`tag` is :c:`CC_TE_EnPassant`.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_CAN_PAWN_SACRIFICE(te)

    Macro to check if given :term:`tag` is :c:`CC_TE_PawnSacrifice`.

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

.. _lbl-libcc-cctags-characters:

Tag characters
--------------

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

.. _lbl-libcc-cctags-types:

Tag types
---------

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

.. _lbl-libcc-cctags-functions:

Tag functions
-------------

.. c:function:: char cc_tag_as_char( CcTagEnum ct )

    Function returning :term:`tag` char, based on tag enum.

    :param ct: :c:expr:`CcTagEnum` value.
    :returns: Tag char, one of :c:`CC_TAG_CHAR_*` constants.

.. c:function:: CcTagEnum cc_tag_from_char( char c )

    Function returning :term:`tag` enum, based on :term:`tag` char.

    :param c: A char, expected to be one of :c:`CC_TAG_CHAR_*` constants.
    :returns: :c:expr:`CcTagEnum` value if valid :term:`tag` character was given,
              :c:`CC_TE_None` otherwise.

.. _lbl-libcc-cctags-losing-tag-types:

Losing tag types
----------------

.. c:enum:: CcLosingTagEnum

    Enumerates only :term:`tag`\s that can be lost, used in all variants.

    Values enumerated in losing tag are the same as in ordinary tag.
    So, conversion between tags changes just type, not value.

    When converting from ordinary tag enum, :c:`CC_LTE_None` is used for
    all values not enumerated here.

    .. c:enumerator:: CC_LTE_None

        No :term:`tag` was lost, equals to :c:`CC_TE_None`.

    .. c:enumerator:: CC_LTE_CanRush

        Pawn lost ability to rush, equals to :c:`CC_TE_CanRush`.

    .. c:enumerator:: CC_LTE_CanCastle

        Rook or King lost ability to castle, equals to :c:`CC_TE_CanCastle`.

    .. c:enumerator:: CC_LTE_DelayedPromotion

        Pawn lost delayed promotion :term:`tag`, equals to :c:`CC_TE_DelayedPromotion`.

.. c:macro:: CC_MAX_LEN_LOSING_TAG

    Maximum length of a losing-tag symbol, equals to :c:`2`.

.. _lbl-libcc-cctags-losing-tag-functions:

Losing tag functions
--------------------

.. c:function:: char const * cc_losing_tag_as_string( CcLosingTagEnum lte )

    Function returning string, based on lost tag.

    :param lte: :c:expr:`CcLosingTagEnum` value.
    :returns: Valid pointer to zero-terminated string literal,
              do not try to :c:`free()` it.
              String can be empty, if tag cannot be lost.

.. c:function:: CcLosingTagEnum cc_tag_to_losing( CcTagEnum te )

    Converts ordinary tag into lost tag.

    Ordinary tag values without equivalent losing tag value are converted into :c:`CC_LTE_None` instead.

    :param te: :c:expr:`CcTagEnum` value.
    :returns: :c:expr:`CcLosingTagEnum` value.

.. c:function:: CcTagEnum cc_tag_from_losing( CcLosingTagEnum lte )

    Converts losing tag into ordinary tag.

    :param lte: :c:expr:`CcLosingTagEnum` value.
    :returns: :c:expr:`CcTagEnum` value.

.. _lbl-libcc-cctags-sourcecodeheader:

Tags source code header
-----------------------

Included source code file is ``cc_tag.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_tag.h
    :language: C
    :linenos:

.. _lbl-libcc-cctags-sourcecodefile:

Tags source code file
---------------------

Included source code file is ``cc_tag.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_tag.c
    :language: C
    :linenos:
