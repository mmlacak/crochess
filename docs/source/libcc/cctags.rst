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
    i.e. between :c:enumerator:`CC_TE_None` and :c:enumerator:`CC_TE_PawnSacrifice`
    values.

    Move-starter flag is not tested, its 1-bit storage exactly covers both
    states it can be in.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_TAG_IS_VALID(te)

    Macro to check if given :term:`tag` is enumerator, and not
    :c:enumerator:`CC_TE_None`.

    Move-starter flag is not tested, its 1-bit storage exactly covers both
    states it can be in.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

.. _lbl-libcc-cctags-values:

Values
------

.. c:macro:: CC_TAG_IS_PERSISTENT(te)

    Macro to check if given :term:`tag` is persistent,
    i.e. if it lasts until used or lost.

    :param te: :c:type:`CcTagType` value.
    :returns: :c:`bool` value.

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
