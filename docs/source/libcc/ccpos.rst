.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccpos:

Position
========

Documents ``cc_pos.h`` and ``cc_pos.c`` files, which contain various
position definitions, linked lists and functions.

.. _lbl-libcc-ccpos-data:

Position data
-------------

.. c:macro:: CC_POS_INVALID

    Invalid position value, both coordinates are equal to :c:expr:`CC_INVALID_COORD`.

.. c:macro:: CC_POS_STATIC_STEP

    Static position value, i.e. no-movement step; both coordinates are equal to :c:`0`.

.. c:macro:: CC_POS_ORIGIN_FIELD

    Origin field, i.e. coordinate system start; both coordinates are equal to :c:`0`.

.. c:struct:: CcPos

    Position :c:`struct`, either absolute or relative,
    i.e. either a location or a step.

    .. c:member:: int i

        File, horizontal coordinate.

    .. c:member:: int j

        Rank, vertical coordinate.

    :c:`CcPos` is tagged with the same :c:expr:`CcPos` name.

.. c:macro:: CC_POS_CAST_INVALID

    Casted invalid position value, i.e. :c:expr:`CC_POS_INVALID`.

.. c:macro:: CC_POS_CAST_STATIC_STEP

    Casted static position value, i.e. :c:expr:`CC_POS_STATIC_STEP`.

.. c:macro:: CC_POS_CAST_ORIGIN_FIELD

    Casted origin field, i.e. :c:expr:`CC_POS_ORIGIN_FIELD`.

.. c:macro:: CC_POS(int_i,int_j)

    Macro definition for instantiating position, i.e. :c:expr:`CcPos` :c:`struct`\ure.

    :param int_i: File, horizontal coordinate.
    :param int_j: Rank, vertical coordinate.
    :returns: Position with given coordinates.

.. c:macro:: CC_POS_CAST(int_i,int_j)

    Macro definition for a casted position.

    :param int_i: File, horizontal coordinate.
    :param int_j: Rank, vertical coordinate.
    :returns: Casted position with given coordinates.

.. c:macro:: CC_POS_IS_VALID(pos)

    Macro to check if given position is valid, i.e. if both
    coordinates are valid.

    Coordinate is valid if it's not :c:expr:`CC_INVALID_COORD`.

    :param pos: A position, i.e. :c:expr:`CcPos` value.
    :returns: :c:`true` if valid position, :c:`false` otherwise.







.. _lbl-libcc-ccpos-sourcecodeheader:

Position source code header
---------------------------

Included source code file is ``cc_pos.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_pos.h
    :language: C
    :linenos:

.. _lbl-libcc-ccpos-sourcecodefile:

Position source code file
-------------------------

Included source code file is ``cc_pos.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_pos.c
    :language: C
    :linenos:
