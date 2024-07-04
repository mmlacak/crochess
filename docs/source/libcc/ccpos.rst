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
