.. Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsestep:

Parse step
==========

Documents ``cc_parse_step.h`` and ``cc_parse_step.c`` files,
which contain various parse step definitions and functions.

.. _lbl-libcc-ccparsestep-functions:

Functions
---------

.. c:function:: bool cc_parse_steps( char const * steps_start_an, char const * steps_end_an, bool is_turn_light, cc_uint_t board_size, CcStep ** steps__o, CcParseMsg ** parse_msgs__iod )

    .. todo::

        DOCS

    ..  Function ...

    ..  :param move_an:
    ..  :param game:
    ..  :param move__o:
    ..  :param parse_msgs__iod:
    ..  :returns:

.. _lbl-libcc-ccparsestep-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_parse_step.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parse_step.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsestep-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_parse_step.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parse_step.c
    :language: C
    :linenos:
