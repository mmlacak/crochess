.. Copyright (c) 2025 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsesideeffect:

Parse side effect
=================

Documents ``cc_parse_side_effect.h`` and ``cc_parse_side_effect.c`` files,
which contain various parse side-effect definitions and functions.

.. _lbl-libcc-ccparsesideeffect-functions:

Functions
---------

.. c:function:: bool cc_parse_side_effect( char const * side_effect_an, char const * step_start_an, char const * step_end_an, bool is_turn_light, cc_uint_t board_size, CcSideEffect * side_effect__o, char const ** side_effect_end_an__o, CcParseMsg ** parse_msgs__iod )

    .. todo::

        DOCS

    ..  Function ...

    ..  :param move_an:
    ..  :param game:
    ..  :param move__o:
    ..  :param parse_msgs__iod:
    ..  :returns:

.. _lbl-libcc-ccparsesideeffect-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_parse_side_effect.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parse_side_effect.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsesideeffect-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_parse_side_effect.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parse_side_effect.c
    :language: C
    :linenos:
