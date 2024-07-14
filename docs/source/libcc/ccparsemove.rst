.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccparsemove:

Parse move
==========

Documents ``cc_parse_move.h`` and ``cc_parse_move.c`` files,
which contain various parse move definitions and functions.

.. _lbl-libcc-ccparsemove-functions:

Parse move functions
--------------------

.. c:function:: bool cc_parse_move( char const * move_an, CcGame * game, CcParsedMove ** move__o, CcParseMsg ** parse_msgs__iod )

    .. todo::

        DOCS

    ..  Function ...

    ..  :param move_an:
    ..  :param game:
    ..  :param move__o:
    ..  :param parse_msgs__iod:
    ..  :returns:

.. _lbl-libcc-ccparsemove-sourcecodeheader:

Parse move source code header
-----------------------------

Included source code file is ``cc_parse_move.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_parse_move.h
    :language: C
    :linenos:

.. _lbl-libcc-ccparsemove-sourcecodefile:

Parse move source code file
---------------------------

Included source code file is ``cc_parse_move.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_parse_move.c
    :language: C
    :linenos:
