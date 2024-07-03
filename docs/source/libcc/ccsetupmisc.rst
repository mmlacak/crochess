.. Copyright (c) 2023, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccsetupmisc:

Setup misc
==========

Documents ``cc_setup_misc.h`` and ``cc_setup_misc.c`` files, which contain
miscellaneous setup definitions and functions.

.. _lbl-libcc-ccsetupmisc-data:

Setup misc data
---------------

.. c:macro:: CC_KING_MIN_CASTLING_DISTANCE

    Minimum distance King can travel when castling, equals to :c:`2`.

    Does not depend on variant being played.

.. _lbl-libcc-ccsetupmisc-functions:

Setup misc functions
--------------------





.. _lbl-libcc-ccsetupmisc-sourcecodeheader:

Setup misc source code header
-----------------------------

Included source code file is ``cc_setup_misc.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_setup_misc.h
    :language: C
    :linenos:

.. _lbl-libcc-ccsetupmisc-sourcecodefile:

Setup misc source code file
---------------------------

Included source code file is ``cc_setup_misc.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_setup_misc.c
    :language: C
    :linenos:
