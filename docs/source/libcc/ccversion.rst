.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccversion:

Version
=======

Documents ``cc_version.h`` and ``cc_version.c`` files, which contain
library version constant.

.. c:type:: char const CC_LIB_VERSION[]

    Library ``libcrochess`` version constant.

    This is the reference point for all the other code versions in the ``crochess`` project.

    Versioning scheme used is
    `Natural Versioning 1.2 <https://croatian-chess.blogspot.com/p/natver.html>`_.

.. _lbl-libcc-ccversion-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_version.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_version.h
    :language: C
    :linenos:

.. _lbl-libcc-ccversion-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_version.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_version.c
    :language: C
    :linenos:
