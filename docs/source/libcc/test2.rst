.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

Test libcc
==========

Not empty any more.

Enum surrogate
--------------

.. c:type:: spdylay_error

    Error codes used in the Spdylay library.

    .. c:macro:: SPDYLAY_ERR_INVALID_ARGUMENT

        (``-501``)
        Invalid argument passed.
    .. c:macro:: SPDYLAY_ERR_ZLIB

        (``-502``)
        Zlib error.

Syntax-hilighting
-----------------

.. c:var:: int a = 42

.. c:function:: int f(int i)

An expression: :c:expr:`a * f(a)` (or as text: :c:texpr:`a * f(a)`).

A type: :c:expr:`const Data*`
(or as text :c:texpr:`const Data*`).


Code as ultimate documentation
------------------------------

.. :doc:`cc_defines.h <../../../ws/libcrochess/inc/cc_defines.h>`

.. include:: ../../../ws/libcrochess/inc/cc_defines.h
    :code: C
