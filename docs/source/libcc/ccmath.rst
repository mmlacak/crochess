.. Copyright (c) 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccmath:

Math
====

Documents ``cc_math.h`` and ``cc_math.c`` files, which contain
various math functions.

.. _lbl-libcc-ccmath-functions:

Math functions
--------------

.. c:function:: int cc_gcd( int x, int y )

    Function returns greatest common divisor between two given integers.

    :param x: An integer.
    :param y: Another integer.
    :returns: Greatest common divisor.
    :seealso: https://en.wikipedia.org/wiki/Euclidean_algorithm

.. c:function:: size_t cc_diagonal( size_t size )

    Function returns length of a chessboard diagonal.

    :param size: An integer.
    :returns: Diagonal of a chessboard, rounded up.

.. c:function:: int cc_count_of_digits( size_t n )

    Function returns number of digits for a given number, in base-10
    representation.

    :param n: A number.
    :returns: Number of digits.

.. _lbl-libcc-ccmath-sourcecodeheader:

Math source code header
-----------------------

Included source code file is ``cc_math.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_math.h
    :language: C
    :linenos:

.. _lbl-libcc-ccmath-sourcecodefile:

Math source code file
---------------------

Included source code file is ``cc_math.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_math.c
    :language: C
    :linenos:
