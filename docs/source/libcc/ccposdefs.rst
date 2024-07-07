.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccposdefs:

Position defines
================

Documents ``cc_pos_defs.h`` and ``cc_pos_defs.c`` files, which contain various
piece steps definitions and functions.

.. _lbl-libcc-ccposdefs-piecestepsdata:

Piece steps data
----------------

.. c:macro:: CC_STEPS_LEN_INVALID_DATA_TERMINATED

    Value to ignore array size constraint on various functions,
    and use invalid step as a guard to indicate end of an array.

.. _lbl-libcc-ccposdefs-piecesteplengths:

Piece step lengths
------------------

    Length of an array is count of useful elements in it, without
    terminating data.

    This is similar how :c:`strlen()` does not count zero-terminating
    :c:`char` (:c:`'\0'`) in strings.

.. c:macro:: CC_STEPS_PAWN_LEN

    Equals to :c:`3`.

.. c:macro:: CC_STEPS_SIDEWAYS_PAWN_LEN

    Equals to :c:`5`.


.. c:macro:: CC_STEPS_KNIGHT_LEN

    Equals to :c:`8`.

.. c:macro:: CC_STEPS_BISHOP_LEN

    Equals to :c:`4`.

.. c:macro:: CC_STEPS_ROOK_LEN

    Equals to :c:`4`.

.. c:macro:: CC_STEPS_QUEEN_LEN

    Equals to :c:`8`.

.. c:macro:: CC_STEPS_KING_LEN

    Equals to :c:expr:`CC_STEPS_QUEEN_LEN`.


.. c:macro:: CC_STEPS_PEGASUS_LEN

    Equals to :c:expr:`CC_STEPS_KNIGHT_LEN`.

.. c:macro:: CC_STEPS_PYRAMID_LEN

    Equals to :c:expr:`CC_STEPS_ROOK_LEN`.

.. c:macro:: CC_STEPS_SHORT_UNICORN_LEN

    Equals to :c:expr:`CC_STEPS_KNIGHT_LEN`.

.. c:macro:: CC_STEPS_LONG_UNICORN_LEN

    Equals to :c:`16`.


.. c:macro:: CC_STEPS_STAR_LEN

    Equals to :c:expr:`CC_STEPS_QUEEN_LEN`.

.. c:macro:: CC_STEPS_SHORT_CENTAUR_LEN

    Equals to :c:expr:`CC_STEPS_SHORT_UNICORN_LEN`.

.. c:macro:: CC_STEPS_LONG_CENTAUR_LEN

    Equals to :c:expr:`CC_STEPS_LONG_UNICORN_LEN`.

.. c:macro:: CC_STEPS_SERPENT_LEN

    Equals to :c:`2`.

.. c:macro:: CC_STEPS_ALL_SERPENT_LEN

    Equals to :c:expr:`CC_STEPS_BISHOP_LEN`.


.. c:macro:: CC_STEPS_LIGHT_SHAMAN_LEN

    Equals to :c:expr:`CC_STEPS_KNIGHT_LEN + CC_STEPS_LONG_UNICORN_LEN`,
    i.e. count of steps + capture-steps, respectively.

.. c:macro:: CC_STEPS_DARK_SHAMAN_LEN

    Equals to :c:expr:`CC_STEPS_LONG_UNICORN_LEN + CC_STEPS_KNIGHT_LEN`,
    i.e. count of steps + capture-steps, respectively.


.. c:macro:: CC_STEPS_SCOUT_LEN

    Equals to :c:`5`.

.. c:macro:: CC_STEPS_GRENADIER_LEN

    Equals to :c:expr:`CC_STEPS_ROOK_LEN + CC_STEPS_BISHOP_LEN`,
    i.e. count of ordinary steps + capture-steps, respectively.


.. c:macro:: CC_STEPS_MIRACLE_STARCHILD_LEN

    Equals to :c:expr:`CC_STEPS_QUEEN_LEN`.

.. c:macro:: CC_STEPS_STARTING_MONOLITH_LEN

    Equals to :c:expr:`CC_STEPS_KNIGHT_LEN`.















.. _lbl-libcc-ccposdefs-sourcecodeheader:

Position defines source code header
-----------------------------------

Included source code file is ``cc_pos_defs.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_pos_defs.h
    :language: C
    :linenos:

.. _lbl-libcc-ccposdefs-sourcecodefile:

Position defines source code file
---------------------------------

Included source code file is ``cc_pos_defs.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_pos_defs.c
    :language: C
    :linenos:
