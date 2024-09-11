.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccposdefs:

Position defines
================

Documents ``cc_pos_defs.h`` and ``cc_pos_defs.c`` files, which contain various
piece steps definitions and functions.

.. _lbl-libcc-ccposdefs-data:

Data
----

.. c:macro:: CC_STEPS_LEN_INVALID_DATA_TERMINATED

    Value to ignore array size constraint on various functions,
    and use invalid step as a guard to indicate end of an array.

.. _lbl-libcc-ccposdefs-lengths:

Lengths
-------

    Length of an array is count of useful elements in it, without
    terminating data.

    This is similar how :c:func:`strlen()` does not count zero-terminating
    :c:`char` (``'\0'``) in strings.

.. c:macro:: CC_STEPS_PAWN_LEN

    Equals to ``3``.

.. c:macro:: CC_STEPS_SIDEWAYS_PAWN_LEN

    Equals to ``5``.


.. c:macro:: CC_STEPS_KNIGHT_LEN

    Equals to ``8``.

.. c:macro:: CC_STEPS_BISHOP_LEN

    Equals to ``4``.

.. c:macro:: CC_STEPS_ROOK_LEN

    Equals to ``4``.

.. c:macro:: CC_STEPS_QUEEN_LEN

    Equals to ``8``.

.. c:macro:: CC_STEPS_KING_LEN

    Equals to :c:macro:`CC_STEPS_QUEEN_LEN`.


.. c:macro:: CC_STEPS_PEGASUS_LEN

    Equals to :c:macro:`CC_STEPS_KNIGHT_LEN`.

.. c:macro:: CC_STEPS_PYRAMID_LEN

    Equals to :c:macro:`CC_STEPS_ROOK_LEN`.

.. c:macro:: CC_STEPS_SHORT_UNICORN_LEN

    Equals to :c:macro:`CC_STEPS_KNIGHT_LEN`.

.. c:macro:: CC_STEPS_LONG_UNICORN_LEN

    Equals to ``16``.


.. c:macro:: CC_STEPS_STAR_LEN

    Equals to :c:macro:`CC_STEPS_QUEEN_LEN`.

.. c:macro:: CC_STEPS_SHORT_CENTAUR_LEN

    Equals to :c:macro:`CC_STEPS_SHORT_UNICORN_LEN`.

.. c:macro:: CC_STEPS_LONG_CENTAUR_LEN

    Equals to :c:macro:`CC_STEPS_LONG_UNICORN_LEN`.


.. c:macro:: CC_STEPS_SERPENT_LEN

    Equals to ``2``.

.. c:macro:: CC_STEPS_ALL_SERPENT_LEN

    Equals to :c:macro:`CC_STEPS_BISHOP_LEN`.

.. c:macro:: CC_STEPS_COLOR_CHANGE_SERPENT_LEN

    Equals to :c:macro:`CC_STEPS_ROOK_LEN`.

.. c:macro:: CC_STEPS_DISPLACEMENT_SERPENT_LEN

    Equals to :c:macro:`CC_STEPS_ROOK_LEN`.


.. c:macro:: CC_STEPS_LIGHT_SHAMAN_LEN

    Equals to :c:macro:`CC_STEPS_KNIGHT_LEN` + :c:macro:`CC_STEPS_LONG_UNICORN_LEN`,
    i.e. count of steps + capture-steps, respectively.

.. c:macro:: CC_STEPS_DARK_SHAMAN_LEN

    Equals to :c:macro:`CC_STEPS_LONG_UNICORN_LEN` + :c:macro:`CC_STEPS_KNIGHT_LEN`,
    i.e. count of steps + capture-steps, respectively.


.. c:macro:: CC_STEPS_SCOUT_LEN

    Equals to ``5``.

.. c:macro:: CC_STEPS_GRENADIER_LEN

    Equals to :c:macro:`CC_STEPS_ROOK_LEN` + :c:macro:`CC_STEPS_BISHOP_LEN`,
    i.e. count of ordinary steps + capture-steps, respectively.


.. c:macro:: CC_STEPS_MIRACLE_STARCHILD_LEN

    Equals to :c:macro:`CC_STEPS_QUEEN_LEN`.

.. c:macro:: CC_STEPS_STARTING_MONOLITH_LEN

    Equals to :c:macro:`CC_STEPS_KNIGHT_LEN`.


.. c:macro:: CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_LEN

    Equals to ``44``.


.. _lbl-libcc-ccposdefs-sizes:

Piece step sizes
----------------

    Size of an array is count of all items in an it, including guard data,
    i.e. terminating, invalid step.

.. c:macro:: CC_STEPS_PAWN_SIZE

    Equals to :c:macro:`CC_STEPS_PAWN_LEN` + 1.

.. c:macro:: CC_STEPS_SIDEWAYS_PAWN_SIZE

    Equals to :c:macro:`CC_STEPS_SIDEWAYS_PAWN_LEN` + 1.


.. c:macro:: CC_STEPS_KNIGHT_SIZE

    Equals to :c:macro:`CC_STEPS_KNIGHT_LEN` + 1.

.. c:macro:: CC_STEPS_BISHOP_SIZE

    Equals to :c:macro:`CC_STEPS_BISHOP_LEN` + 1.

.. c:macro:: CC_STEPS_ROOK_SIZE

    Equals to :c:macro:`CC_STEPS_ROOK_LEN` + 1.

.. c:macro:: CC_STEPS_QUEEN_SIZE

    Equals to :c:macro:`CC_STEPS_QUEEN_LEN` + 1.

.. c:macro:: CC_STEPS_KING_SIZE

    Equals to :c:macro:`CC_STEPS_QUEEN_SIZE`.


.. c:macro:: CC_STEPS_PEGASUS_SIZE

    Equals to :c:macro:`CC_STEPS_KNIGHT_SIZE`.

.. c:macro:: CC_STEPS_PYRAMID_SIZE

    Equals to :c:macro:`CC_STEPS_ROOK_SIZE`.

.. c:macro:: CC_STEPS_SHORT_UNICORN_SIZE

    Equals to :c:macro:`CC_STEPS_KNIGHT_SIZE`.

.. c:macro:: CC_STEPS_LONG_UNICORN_SIZE

    Equals to :c:macro:`CC_STEPS_LONG_UNICORN_LEN` + 1.


.. c:macro:: CC_STEPS_STAR_SIZE

    Equals to :c:macro:`CC_STEPS_QUEEN_SIZE`.

.. c:macro:: CC_STEPS_SHORT_CENTAUR_SIZE

    Equals to :c:macro:`CC_STEPS_SHORT_UNICORN_SIZE`.

.. c:macro:: CC_STEPS_LONG_CENTAUR_SIZE

    Equals to :c:macro:`CC_STEPS_LONG_UNICORN_SIZE`.


.. c:macro:: CC_STEPS_SERPENT_SIZE

    Equals to :c:macro:`CC_STEPS_SERPENT_LEN` + 1.

.. c:macro:: CC_STEPS_ALL_SERPENT_SIZE

    Equals to :c:macro:`CC_STEPS_ALL_SERPENT_LEN` + 1.

.. c:macro:: CC_STEPS_COLOR_CHANGE_SERPENT_SIZE

    Equals to :c:macro:`CC_STEPS_COLOR_CHANGE_SERPENT_LEN` + 1.

.. c:macro:: CC_STEPS_DISPLACEMENT_SERPENT_SIZE

    Equals to :c:macro:`CC_STEPS_DISPLACEMENT_SERPENT_LEN` + 1.


.. c:macro:: CC_STEPS_LIGHT_SHAMAN_SIZE

    Equals to :c:macro:`CC_STEPS_LIGHT_SHAMAN_LEN` + 1.

.. c:macro:: CC_STEPS_DARK_SHAMAN_SIZE

    Equals to :c:macro:`CC_STEPS_DARK_SHAMAN_LEN` + 1.


.. c:macro:: CC_STEPS_SCOUT_SIZE

    Equals to :c:macro:`CC_STEPS_SCOUT_LEN` + 1.

.. c:macro:: CC_STEPS_GRENADIER_SIZE

    Equals to :c:macro:`CC_STEPS_GRENADIER_LEN` + 1.


.. c:macro:: CC_STEPS_MIRACLE_STARCHILD_SIZE

    Equals to :c:macro:`CC_STEPS_QUEEN_SIZE`.

.. c:macro:: CC_STEPS_STARTING_MONOLITH_SIZE

    Equals to :c:macro:`CC_STEPS_STARTING_MONOLITH_LEN` + 1.


.. c:macro:: CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_SIZE

    Equals to :c:macro:`CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_LEN` + 1.


.. _lbl-libcc-ccposdefs-arrays:

Arrays
------

All arrays, beside their length and size, have a terminating position,
similar to zero-terminating strings; terminating position is
:c:data:`CC_TYPED_STEP_INVALID`.

.. c:type:: CcTypedStep const CC_STEPS_LIGHT_PAWN[ CC_STEPS_PAWN_SIZE ]

    Steps array for light Pawn, from Classical Chess up to, and including,
    Miranda's Veil variant.

.. c:type:: CcTypedStep const CC_STEPS_DARK_PAWN[ CC_STEPS_PAWN_SIZE ]

    Steps array for dark Pawn, from Classical Chess up to, and including,
    Miranda's Veil variant.

.. c:type:: CcTypedStep const CC_STEPS_LIGHT_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ]

    Steps array for light Pawn, from Nineteen variant onwards.

.. c:type:: CcTypedStep const CC_STEPS_DARK_SIDEWAYS_PAWN[ CC_STEPS_SIDEWAYS_PAWN_SIZE ]

    Steps array for dark Pawn, from Nineteen variant onwards.


.. c:type:: CcTypedStep const CC_STEPS_KNIGHT[ CC_STEPS_KNIGHT_SIZE ]

    Steps array for Knights, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_BISHOP[ CC_STEPS_BISHOP_SIZE ]

    Steps array for Bishops, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_ROOK[ CC_STEPS_ROOK_SIZE ]

    Steps array for Rooks, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_QUEEN[ CC_STEPS_QUEEN_SIZE ]

    Steps array for Queens, for all variants.

.. c:macro:: CC_STEPS_KING

    Steps array for Kings, for all variants; equals to
    :c:macro:`CC_STEPS_QUEEN`.


.. c:macro:: CC_STEPS_PEGASUS

    Steps array for light and dark Pegasus, for all variants;
    equals to :c:macro:`CC_STEPS_KNIGHT`.

.. c:macro:: CC_STEPS_PYRAMID

    Steps array for Pyramids, for all variants; equals to
    :c:macro:`CC_STEPS_ROOK`.

.. c:macro:: CC_STEPS_SHORT_UNICORN

    Steps array for Unicorns featuring short step, for all variants;
    equals to :c:macro:`CC_STEPS_KNIGHT`.

.. c:type:: CcTypedStep const CC_STEPS_LONG_UNICORN[ CC_STEPS_LONG_UNICORN_SIZE ]

    Steps array for Unicorns featuring long step, for all variants.


.. c:macro:: CC_STEPS_STAR

    Steps array for Stars, for all variants; equals to
    :c:macro:`CC_STEPS_QUEEN`.

.. c:macro:: CC_STEPS_SHORT_CENTAUR

    Steps array for Centaurs featuring short step, for all variants;
    equals to :c:macro:`CC_STEPS_SHORT_UNICORN`.

.. c:macro:: CC_STEPS_LONG_CENTAUR

    Steps array for Centaurs featuring long step, for all variants;
    equals to :c:macro:`CC_STEPS_LONG_UNICORN`.


.. c:type:: CcTypedStep const CC_STEPS_SERPENT_LEFT[ CC_STEPS_SERPENT_SIZE ]

    Steps array for Serpents featuring left diagonal, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_SERPENT_SIZE ]

    Steps array for Serpents featuring right diagonal, for all variants.

.. c:macro:: CC_STEPS_ALL_SERPENT

    Steps array for Serpents, for all variants; equals to
    :c:macro:`CC_STEPS_BISHOP`.

.. c:macro:: CC_STEPS_COLOR_CHANGE_SERPENT

    Color-changing steps array for Serpents, for all variants; equals to
    :c:macro:`CC_STEPS_ROOK`.

    .. note::

        These color-changing steps are *not* included in all-steps array,
        i.e. they are *not* in :c:macro:`CC_STEPS_ALL_SERPENT`.

.. c:macro:: CC_STEPS_DISPLACEMENT_SERPENT

    Pawn-displacement steps array for Serpents, for all variants; equals to
    :c:macro:`CC_STEPS_ROOK`.

    .. note::

        These Pawn-displacement steps are *not* included in all-steps array,
        i.e. they are *not* in :c:macro:`CC_STEPS_ALL_SERPENT`.


.. c:type:: CcTypedStep const CC_STEPS_LIGHT_SHAMAN[ CC_STEPS_LIGHT_SHAMAN_SIZE ]

    Steps array for light Shaman, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_DARK_SHAMAN[ CC_STEPS_DARK_SHAMAN_SIZE ]

    Steps array for dark Shaman, for all variants.


.. c:type:: CcTypedStep const CC_STEPS_LIGHT_SCOUT[ CC_STEPS_SCOUT_SIZE ]

    Steps array for light Scout, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_DARK_SCOUT[ CC_STEPS_SCOUT_SIZE ]

    Steps array for dark Scout, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_GRENADIER[ CC_STEPS_GRENADIER_SIZE ]

    Steps array for Grenadiers, for all variants.


.. c:type:: CcTypedStep const CC_STEPS_MIRACLE_STARCHILD[ CC_STEPS_MIRACLE_STARCHILD_SIZE ]

    Miracle-steps array for Starchilds, for all variants.

.. c:macro:: CC_STEPS_STARTING_MONOLITH

    Steps array for Monolith, for all variants; equals to
    :c:macro:`CC_STEPS_KNIGHT`.


.. c:type:: CcTypedStep const CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY[ CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_SIZE ]

    Displacement steps array for trance-journey initiated by light Shaman,
    for all variants.


.. _lbl-libcc-ccposdefs-validity:

Validity
--------

.. c:function:: bool cc_is_step_valid( CcTypedStep step, CcTypedStep const steps[], size_t steps_len__d )

    Function checking if step is valid, by searching a given array
    holding all valid steps for a piece.

    If :c:`steps_len__d` is not used (i.e. it's :c:macro:`CC_STEPS_LEN_INVALID_DATA_TERMINATED`),
    :c:`steps` array **must** be terminated with invalid step (i.e. :c:macro:`CC_POS_INVALID`)
    as a guard.

    :param step: A step to check.
    :param steps: An array of all valid steps.
    :param steps_len__d: *Optional* parameter, array length.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. _lbl-libcc-ccposdefs-validitymacros:

Validity macros
^^^^^^^^^^^^^^^

    The step validity macro conveniences are meant to be used instead
    of :c:func:`cc_is_step_valid()`.

.. c:macro:: CC_LIGHT_PAWN_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by light Pawn, from
    Classical Chess up to, and including, Miranda's Veil variant.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_DARK_PAWN_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by dark Pawn, from
    Classical Chess up to, and including, Miranda's Veil variant.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_LIGHT_SIDEWAYS_PAWN_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by light Pawn,
    from Nineteen variant onwards.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_DARK_SIDEWAYS_PAWN_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by dark Pawn,
    from Nineteen variant onwards.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.


.. c:macro:: CC_KNIGHT_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Knight.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_BISHOP_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Bishop.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_ROOK_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Rook.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_QUEEN_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Queen.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_KING_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by King.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.


.. c:macro:: CC_PEGASUS_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Pegasus.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_PYRAMID_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Pyramid.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_UNICORN_SHORT_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Unicorn featuring
    short step.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_UNICORN_LONG_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Unicorn featuring
    long step.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.


.. c:macro:: CC_STAR_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Star.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_CENTAUR_SHORT_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Centaur featuring
    short step.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_CENTAUR_LONG_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Centaur featuring
    long step.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_SERPENT_LEFT_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Serpent featuring
    left diagonal step.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_SERPENT_RIGHT_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Serpent featuring
    right diagonal step.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_SERPENT_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Serpent featuring
    initial step.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_SERPENT_COLOR_CHANGE_STEP_IS_VALID(step)

    Macro to check if color-changing :c:`step` can be made by Serpent.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.


.. c:macro:: CC_LIGHT_SCOUT_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by light Scout.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_DARK_SCOUT_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by dark Scout.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_GRENADIER_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by Grenadier.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.


.. c:macro:: CC_LIGHT_SHAMAN_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by light Shaman.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_DARK_SHAMAN_STEP_IS_VALID(step)

    Macro to check if :c:`step` can be made by dark Shaman.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. c:macro:: CC_STARCHILD_MIRACLE_STEP_IS_VALID(step)

    Macro to check if miracle-:c:`step` can be made by Starchild.

    :param step: A step to check.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.

.. _lbl-libcc-ccposdefs-functions:

Functions
---------

.. c:function:: bool cc_is_same_color( CcPieceType piece, CcPos pos )

    Function checks if piece and a field are in the same color.

    :param piece: A piece.
    :param pos: A position.
    :returns: :c:data:`true` if in the same color, :c:data:`false` otherwise.

.. c:function:: bool cc_convert_steps_to_pos_link( CcTypedStep const steps[], size_t steps_len__d, CcTypedStepLink ** steps__o )

    Function converts step array into a newly allocated linked list.

    *Output* argument :c:`*steps__o` has to be :c:data:`NULL`; appending
    steps to the same linked list is not supported.

    If :c:`steps_len__d` is :c:macro:`CC_STEPS_LEN_INVALID_DATA_TERMINATED`,
    :c:`steps` array **must** be terminated with invalid step
    (i.e. :c:macro:`CC_POS_INVALID`) as a guard.

    :param steps: A piece step array.
    :param steps_len__d: *Optional*; size of :c:`steps` array.
    :param steps__o: *Output*; newly allocated linked list.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-ccposdefs-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_pos_defs.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_pos_defs.h
    :language: C
    :linenos:

.. _lbl-libcc-ccposdefs-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_pos_defs.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_pos_defs.c
    :language: C
    :linenos:
