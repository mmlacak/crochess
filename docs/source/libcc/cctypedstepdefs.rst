.. Copyright (c) 2021, 2022, 2024, 2025 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-cctypedstepdefs:

Typed step defines
==================

Documents ``cc_typed_step_defs.h`` and ``cc_typed_step_defs.c`` files, which contain various
piece steps definitions and functions.

.. _lbl-libcc-cctypedstepdefs-data:

Data
----

.. c:macro:: CC_STEPS_LEN_GUARD_DATA_TERMINATED

    Value to ignore array size constraint on various functions,
    and use invalid step as a guard to indicate end of an array.

.. _lbl-libcc-cctypedstepdefs-lengths:

Lengths
-------

    Length of an array is count of useful elements in it, without
    terminating data.

    This is similar how :c:func:`strlen()` does not count null-terminating
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


.. c:macro:: CC_STEPS_DIAGONAL_SERPENT_LEN

    Equals to ``2``.

.. c:macro:: CC_STEPS_COLOR_CHANGE_SERPENT_LEN

    Equals to ``4``.

.. c:macro:: CC_STEPS_DISPLACEMENT_SERPENT_LEN

    Equals to ``4``.

.. c:macro:: CC_STEPS_ALL_SERPENT_LEN

    All Serpent steps are comprised from left diagonal (represented by the first
    :c:macro:`CC_STEPS_DIAGONAL_SERPENT_LEN`), right diagonal (the second
    :c:macro:`CC_STEPS_DIAGONAL_SERPENT_LEN`), color-change steps, and displacement
    steps.

    Equals to :c:macro:`CC_STEPS_DIAGONAL_SERPENT_LEN` + :c:macro:`CC_STEPS_DIAGONAL_SERPENT_LEN`
    + :c:macro:`CC_STEPS_COLOR_CHANGE_SERPENT_LEN` + :c:macro:`CC_STEPS_DISPLACEMENT_SERPENT_LEN`.


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


.. c:macro:: CC_STEPS_PAWN_DISPLACEMENT_LEN

    Equals to ``4``.

.. c:macro:: CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_LEN

    Equals to ``44``.


.. _lbl-libcc-cctypedstepdefs-sizes:

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


.. c:macro:: CC_STEPS_DIAGONAL_SERPENT_SIZE

    Equals to :c:macro:`CC_STEPS_DIAGONAL_SERPENT_LEN` + 1.

.. c:macro:: CC_STEPS_ALL_SERPENT_SIZE

    Equals to :c:macro:`CC_STEPS_ALL_SERPENT_LEN` + 1.


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


.. c:macro:: CC_STEPS_PAWN_DISPLACEMENT_SIZE

    Equals to :c:macro:`CC_STEPS_PAWN_DISPLACEMENT_LEN` + 1.

.. c:macro:: CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_SIZE

    Equals to :c:macro:`CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_LEN` + 1.


.. _lbl-libcc-cctypedstepdefs-arrays:

Arrays
------

All arrays, beside their length and size, have a terminating position,
similar to null-terminating strings; terminating position is
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


.. c:type:: CcTypedStep const CC_STEPS_SERPENT_LEFT[ CC_STEPS_DIAGONAL_SERPENT_SIZE ]

    Steps array for Serpents featuring left diagonal, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_SERPENT_RIGHT[ CC_STEPS_DIAGONAL_SERPENT_SIZE ]

    Steps array for Serpents featuring right diagonal, for all variants.

.. c:macro:: CC_STEPS_ALL_SERPENT

    Steps array for Serpents, for all variants; includes Serpent's left and
    right diagonal steps, color-changing steps and Pawn displacement steps.

    Displacements here are steps made by Pawns when encountered by Serpent.


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


.. c:type:: CcTypedStep const CC_STEPS_PAWN_DISPLACEMENT[ CC_STEPS_PAWN_DISPLACEMENT_SIZE ]

    Displacement steps array for Pawn, initiated by Serpent, for all variants.

.. c:type:: CcTypedStep const CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY[ CC_STEPS_DISPLACEMENT_TRANCE_JOURNEY_SIZE ]

    Displacement steps array for trance-journey initiated by light Shaman,
    for all variants.

    Displacements here are steps made by any pieces encountered by trance-journey
    Shaman.


.. _lbl-libcc-cctypedstepdefs-validity:

Validity
--------

.. c:function:: bool cc_is_typed_step_valid( CcTypedStep step, CcStepTypeEnum filter__d, CcTypedStep const steps[], size_t steps_len__d )

    Function checking if step is valid, by searching a given array holding all
    valid steps.

    :param step: A step to check.
    :param filter__d: *Optional* value to filter out :c:var:`steps` array by type,
        can be :c:enumerator:`CC_STE_None`, in which case steps are not filtered.
    :param steps: An array of all valid steps.
    :param steps_len__d: *Optional* parameter, array length.
    :returns: :c:data:`true` if step is valid, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_get_step_type`

.. _lbl-libcc-cctypedstepdefs-steptype:

Step type
---------

.. c:function:: CcStepTypeEnum cc_get_step_type( CcPos step, CcStepTypeEnum filter__d, CcTypedStep const steps[], size_t steps_len__d )

    Function returns type of a given step, by searching a given array.

    Returned type also serves as a check if step is valid for a piece, if given
    array holds all valid steps for that piece. This is not the case for Monolith,
    Starchild, trance-journey Shamans, etc.; such steps needs to be calculated.

    If :c:`steps_len__d` is not used (i.e. it's :c:macro:`CC_STEPS_LEN_GUARD_DATA_TERMINATED`),
    :c:`steps` array must be terminated with invalid step (i.e. :c:macro:`CC_TYPED_STEP_INVALID`)
    as a guard.

    :param step: A step to check.
    :param filter__d: *Optional* value to filter out :c:var:`steps` array by type,
        can be :c:enumerator:`CC_STE_None`, in which case steps are not filtered.
    :param steps: An array of all valid steps.
    :param steps_len__d: *Optional* parameter, array length.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
        :c:enumerator:`CC_STE_None` otherwise.

.. _lbl-libcc-cctypedstepdefs-steptypemacros:

Step type macros
^^^^^^^^^^^^^^^^

    The step type macro conveniences are meant to be used instead of :c:func:`cc_get_step_type()`.

.. c:macro:: CC_GET_LIGHT_PAWN_STEP_TYPE(step)

    Macro to get :c:`step` type made by light Pawn, from Classical Chess up to,
    and including, Miranda's Veil variant.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_DARK_PAWN_STEP_TYPE(step)

    Macro to get :c:`step` type made by dark Pawn, from Classical Chess up to,
    and including, Miranda's Veil variant.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_LIGHT_SIDEWAYS_PAWN_STEP_TYPE(step)

    Macro to get :c:`step` type made by light Pawn, from Nineteen variant onwards.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_DARK_SIDEWAYS_PAWN_STEP_TYPE(step)

    Macro to get :c:`step` type made by dark Pawn, from Nineteen variant onwards.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.


.. c:macro:: CC_GET_KNIGHT_STEP_TYPE(step)

    Macro to get :c:`step` type made by Knight.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_BISHOP_STEP_TYPE(step)

    Macro to get :c:`step` type made by Bishop.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_ROOK_STEP_TYPE(step)

    Macro to get :c:`step` type made by Rook.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_QUEEN_STEP_TYPE(step)

    Macro to get :c:`step` type made by Queen.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_KING_STEP_TYPE(step)

    Macro to get :c:`step` type made by King.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.


.. c:macro:: CC_GET_PEGASUS_STEP_TYPE(step)

    Macro to get :c:`step` type made by Pegasus.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_PYRAMID_STEP_TYPE(step)

    Macro to get :c:`step` type made by Pyramid.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_UNICORN_SHORT_STEP_TYPE(step)

    Macro to get :c:`step` type made by Unicorn featuring
    short step.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_UNICORN_LONG_STEP_TYPE(step)

    Macro to get :c:`step` type made by Unicorn featuring
    long step.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.


.. c:macro:: CC_GET_STAR_STEP_TYPE(step)

    Macro to get :c:`step` type made by Star.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_CENTAUR_SHORT_STEP_TYPE(step)

    Macro to get :c:`step` type made by Centaur featuring
    short step.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_CENTAUR_LONG_STEP_TYPE(step)

    Macro to get :c:`step` type made by Centaur featuring
    long step.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_SERPENT_LEFT_STEP_TYPE(step)

    Macro to get :c:`step` type made by Serpent featuring
    left diagonal step.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_SERPENT_RIGHT_STEP_TYPE(step)

    Macro to get :c:`step` type made by Serpent featuring
    right diagonal step.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_SERPENT_STEP_TYPE(step)

    Macro to get :c:`step` type made by Serpent.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.


.. c:macro:: CC_GET_LIGHT_SCOUT_STEP_TYPE(step)

    Macro to get :c:`step` type made by light Scout.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_DARK_SCOUT_STEP_TYPE(step)

    Macro to get :c:`step` type made by dark Scout.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_GRENADIER_STEP_TYPE(step)

    Macro to get :c:`step` type made by Grenadier.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.


.. c:macro:: CC_GET_LIGHT_SHAMAN_STEP_TYPE(step)

    Macro to get :c:`step` type made by light Shaman.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_DARK_SHAMAN_STEP_TYPE(step)

    Macro to get :c:`step` type made by dark Shaman.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. c:macro:: CC_GET_STARCHILD_MIRACLE_STEP_TYPE(step)

    Macro to get :c:`step` type made by Starchild.

    :param step: A step to check.
    :returns: valid :c:enum:`CcStepTypeEnum` value if step is valid,
      :c:enumerator:`CC_STE_None` otherwise.

.. _lbl-libcc-cctypedstepdefs-functions:

Functions
---------

.. c:function:: bool cc_convert_typed_steps_to_links( CcTypedStep const steps[], size_t steps_len__d, CcTypedStepLink ** steps__o )

    Function converts typed steps array into a newly allocated linked list.

    *Output* argument :c:`*steps__o` has to be :c:data:`NULL`; appending
    steps to the same linked list is not supported.

    If :c:`steps_len__d` is :c:macro:`CC_STEPS_LEN_GUARD_DATA_TERMINATED`,
    :c:`steps` array **must** be terminated with invalid step
    (i.e. :c:macro:`CC_TYPED_STEP_INVALID`) as a guard.

    :param steps: A piece step array.
    :param steps_len__d: *Optional*; size of :c:`steps` array.
    :param steps__o: *Output*; newly allocated linked list.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-cctypedstepdefs-typedsteps:

Typed steps
-----------

.. c:function:: bool cc_iter_typed_steps( CcTypedStep const steps[], size_t steps_len__d, CcStepTypeEnum filter__d, CcTypedStep const ** step__iod )

    Function iterates over steps in a given array.

    .. note::

        Inner *output* pointer :c:`*step__iod` **must** be reset to :c:data:`NULL`
        before iterating steps.

    After each call, function returns :c:data:`true` if next step is valid, and
    sets pointer to it.

    Once all steps are exhausted, function returns :c:data:`false`, and resets
    pointer to :c:data:`NULL`.

    Typical usage:

    .. code-block:: C
        :force:

        // Typed step storage during iteration.
        CcTypedStep const * step = NULL;
        // Note: pointer must be set to NULL before iterating steps.

        while ( cc_iter_typed_steps( ..., &step ) ) {
            // Do stuff with found step, ...
        }

        // After iteration, step is reset to NULL, and ready for another.

    :param steps: A piece step array.
    :param steps_len__d: *Optional*; size of :c:`steps` array.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :param step__iod: *Input/output*, *optional*; iteration step.
    :returns: :c:data:`true` while successful, :c:data:`false` otherwise.

.. _lbl-libcc-cctypedstepdefs-typedstepsmacros:

Typed steps macros
^^^^^^^^^^^^^^^^^^

    The typed steps macro conveniences are meant to be used instead of
    :c:func:`cc_iter_typed_steps()`.

.. c:macro:: CC_ITER_LIGHT_PAWN_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by light Pawn, from Classical Chess up to,
    and including, Miranda's Veil variant.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_DARK_PAWN_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by dark Pawn, from Classical Chess up to,
    and including, Miranda's Veil variant.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_LIGHT_SIDEWAYS_PAWN_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by light Pawn, from Nineteen variant onwards.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_DARK_SIDEWAYS_PAWN_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by dark Pawn, from Nineteen variant onwards.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.


.. c:macro:: CC_ITER_KNIGHT_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Knight.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_BISHOP_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Bishop.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_ROOK_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Rook.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_QUEEN_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Queen.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_KING_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by King.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.


.. c:macro:: CC_ITER_PEGASUS_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Pegasus.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_PYRAMID_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Pyramid.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_UNICORN_SHORT_STEPS(step__iod,filter__d)

    Macro to iterate over short steps made by Unicorn.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_UNICORN_LONG_STEPS(step__iod,filter__d)

    Macro to iterate over long steps made by Unicorn.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.


.. c:macro:: CC_ITER_STAR_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Star.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_CENTAUR_SHORT_STEPS(step__iod,filter__d)

    Macro to iterate over short steps made by Centaur.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_CENTAUR_LONG_STEPS(step__iod,filter__d)

    Macro to iterate over long steps made by Centaur.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.


.. c:macro:: CC_ITER_SERPENT_LEFT_STEPS(step__iod,filter__d)

    Macro to iterate over left steps made by Serpent.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_SERPENT_RIGHT_STEPS(step__iod,filter__d)

    Macro to iterate over right steps made by Serpent.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_SERPENT_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Serpent.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.


.. c:macro:: CC_ITER_LIGHT_SCOUT_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by light Scout.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_DARK_SCOUT_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by dark Scout.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_GRENADIER_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by Grenadier.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.


.. c:macro:: CC_ITER_LIGHT_SHAMAN_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by light Shaman.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_DARK_SHAMAN_STEPS(step__iod,filter__d)

    Macro to iterate over steps made by dark Shaman.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:macro:: CC_ITER_STARCHILD_MIRACLE_STEPS(step__iod,filter__d)

    Macro to iterate over miracle-steps made by Starchild.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.


.. c:macro:: CC_ITER_DISPLACEMENT_TRANCE_JOURNEY_STEPS(step__iod,filter__d)

    Macro to iterate over displacement steps in a trance-journey.

    :param step__iod: *Input/output*, *optional*; iteration step.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. _lbl-libcc-cctypedstepdefs-stepiterators:

Step iterators
--------------

.. c:function:: bool cc_iter_monolith_steps( cc_uint_t step_index, CcTypedStep * step__io )

    Function iterates over Monolith steps for a given step index.

    Step index is simply count of steps taken so far, and effectively is the same
    as momentum.

    Step index ``0`` is meant for starting position, and does not iterate over anything.

    Step index is limited to ``24``, since for every step both starting position
    and destination has to be on a chessboard.

    .. note::

        *Output* pointer :c:`step__io` **must** be reset to
        :c:data:`CC_TYPED_STEP_CAST_INVALID` before iterating steps.

    After each call, function calculates next step, and returns :c:data:`true`
    if calculated step is valid.

    Once all steps are exhausted, function returns :c:data:`false`, and resets
    *output* step parameter to :c:data:`CC_TYPED_STEP_CAST_INVALID`.

    Typical usage:

    .. code-block:: C
        :force:

        // Typed step storage during iteration.
        CcTypedStep step = CC_TYPED_STEP_CAST_INVALID;
        // Note: pointer must be reset to CC_TYPED_STEP_CAST_INVALID before iterating steps.

        while ( cc_iter_monolith_steps( ..., &step ) ) {
            // Do stuff with step, ...
        }

        // After iteration, step is reset to CC_TYPED_STEP_CAST_INVALID, and ready for another.

    :param step_index: A piece step array.
    :param step__io: *Input/output*; iteration step.
    :returns: :c:data:`true` while successful, :c:data:`false` otherwise.

.. c:function:: bool cc_iter_piece_steps( CcPieceTagType piece, bool sideways_pawns, bool short_step, CcMaybeBoolEnum serpent_diagonal, CcStepTypeEnum filter__d, CcTypedStep const ** step__iod )

    Function iterates over steps a given piece can make.

    If Pawn was given, :c:var:`sideways_pawns` flag determines if sideways steps are also iterated over.

    If Unicorn or Centaur is given, :c:var:`short_step` flag determines if short or long steps are iterated over.

    If Serpent was given, :c:var:`serpent_diagonal` flag determines which Serpent's diagonal(s) to iterate over:

        * :c:enumerator:`CC_MBE_True`, only steps on right diagonal are iterated
        * :c:enumerator:`CC_MBE_False`, only steps on left diagonal are iterated
        * :c:enumerator:`CC_MBE_Void`, all steps are iterated

    Steps on right diagonal are upper-right, lower-left; on left diagonal they are upper-left, lower-right.

    .. note::

        Function does not handle neither Monolith, nor Waves.

        For Monolith, use :c:func:`cc_iter_monolith_steps()` instead.

    Type of steps to iterate over are given by *optional* :c:var:`filter__d` argument, if :c:enumerator:`CC_STE_None` all types of steps would be iterated.

    .. note::

        Inner *output* pointer :c:`*step__iod` **must** be reset to :c:data:`NULL`
        before iterating steps.

    After each call, function returns :c:data:`true` if next step is valid, and
    sets pointer to it.

    Once all steps are exhausted, function returns :c:data:`false`, and resets
    pointer to :c:data:`NULL`.

    Typical usage:

    .. code-block:: C
        :force:

        // Typed step storage during iteration.
        CcTypedStep const * step = NULL;
        // Note: pointer must be set to NULL before iterating steps.

        while ( cc_iter_piece_steps( ..., &step ) ) {
            // Do stuff with found step, ...
        }

        // After iteration, step is reset to NULL, and ready for another.

    :param piece: A piece.
    :param sideways_pawns: A flag, whether Pawns can move sideways, or not.
    :param short_step: A flag, whether Unicorns and Centaurs move over short or long steps.
    :param serpent_diagonal: A flag, which Serpent's diagonal(s) to iterate over.
    :param filter__d: *Optional*; type of steps to iterate over, can be
        :c:enumerator:`CC_STE_None`, in which case all steps would be iterated.
    :param step__iod: *Input/output*, *optional*; iteration step.
    :returns: :c:data:`true` while successful, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_iter_typed_steps()`, :c:func:`cc_iter_monolith_steps()`

.. c:function:: CcTypedStep cc_fetch_piece_step( CcPieceTagType piece, CcPos pos, CcPieceTagType activator, cc_uint_t board_size, CcTypedStep step_1, CcTypedStep step_2 )

    Function returns next step for a given piece based on its activator and
    current position.

    For one-step pieces (and Waves activated by those), function returns the first
    step that was given, i.e. :c:var:`step_1`.

    For two-step pieces (and Waves activated by those), function finds alternating
    step based on current position, either :c:var:`step_1` or :c:var:`step_2`.

    For other pieces, function fails with :c:macro:`CC_TYPED_STEP_CAST_INVALID`.

    :param piece: A piece.
    :param pos: Current position.
    :param activator: A piece, an activator.
    :param board_size: Board size.
    :param step_1: Step.
    :param step_2: Alternating step.
    :returns: Next step if successful, :c:macro:`CC_TYPED_STEP_CAST_INVALID` otherwise.
    :seealso: :c:func:`cc_piece_is_one_step()`, :c:func:`cc_piece_is_two_step()`, :c:func:`cc_piece_is_many_steps()`,

.. _lbl-libcc-cctypedstepdefs-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_typed_step_defs.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_typed_step_defs.h
    :language: C
    :linenos:

.. _lbl-libcc-cctypedstepdefs-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_typed_step_defs.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_typed_step_defs.c
    :language: C
    :linenos:
