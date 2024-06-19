.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccdefines:

Defines
=======

Documents ``cc_defines.h`` file, which contains constants and macros used throughout project.

Convenience types.

.. c:type:: unsigned char uchar
.. c:type:: unsigned short ushort
.. c:type:: unsigned int uint

.. c:macro:: CC_UNSIGNED_MIN

    Constant representing minimum value for all :c:`unsigned` types.

.. c:enum:: CcMaybeBoolEnum

    Maybe bool enum represents :c:`bool` values, which may undefined
    or uninitialized, or has to differentiate when both :c:`true` and
    :c:`false` are valid data.

    .. c:enumerator::
        CC_MBE_Void
        CC_MBE_False
        CC_MBE_True

Macros to convert from and into :c:`bool` value.

.. c:macro:: CC_BOOL_TO_MAYBE(bool_val)

    Macro to convert :c:`bool` value into :c:`CcMaybeBoolEnum`.

    :param bool_val: Boolean value.
    :returns: `CcMaybeBoolEnum` value.

.. c:macro:: CC_MAYBE_IS_TRUE(maybe_bool)

    Macro to check if :c:`CcMaybeBoolEnum` value is :c:`CC_MBE_True`.

    :param maybe_bool: :c:`CcMaybeBoolEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_MAYBE_IS_FALSE(maybe_bool)

    Macro to check if :c:`CcMaybeBoolEnum` value is :c:`CC_MBE_False`.

    :param maybe_bool: :c:`CcMaybeBoolEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_MAYBE_IS_VOID(maybe_bool)

    Macro to check if :c:`CcMaybeBoolEnum` value is :c:`CC_MBE_Void`.

    :param maybe_bool: :c:`CcMaybeBoolEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_XOR(to_bool_1,to_bool_2)

    Macro to evaluate logical XOR, i.e. to check if one or the other :c:`bool`
    value is :c:`true`, but not both.

    :param to_bool_1: Value coerced to :c:`bool`.
    :param to_bool_2: Value coerced to :c:`bool`.
    :returns: :c:`bool` value.

    .. note::

        In case of integer(s), one has to be non-zero, while the other has to be zero,
        for :c:expr:`CC_XOR` to return :c:`true`. Arguments are converted to :c:`bool`\s
        (so, non-zero integer is :c:`1`, otherwise it's :c:`0`), then they are compared.

    .. seealso::

        `<https://en.wikipedia.org/wiki/Bitwise_operations_in_C#Logical_equivalents>`_,
        `<https://www.reddit.com/r/C_Programming/comments/2cruz3/comment/cjih6wt/>`_


.. _lbl-libcc-ccdefines-coordinatesizeconstants:

Coordinate, size constants
--------------------------

Invalid, off-board coordinate.

No valid trance-journey starting from any chessboard field could get to this coordinate,
and still make it back to any on-board field.

Used for e.g. missing coordinates.

.. code-block:: C
    :force:

    #define CC_INVALID_COORD (INT_MIN + 3583) // + number, so that value can't be get by accident, e.g. by simply flipping bits, ...

The smallest and the largest valid on-board coordinate, and board sizes.

The largest valid coordinate, board size is for the largest board, used by One
variant. For other variants actual upper limit is smaller.

.. TODO
    .. sealso::
        , see `cc_variant_board_size()`.

.. code-block:: C
    :force:

    #define CC_MIN_BOARD_COORD (0)
    #define CC_MAX_BOARD_COORD (25)

    #define CC_MIN_BOARD_SIZE (8)
    #define CC_MAX_BOARD_SIZE (26)

Light and dark field check constant. Used when checking if colors of a piece and
field are the same, or different.

.. code-block:: C
    :force:

    #define CC_FIELD_COLOR_LIGHT (1)
    #define CC_FIELD_COLOR_DARK (0)

.. _lbl-libcc-ccdefines-coordinateconversion:

Coordinate conversion
---------------------

Macro to convert numerical file value into char.

@param byte_file Rank, position along vertical axis, numerical value.

@warning
Value of `byte_file` is expected to be in a range of [0, 25],
undefined behavior if it's not.

@return File character if argument within range, undefined behavior otherwise.

.. code-block:: C
    :force:

    #define CC_CONVERT_BYTE_INTO_FILE_CHAR(byte_file)

.. code-block:: C
    :force:

    #define CC_CONVERT_FILE_CHAR_INTO_NUM(char_file)

.. code-block:: C
    :force:

    #define CC_CONVERT_RANK_STR_INTO_NUM(char_ptr_rank)
