.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccdefines:

Defines
=======

Documents ``cc_defines.h`` file, which contains constants and macros used throughout project.

.. c:type:: unsigned char uchar

    Convenience type.

.. c:type:: unsigned short ushort

    Convenience type.

.. c:type:: unsigned int uint

    Convenience type.

.. c:macro:: CC_UNSIGNED_MIN

    Constant, minimum value for all :c:`unsigned` types; equals to :c:`0`.

.. c:enum:: CcMaybeBoolEnum

    Maybe bool enum represents :c:`bool` values, which may undefined
    or uninitialized, or has to differentiate when both :c:`true` and
    :c:`false` are valid data.

    .. c:enumerator:: CC_MBE_Void

        Void (undefined, uninitialized, or error) value; equals to :c:`-1`.

    .. c:enumerator:: CC_MBE_False

        Boolean :c:`false` value; equals to :c:`0`.

    .. c:enumerator:: CC_MBE_True

        Boolean :c:`true` value; equals to :c:`1`.

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

.. c:macro:: CC_INVALID_COORD

    Constant, invalid off-board coordinate; equals to :c:expr:`INT_MIN + 3583`,
    so that value can't be had by accident, e.g. by simply flipping bits.

    No valid trance- or any other journey off-board starting from any field could
    get to this coordinate, and make it back to chessboard.

    Used for initializing variables, missing coordinates in disambiguations, etc.

.. c:macro:: CC_MIN_BOARD_COORD

    The smallest valid on-board coordinate; equals to :c:`0`.

.. c:macro:: CC_MAX_BOARD_COORD

    The largest valid on-board coordinate; equals to :c:`25`.

.. c:macro:: CC_MIN_BOARD_SIZE

    The smallest valid board size, used by Classic Chess; equals to :c:`8`.

.. c:macro:: CC_MAX_BOARD_SIZE

    The largest valid board size; equals to :c:`26`.

The largest valid coordinate and board size are for the largest board, used by
One variant. For other variants actual upper limit is smaller.

.. seealso::
    :c:`cc_variant_board_size() /* TODO .. x-ref back */`

.. TODO .. , see `cc_variant_board_size()`.
    .. seealso::
        , see `cc_variant_board_size()`.

.. c:macro:: CC_FIELD_COLOR_LIGHT

    Light field check constant; equals to :c:`1`.

.. c:macro:: CC_FIELD_COLOR_DARK

    Dark field check constant; equals to :c:`0`.

    Light and dark field check constants are used when checking if colors
    of a piece and field are the same, or different.

.. c:macro:: CC_CONVERT_BYTE_INTO_FILE_CHAR(byte_file)

    Macro to convert numerical file value into char.

    .. warning::

        Value of :c:`byte_file` is expected to be in a range of [:c:`0`, :c:`25`],
        undefined behavior if it's not.

    :param byte_file: Rank, position along vertical axis, numerical value.
    :returns: File character if argument within range, undefined behavior otherwise.

.. c:macro:: CC_CONVERT_FILE_CHAR_INTO_NUM(char_file)

    Macro to convert char into numerical file value.

    .. warning::

        Value of :c:`char_file` is expected to be in a range of [:c:`'a'`, :c:`'z'`],
        undefined behavior if it's not.

    :param char_file: Rank, position along vertical axis, char value.
    :returns: File number if argument within range, undefined behavior otherwise.


.. _lbl-libcc-ccdefines-coordinateconversion:

Coordinate conversion
---------------------

.. code-block:: C
    :force:

    #define CC_CONVERT_RANK_STR_INTO_NUM(char_ptr_rank)
