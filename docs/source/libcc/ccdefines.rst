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

    Maybe bool enum represents :c:`bool` values, which may be undefined,
    uninitialized, or error value.

    .. c:enumerator:: CC_MBE_Void

        Void (undefined, uninitialized, or error) value; equals to :c:`-1`.

    .. c:enumerator:: CC_MBE_False

        Boolean :c:`false` value; equals to :c:`0`.

    .. c:enumerator:: CC_MBE_True

        Boolean :c:`true` value; equals to :c:`1`.

.. c:macro:: CC_BOOL_TO_MAYBE(bool_val)

    Macro to convert :c:`bool` value into :c:expr:`CcMaybeBoolEnum`.

    :param bool_val: Boolean value.
    :returns: `CcMaybeBoolEnum` value.

.. c:macro:: CC_MAYBE_IS_TRUE(maybe_bool)

    Macro to check if :c:expr:`CcMaybeBoolEnum` value is :c:`CC_MBE_True`.

    :param maybe_bool: :c:expr:`CcMaybeBoolEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_MAYBE_IS_FALSE(maybe_bool)

    Macro to check if :c:expr:`CcMaybeBoolEnum` value is :c:`CC_MBE_False`.

    :param maybe_bool: :c:expr:`CcMaybeBoolEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_MAYBE_IS_VOID(maybe_bool)

    Macro to check if :c:expr:`CcMaybeBoolEnum` value is :c:`CC_MBE_Void`.

    :param maybe_bool: :c:expr:`CcMaybeBoolEnum` value.
    :returns: :c:`bool` value.

.. c:macro:: CC_XOR(to_bool_1,to_bool_2)

    Macro to evaluate logical XOR, i.e. to check if one or the other :c:`bool`
    value is :c:`true`, but not both.

    :param to_bool_1: Value cast to :c:`bool`.
    :param to_bool_2: Value cast to :c:`bool`.
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

.. c:macro:: CC_CONVERT_RANK_STR_INTO_NUM(char_ptr_rank)

    Macro to convert string into numerical rank value, using :c:`atoi()` function.

    .. warning::

        Given string must be zero-terminated, undefined behavior otherwise.

    .. seealso::

        `<https://en.cppreference.com/w/c/string/byte/atoi>`_

    :param char_ptr_rank: Rank, position along vertical axis, string pointer value, i.e. :c:expr:`char const *`.
    :returns: Rank number if successful, undefined behavior otherwise.

.. c:macro:: CC_IS_COORD_VALID(coord)

    Macro to check if a given coordinate is valid, i.e. different than
    :c:expr:`CC_INVALID_COORD`.

    :param coord: Coordinate; integer value.
    :returns: :c:`1` if valid, :c:`0` otherwise.

.. c:macro:: CC_IS_COORD_2_VALID(i,j)

    Macro to check if a given position is valid, i.e. if given coordinates are
    different than :c:expr:`CC_INVALID_COORD`.

    :param i: File, position along horizontal axis; integer value.
    :param j: Rank, position along vertical axis; integer value.
    :returns: :c:`1` if valid, :c:`0` otherwise.

.. c:macro:: CC_IS_FIELD_LIGHT(i,j)

    Macro to check if a given position is light.

    :param i: File, position along horizontal axis; integer value.
    :param j: Rank, position along vertical axis; integer value.
    :returns: :c:`1` if light, :c:`0` otherwise.

.. c:macro:: CC_IS_FIELD_DARK(i,j)

    Macro to check if a given position is dark.

    :param i: File, position along horizontal axis; integer value.
    :param j: Rank, position along vertical axis; integer value.
    :returns: :c:`1` if dark, :c:`0` otherwise.

.. c:macro:: CC_IS_FIELD_COLOR(i,j,fc)

    Macro to check if a given field is light or dark.

    :param i: File, position along horizontal axis; integer value.
    :param j: Rank, position along vertical axis; integer value.
    :param fc: Field color, either :c:expr:`CC_FIELD_COLOR_LIGHT`
        or :c:expr:`CC_FIELD_COLOR_DARK`.
    :returns: :c:`1` if field is in given color, :c:`0` otherwise.

.. c:macro:: CC_IS_BOARD_SIZE_VALID(board_size)

    Macro to check if a given board size is valid,
    i.e. between :c:expr:`CC_MIN_BOARD_SIZE` and :c:expr:`CC_MAX_BOARD_SIZE`.

    :param board_size: Chessboard size, integer value; cast to :c:`int`.
    :returns: :c:`1` if valid board size, :c:`0` otherwise.

.. c:macro:: CC_IS_COORD_ON_BOARD(board_size,coord)

    Macro to check if a given coordinate is on board, i.e. larger than (or equal
    to) :c:expr:`CC_MIN_BOARD_COORD` and smaller than :c:`board_size`.

    .. note::

        This macro does not check if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param coord: Coordinate, cast to :c:`int`.
    :returns: :c:`1` if on board, :c:`0` otherwise.

.. c:macro:: CC_IS_COORD_ON_VALID_BOARD(board_size,coord)

    Macro to check if a given coordinate is on board, i.e. larger than (or equal
    to) :c:expr:`CC_MIN_BOARD_COORD` and smaller than :c:`board_size`.

    This macro checks if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param coord: Coordinate, cast to :c:`int`.
    :returns: :c:`1` if on board, :c:`0` otherwise.

.. c:macro:: CC_IS_POS_ON_BOARD(board_size,i,j)

    Macro to check if a given position is on board.

    This macro checks if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param i: File, position along horizontal axis; cast to :c:`int`.
    :param j: Rank, position along vertical axis; cast to :c:`int`.
    :returns: :c:`1` if on board, :c:`0` otherwise.

.. c:macro:: CC_IS_ANY_COORD_ON_BOARD(board_size,i,j)

    Macro to check if a given disambiguation (i.e. partial position) is on board.

    This macro checks if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param i: File, position along horizontal axis; cast to :c:`int`.
    :param j: Rank, position along vertical axis; cast to :c:`int`.
    :returns: :c:`1` if at least one coordinate is on board, :c:`0` otherwise.

.. c:macro:: CC_IS_FIELD_ON_LIGHT_SIDE(board_size,rank)

    Macro to check if a given position is on a light side of a chessboard.

    This macro checks if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param rank: Rank, position along vertical axis; cast to :c:`int`.
    :returns: :c:`1` if on light side, :c:`0` otherwise.

.. c:macro:: CC_IS_FIELD_ON_DARK_SIDE(board_size,rank)

    Macro to check if a given position is on a dark side of a chessboard.

    This macro checks if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param rank: Rank, position along vertical axis; cast to :c:`int`.
    :returns: :c:`1` if on dark side, :c:`0` otherwise.

.. c:macro:: CC_MIN(x,y)

    Macro to inline comparing, producing smaller value of the two given.

    .. note::
        Given values are not cast. Depending on their type(s), this might
        lead to undefined behavior (e.g. if not comparable pointers).

    .. seealso::
        `<https://en.cppreference.com/w/c/language/operator_comparison>`_

    :param x: A number, value is not cast.
    :param y: Other number, value is not cast.
    :returns: Smaller value of the two given.

.. c:macro:: CC_MAX(x,y)

    Macro to inline comparing, producing larger value of the two given.

    .. note::
        Given values are not cast. Depending on their type(s), this might
        lead to undefined behavior (e.g. if not comparable pointers).

    .. seealso::
        `<https://en.cppreference.com/w/c/language/operator_comparison>`_

    :param x: A number, value is not cast.
    :param y: Other number, value is not cast.
    :returns: Larger value of the two given.

.. c:macro:: CC_SIGN(i)

    Macro to inline sign function.

    Sign of a number is defined as :c:`1` for positive numbers,
    :c:`-1` for negative numbers, :c:`0` otherwise.

    :param i: A number, value is not cast.
    :returns: Larger value of the two given.

.. c:macro:: CC_FREE(ptr)

    Macro to call :c:expr:`free()`, given pointer is casted to :c:`void *`.

    :param ptr: Any pointer to allocated storage.
    :returns: Nothing.

.. c:macro:: CC_FREE_AND_NULL(ptr_ptr)

    Macro to call :c:expr:`free()`, inner pointer is casted to :c:`void *`
    before the call, then set to :c:`NULL`.

    :param ptr_ptr: A pointer to pointer to allocated storage.
    :returns: Nothing.

.. c:macro:: CC_DEFAULT_ENTITY_STRING

    Default entity string, equals to :c:`"<default>"`.

    It is used as default value when function has to return string value,
    usually based on some enum, e.g. a piece label.

    .. seealso::
        :c:`cc_piece_label() /* TODO .. x-ref back */`

    .. TODO .. , see `cc_piece_label()`.
        .. seealso::
            , see `cc_piece_label()`.

.. c:macro:: CC_REWIND(ptr_var_queue)

    Macro to rewind queue pointer to its first item.

    .. warning::

        Pointer to queue :c:`ptr_var_queue` must be valid pointer, i.e. *must* not be :c:`NULL`.

    .. warning::

        Pointer to queue :c:`ptr_var_queue` must be valid variable, not expression.

    .. warning::

        Queue :c:`struct` must have :c:`prev` member, which points to previous item in that queue.

    :param ptr_var_queue: A queue pointer variable.
    :returns: Nothing.

.. c:macro:: CC_FASTFORWARD(ptr_var_lst)

    Macro to fast-forward list pointer to its last item.

    .. warning::

        Pointer to list :c:`ptr_var_lst` must be valid pointer, i.e. *must* not be :c:`NULL`.

    .. warning::

        Pointer to list :c:`ptr_var_lst` must be valid variable, not expression.

    .. warning::

        List :c:`struct` must have :c:`next` member, which points to next item in that list.

    :param ptr_var_lst: A list pointer variable.
    :returns: Nothing.

.. c:macro:: CC_REWIND_BY(ptr_var_seq,ptr_item)

    Macro to rewind sequence pointer by one of its members.

    .. warning::

        Pointer to sequence :c:`ptr_var_seq` must be valid pointer, i.e. *must* not be :c:`NULL`.

    .. warning::

        Pointer to sequence :c:`ptr_var_seq` must be valid variable, not expression.

    :param ptr_var_seq: A sequence pointer variable.
    :param ptr_item: Pointer, member to iterate over.
    :returns: Nothing.

.. c:macro:: CC_PRINTF_IF_INFO(fmt,...)

    Macro to call :c:`printf()`, depending on a compile-time constant.

    Compile-time constant which controls definition of this macro is :c:`__CC_STR_PRINT_INFO__`.

    .. seealso::

        `<https://en.cppreference.com/w/c/io/fprintf>`_

    :param fmt: Formatting string.
    :param ...: Variadic parameters, as used by :c:`printf()`.
    :returns: The same as :c:`printf()`, i.e. an :c:`int` value.
              Number of :c:`char`\s printed, an error code if negative.
