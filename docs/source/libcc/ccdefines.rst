.. Copyright (c) 2021, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccdefines:

Defines
=======

Documents ``cc_defines.h`` file, which contains constants and macros used throughout project.

.. _lbl-libcc-ccdefines-types:

Types
-----

Convenience shorthands for signed integer types.

.. c:type:: signed char cc_schar_t

.. c:type:: signed short cc_sshort_t

.. c:type:: signed int cc_sint_t

.. c:type:: signed long cc_slong_t

.. c:type:: signed long long cc_sll_t

Convenience shorthands for unsigned integer types.

.. c:type:: unsigned char cc_uchar_t

.. c:type:: unsigned short cc_ushort_t

.. c:type:: unsigned int cc_uint_t

.. c:type:: unsigned long cc_ulong_t

.. c:type:: unsigned long long cc_ull_t

.. c:macro:: CC_UNSIGNED_MIN

    Constant, minimum value for all :c:`unsigned` types; equals to ``0``.

.. _lbl-libcc-ccdefines-macros:

Macros
------

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

    Sign of a number is defined as ``1`` for positive numbers,
    ``-1`` for negative numbers, ``0`` otherwise.

    :param i: A number, value is not cast.
    :returns: Larger value of the two given.

.. c:macro:: CC_XOR(to_bool_1,to_bool_2)

    Macro to evaluate logical XOR, i.e. to check if one or the other :c:`bool`
    value is :c:data:`true`, but not both.

    :param to_bool_1: Value cast to :c:`bool`.
    :param to_bool_2: Value cast to :c:`bool`.
    :returns: :c:`bool` value.

    .. note::

        In case of integer(s), one has to be non-zero, while the other has to be zero,
        for :c:macro:`CC_XOR` to return :c:data:`true`. Arguments are converted to :c:`bool`\s
        (so, non-zero integer is ``1``, otherwise it's ``0``), then they are compared.

    .. seealso::

        `<https://en.wikipedia.org/wiki/Bitwise_operations_in_C#Logical_equivalents>`_,
        `<https://www.reddit.com/r/C_Programming/comments/2cruz3/comment/cjih6wt/>`_

.. c:macro:: CC_FREE(ptr)

    Macro to call :c:func:`free()`, given pointer is casted to :c:`void *`.

    :param ptr: Any pointer to allocated storage.
    :returns: Nothing.

.. c:macro:: CC_FREE_AND_NULL(ptr_ptr)

    Macro to call :c:func:`free()`, inner pointer is casted to :c:`void *`
    before the call, then set to :c:data:`NULL`.

    :param ptr_ptr: A pointer to pointer to allocated storage.
    :returns: Nothing.

.. c:macro:: CC_MALLOC(size)

    Macro to allocate and zero memory, via a call to :c:func:`calloc()` with a given size.

    :param size: Size in bytes to allocate.
    :returns: A valid void pointer to allocated memory if successful,
        :c:data:`NULL` otherwise.
    :seealso: :c:func:`calloc()`

.. c:macro:: CC_PRINTF(fmt,...)

    Macro to call :c:func:`printf()`, depending on a compile-time constant.

    Compile-time constant which controls definition of this macro is :c:macro:`__CC_DEBUG__`.

    :param fmt: Format string.
    :param ...: Variadic input for a string format.
    :returns: An integer; number of characters written to output stream, or
        negative value if an error occurred.
    :seealso: https://en.cppreference.com/w/c/io/fprintf

.. _lbl-libcc-ccdefines-maybebool:

Maybe bool
----------

.. c:enum:: CcMaybeBoolEnum

    Maybe bool enum represents :c:`bool` values, which may be undefined,
    uninitialized, or error value.

    .. c:enumerator:: CC_MBE_Void

        Void (undefined, uninitialized, or error) value; equals to ``-1``.

    .. c:enumerator:: CC_MBE_False

        Boolean :c:data:`false` value; equals to ``0``.

    .. c:enumerator:: CC_MBE_True

        Boolean :c:data:`true` value; equals to ``1``.

    :c:`enum` is tagged with the same :c:enum:`CcMaybeBoolEnum` name.

.. c:macro:: CC_MAYBE_BOOL_IS_ENUMERATOR(mbe)

    Macro to check if given piece is :c:type:`CcMaybeBoolEnum` enumerator, i.e.
    between :c:enumerator:`CC_MBE_Void` and :c:enumerator:`CC_MBE_True` values.

    :param mbe: Maybe bool, integer value.
    :returns: :c:data:`true` if :c:type:`CcMaybeBoolEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_MAYBE_BOOL_IS_VALID(mbe)

    Macro to check if given piece is a valid :c:type:`CcMaybeBoolEnum` enumerator,
    i.e. either :c:enumerator:`CC_MBE_False` or :c:enumerator:`CC_MBE_True`.

    :param mbe: Maybe bool, integer value.
    :returns: :c:data:`true` if valid :c:type:`CcMaybeBoolEnum` enumerator,
              :c:data:`false` otherwise.

.. c:macro:: CC_BOOL_TO_MAYBE(bool_val)

    Macro to convert :c:`bool` value into :c:enum:`CcMaybeBoolEnum`.

    :param bool_val: Boolean value.
    :returns: `CcMaybeBoolEnum` value.

.. _lbl-libcc-ccdefines-coordinates:

Coordinates
-----------

.. c:macro:: CC_INVALID_COORD

    Constant, invalid off-board coordinate; equals to :c:macro:`INT_MIN` + 3583,
    so that value can't be had by accident, e.g. by simply flipping bits.

    No valid trance- or any other journey off-board starting from any field could
    get to this coordinate, and make it back to chessboard.

    Used for initializing variables, missing coordinates in disambiguations, etc.

.. c:macro:: CC_MIN_BOARD_COORD

    The smallest valid on-board coordinate; equals to ``0``.

.. c:macro:: CC_MAX_BOARD_COORD

    The largest valid on-board coordinate; equals to ``25``.

.. c:macro:: CC_MIN_BOARD_SIZE

    The smallest valid board size, used by Classic Chess; equals to ``8``.

.. c:macro:: CC_MAX_BOARD_SIZE

    The largest valid board size; equals to ``26``.

    The largest valid coordinate and board size are for the largest board, used by
    One variant. For other variants actual upper limit is smaller.

    :seealso: :c:func:`cc_variant_board_size()`

.. c:macro:: CC_FIELD_COLOR_LIGHT

    Light field check constant; equals to ``1``.

.. c:macro:: CC_FIELD_COLOR_DARK

    Dark field check constant; equals to ``0``.

    Light and dark field check constants are used when checking if colors
    of a piece and field are the same, or different.

.. c:macro:: CC_IS_CHAR_COORD(chr)

    Macro to check if a given :c:var:`chr` is notational coordinate, either lowercase
    :c:`char` or a digit.

    :param chr: :c:`char`\acter to check.
    :returns: ``1`` if coordinate, ``0`` otherwise.

.. _lbl-libcc-ccdefines-conversions:

Conversions
-----------

.. c:macro:: CC_CONVERT_BYTE_INTO_FILE_CHAR(byte_file)

    Macro to convert numerical file value into char.

    .. note::

        Value of :c:`byte_file` is expected to be in a range of [``0``, ``25``],
        undefined result if it's not.

    :param byte_file: Rank, position along vertical axis, numerical value.
    :returns: File character if argument within range, undefined result otherwise.

.. c:macro:: CC_CONVERT_FILE_CHAR_INTO_NUM(char_file)

    Macro to convert char into numerical file value.

    .. note::

        Value of :c:`char_file` is expected to be in a range of [``'a'``, ``'z'``],
        undefined result if it's not.

    :param char_file: Rank, position along vertical axis, char value.
    :returns: File number if argument within range, undefined result otherwise.

.. c:macro:: CC_CONVERT_RANK_STR_INTO_NUM(char_ptr_rank)

    Macro to convert string into numerical rank value, using :c:func:`atoi()` function.

    .. warning::

        Given string must be null-terminated, undefined behavior otherwise.

    .. seealso::

        `<https://en.cppreference.com/w/c/string/byte/atoi>`_

    :param char_ptr_rank: Rank, position along vertical axis, string pointer value, i.e. :c:expr:`char const *`.
    :returns: Rank number if successful, undefined behavior otherwise.

.. _lbl-libcc-ccdefines-coordinatesfieldspositions:

Coordinates, fields, positions
------------------------------

.. c:macro:: CC_IS_COORD_VALID(coord)

    Macro to check if a given coordinate is valid, i.e. different than
    :c:macro:`CC_INVALID_COORD`.

    :param coord: Coordinate; integer value.
    :returns: ``1`` if valid, ``0`` otherwise.

.. c:macro:: CC_IS_COORD_2_VALID(i,j)

    Macro to check if a given position is valid, i.e. if given coordinates are
    different than :c:macro:`CC_INVALID_COORD`.

    :param i: File, position along horizontal axis; integer value.
    :param j: Rank, position along vertical axis; integer value.
    :returns: ``1`` if valid, ``0`` otherwise.

.. c:macro:: CC_IS_FIELD_LIGHT(i,j)

    Macro to check if a given position is light.

    :param i: File, position along horizontal axis; integer value.
    :param j: Rank, position along vertical axis; integer value.
    :returns: ``1`` if light, ``0`` otherwise.

.. c:macro:: CC_IS_FIELD_DARK(i,j)

    Macro to check if a given position is dark.

    :param i: File, position along horizontal axis; integer value.
    :param j: Rank, position along vertical axis; integer value.
    :returns: ``1`` if dark, ``0`` otherwise.

.. c:macro:: CC_IS_FIELD_COLOR(i,j,fc)

    Macro to check if a given field is light or dark.

    :param i: File, position along horizontal axis; integer value.
    :param j: Rank, position along vertical axis; integer value.
    :param fc: Field color, either :c:data:`CC_FIELD_COLOR_LIGHT`
        or :c:data:`CC_FIELD_COLOR_DARK`.
    :returns: ``1`` if field is in given color, ``0`` otherwise.

.. c:macro:: CC_IS_BOARD_SIZE_VALID(board_size)

    Macro to check if a given board size is valid,
    i.e. between :c:macro:`CC_MIN_BOARD_SIZE` and :c:macro:`CC_MAX_BOARD_SIZE`.

    :param board_size: Chessboard size, integer value; cast to :c:`int`.
    :returns: ``1`` if valid board size, ``0`` otherwise.

.. c:macro:: CC_IS_COORD_ON_BOARD(board_size,coord)

    Macro to check if a given coordinate is on board, i.e. larger than (or equal
    to) :c:macro:`CC_MIN_BOARD_COORD` and smaller than :c:`board_size`.

    .. note::

        This macro does not check if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param coord: Coordinate, cast to :c:`int`.
    :returns: ``1`` if on board, ``0`` otherwise.

.. c:macro:: CC_IS_POS_ON_BOARD(board_size,i,j)

    Macro to check if a given position is on board.

    .. note::

        This macro does not check if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param i: File, position along horizontal axis; cast to :c:`int`.
    :param j: Rank, position along vertical axis; cast to :c:`int`.
    :returns: ``1`` if on board, ``0`` otherwise.

.. c:macro:: CC_IS_DISAMBIGUATION_ON_BOARD(board_size,i,j)

    Macro to check if a given disambiguation (i.e. partial position) is on board.

    .. note::

        This macro does not check if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param i: File, position along horizontal axis; cast to :c:`int`.
    :param j: Rank, position along vertical axis; cast to :c:`int`.
    :returns: ``1`` if at least one coordinate is on board, ``0`` otherwise.

.. c:macro:: CC_IS_FIELD_ON_LIGHT_SIDE(board_size,rank)

    Macro to check if a given position is on a light side of a chessboard.

    This macro checks if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param rank: Rank, position along vertical axis; cast to :c:`int`.
    :returns: ``1`` if on light side, ``0`` otherwise.

.. c:macro:: CC_IS_FIELD_ON_DARK_SIDE(board_size,rank)

    Macro to check if a given position is on a dark side of a chessboard.

    This macro checks if board size is valid.

    :param board_size: Chessboard size, cast to :c:`int`.
    :param rank: Rank, position along vertical axis; cast to :c:`int`.
    :returns: ``1`` if on dark side, ``0`` otherwise.

.. _lbl-libcc-ccdefines-defaults:

Defaults
--------

.. c:macro:: CC_DEFAULT_VALUE_STRING

    Default entity string, equals to ``"<default>"``.

    It is used as default value when function has to return string value,
    usually based on some enum, e.g. a piece label.

    :seealso: :c:func:`cc_piece_label()`, :c:func:`cc_piece_as_string()`

.. _lbl-libcc-ccdefines-navigation:

Navigation
----------

.. c:macro:: CC_REWIND(ptr_var_queue)

    Macro to rewind queue pointer variable to its first item; if pointer is
    :c:data:`NULL`, nothing happens.

    .. warning::

        Pointer to queue :c:`ptr_var_queue` must be valid variable, not expression.

    .. warning::

        Queue :c:`struct` must have :c:`prev__w` member, which points to previous item in that queue.

    :param ptr_var_queue: A queue pointer variable.
    :returns: Nothing.

.. c:macro:: CC_FASTFORWARD(ptr_var_lst)

    Macro to fast-forward list pointer variable to its last item; if pointer is
    :c:data:`NULL`, nothing happens.

    .. warning::

        Pointer to list :c:`ptr_var_lst` must be valid variable, not expression.

    .. warning::

        List :c:`struct` must have :c:`next` member, which points to next item in that list.

    :param ptr_var_lst: A list pointer variable.
    :returns: Nothing.

.. c:macro:: CC_REWIND_BY(ptr_var_seq,ptr_item)

    Macro to rewind sequence pointer variable by one of its members; if pointer
    is :c:data:`NULL`, nothing happens.

    .. warning::

        Pointer to sequence :c:`ptr_var_seq` must be valid variable, not expression.

    :param ptr_var_seq: A sequence pointer variable.
    :param ptr_item: Pointer, member to iterate over.
    :returns: Nothing.

.. c:macro:: CC_ARRAY_SIZE(array)

    Macro to calculate size of a given array.

    .. warning::

        Does not work in functions, array parameters are converted into
        pointers.

        For instance, in function:

        .. code-block:: C
            :force:

            void print_array_param( int ar[] ) {
                printf("sizeof of parameter: %zu.\n", sizeof(ar));
                printf("Length of parameter: %zu.\n", ( sizeof(ar) / sizeof(ar[0]) ) );
            }

        array :c:`ar` is treated as pointer, so :c:expr:`sizeof(ar)` returns
        ``8`` (bytes).

        Similarly, :c:expr:`sizeof(ar) / sizeof(ar[0])` returns ``1``, which
        is just :c:expr:`8/8`, since both :c:`ar` and :c:`ar[0]` are pointers,
        hence ``8`` bytes each.

    .. seealso::

        https://stackoverflow.com/questions/37538/how-do-i-determine-the-size-of-my-array-in-c,
        search for "An array sent as a parameter to a function is treated as a pointer".

    :param array: An array variable.
    :returns: :c:type:`size_t` value, size of a given array.

.. _lbl-libcc-ccdefines-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_defines.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_defines.h
    :language: C
    :linenos:
