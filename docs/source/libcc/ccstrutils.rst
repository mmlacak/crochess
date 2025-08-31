.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccstrutils:

String utilities
================

Documents ``cc_str_utils.h`` and ``cc_str_utils.c`` files, which contain
strings, :c:`char` arrays utility functions.

All functions which return newly allocated string, return them
null-terminated (``'\0'``).

Length of a string counts only content, without null-terminating character.
This is the same as :c:func:`strlen()`, see
`<https://en.cppreference.com/w/c/string/byte/strlen>`_.

Size of a string **includes** terminating character.

Length of :c:`char` arrays is the same as its size, regardless of length
of its content, if it's null-terminated, or not.

When content in :c:`char` array is shorter than the array itself, it is
null-terminated.

Utility functions have string and its maximum length parameters in pairs.

When calling such functions, :c:macro:`CC_MAX_LEN_ZERO_TERMINATED` can be
used as an argument to maximum length parameter; if so, there is no limit
on string length, and string itself **must** be null-terminated (``'\0'``).

For :c:`char` arrays always do use appropriate maximum length argument,
e.g. :c:macro:`CC_MAX_LEN_CHAR_8`.

.. _lbl-libcc-ccstrutils-sizeslengths:

Sizes, lengths
--------------

.. c:macro:: CC_MAX_LEN_ZERO_TERMINATED

    Constant to ignore maximum length constraint in functions, equals to ``0``.

    If used, entirety of a given string is processed, which then **must** be
    null-terminated (i.e. end with ``'\0'``).

.. c:macro:: CC_SIZE_BUFFER

    Constant to set buffer size, equals to ``8192``.

.. c:macro:: CC_MAX_LEN_BUFFER

    Constant to set maximum length of a buffer, equals to :c:macro:`CC_SIZE_BUFFER` - 1.

.. c:macro:: CC_SIZE_IGNORE

    Invalid size, to flag size argument to be ignored, equals to ``0``.

.. c:macro:: CC_SIZE_CHAR_8

    Size of an 8 :c:`char` array, equals to ``8``.

.. c:macro:: CC_MAX_LEN_CHAR_8

    Maximum length of an 8 :c:`char` array, equals to :c:macro:`CC_SIZE_CHAR_8`.

.. c:macro:: CC_SIZE_CHAR_16

    Size of an 16 :c:`char` array, equals to ``16``.

.. c:macro:: CC_MAX_LEN_CHAR_16

    Maximum length of an 16 :c:`char` array, equals to :c:macro:`CC_SIZE_CHAR_16`.

.. c:macro:: CC_SIZE_CHAR_32

    Size of an 32 :c:`char` array, equals to ``32``.

.. c:macro:: CC_MAX_LEN_CHAR_32

    Maximum length of an 32 :c:`char` array, equals to :c:macro:`CC_SIZE_CHAR_32`.

.. c:macro:: CC_SIZE_CHAR_64

    Size of an 64 :c:`char` array, equals to ``64``.

.. c:macro:: CC_MAX_LEN_CHAR_64

    Maximum length of an 64 :c:`char` array, equals to :c:macro:`CC_SIZE_CHAR_64`.

.. c:macro:: CC_SIZE_CHAR_128

    Size of an 128 :c:`char` array, equals to ``128``.

.. c:macro:: CC_MAX_LEN_CHAR_128

    Maximum length of an 128 :c:`char` array, equals to :c:macro:`CC_SIZE_CHAR_128`.

.. c:macro:: CC_SIZE_CHAR_256

    Size of an 256 :c:`char` array, equals to ``256``.

.. c:macro:: CC_MAX_LEN_CHAR_256

    Maximum length of an 256 :c:`char` array, equals to :c:macro:`CC_SIZE_CHAR_256`.

.. c:macro:: CC_SIZE_CHAR_512

    Size of an 512 :c:`char` array, equals to ``512``.

.. c:macro:: CC_MAX_LEN_CHAR_512

    Maximum length of an 512 :c:`char` array, equals to :c:macro:`CC_SIZE_CHAR_512`.

.. _lbl-libcc-ccstrutils-interfaces:

Interfaces
----------

.. c:type:: int (*cc_ctype_fp_ischar_t)( int ch )

    Function interface, i.e. function pointer type; used to interface with all
    ``ctype.h`` filter functions, e.g. :c:func:`islower()`.

    :param ch: A single :c:`char`\acter.
    :returns: Integer, meaning depends on interfaced function.
    :seealso: https://en.cppreference.com/w/c/string/byte

.. _lbl-libcc-ccstrutils-typesarrays:

Types, arrays
-------------

All arrays defined here have all their :c:`char`\s initialized to ``'\0'``.

.. c:type:: char cc_char_8 [ CC_SIZE_CHAR_8 ]

    A :c:`char` array type, size 8.

.. c:macro:: CC_CHAR_8_EMPTY

    An empty char array, size 8.

.. c:type:: char cc_char_16 [ CC_SIZE_CHAR_16 ]

    A :c:`char` array type, size 16.

.. c:macro:: CC_CHAR_16_EMPTY

    An empty char array, size 16.

.. c:type:: char cc_char_32 [ CC_SIZE_CHAR_32 ]

    A :c:`char` array type, size 32.

.. c:macro:: CC_CHAR_32_EMPTY

    An empty char array, size 32.

.. c:type:: char cc_char_64 [ CC_SIZE_CHAR_64 ]

    A :c:`char` array type, size 64.

.. c:macro:: CC_CHAR_64_EMPTY

    An empty char array, size 64.

.. c:type:: char cc_char_128 [ CC_SIZE_CHAR_128 ]

    A :c:`char` array type, size 128.

.. c:macro:: CC_CHAR_128_EMPTY

    An empty char array, size 128.

.. c:type:: char cc_char_256 [ CC_SIZE_CHAR_256 ]

    A :c:`char` array type, size 256.

.. c:macro:: CC_CHAR_256_EMPTY

    An empty char array, size 256.

.. c:type:: char cc_char_512 [ CC_SIZE_CHAR_512 ]

    A :c:`char` array type, size 512.

.. c:macro:: CC_CHAR_512_EMPTY

    An empty char array, size 512.

.. _lbl-libcc-ccstrutils-functions:

Functions
---------

.. c:function:: bool cc_str_clear( char * str__io, size_t size__d )

    Function to clear string, or char array, by writing ``'\0'`` into every char.

    .. note::

        If *optional* size is not supplied (i.e. is :c:macro:`CC_SIZE_IGNORE`),
        given string must be null-terminated.

    :param str__io: *Input/output*, string to overwrite with zeros.
    :param size__d: *Optional*, maximum size to overwrite.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: char * cc_str_pad__new( char pad, size_t count )

    Function returns newly allocated, null-terminated string, containing :c:`pad`
    character repeated :c:`count` times.

    Allocated string is greater than :c:`count` by ``1``, for null-terminating
    :c:`char`, i.e. ``'\0'``.

    :param pad: A pad character.
    :param count: Count of padding characters in a returned string.
    :returns: Valid pointer to string if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_str_is_empty( char const * str, bool ignore_spaces )

    Function checks if string is empty, i.e. does not contain printable :c:`char`\s.

    :param str: A string to check.
    :param ignore_spaces: Flag, whether to disregard spaces, or not.
    :returns: :c:data:`true` if empty, :c:data:`false` otherwise.

.. c:function:: bool cc_str_count_chars( char const * str, cc_ctype_fp_ischar_t fp_is_char, size_t max_len__d, size_t * count__o )

    Function counts characters in a string, based on a given filtering function.

    :param str: A string.
    :param fp_is_char: Function pointer, used to filter characters.
    :param max_len__d: *Optional*, maximum length to count over.
    :param count__o: *Output*, result.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: char const * cc_str_contains_char( char c, bool case_sensitive, char const * start, char const * end__d, size_t max_len__d )

    Function returns a pointer to character found in a (sub-)string.

    If both optional arguments (:c:`end__d`, :c:`max_len__d`) are given, together
    they limit search to first condition met (end of (sub-)string, or maximum length,
    respectively).

    .. note::

        If no optional arguments (:c:`end__d`, :c:`max_len__d`) were given, string
        to search (:c:`start`) has to be null-terminated.

    :param c: Character to be found.
    :param case_sensitive: Flag, whether search is case-sensitive, or not.
    :param start: Pointer to a start of a (sub-)string.
    :param end__d: *Optional*, pointer to an end of a (sub-)string; can be :c:data:`NULL`.
    :param max_len__d: *Optional*, maximum length of a string to check; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: Pointer to a character if successful, :c:data:`NULL` otherwise.

.. c:function:: char const * cc_str_traverse_chars( char const * str, cc_ctype_fp_ischar_t fp_is_char, bool skip_or_stop_at, size_t max_len__d )

    Function traverses given string, by either skipping over filtered characters,
    or stopping at first of those encountered.

    If there are no filtered characters in a string, function returns pointer to:

        - the terminating character (i.e. ``'\0'``) of a given string,
        - :c:`char` at maximum length, if :c:`max_len__d` was given

    whichever comes first.

    :param str: A string.
    :param fp_is_char: A function pointer, used to filter characters.
    :param skip_or_stop_at: A flag, whether to skip (if :c:data:`true`) or stop at (if :c:data:`false`) filtered character.
    :param max_len__d: *Optional*, maximum length to traverse; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A string pointer within a given string if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_str_to_case( char * str__io, bool to_upper_or_lower, size_t max_len__d )

    Function converting a string in-place, to uppercase or lowercase.

    :param str__io: *Input/output*, string to convert.
    :param to_upper_or_lower: Flag, convert to uppercase (:c:data:`true`), or lowercase (:c:data:`false`) string.
    :param max_len__d: *Optional*, maximum length to convert; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.

.. c:function:: char * cc_str_to_case__new( char const * str, bool to_upper_or_lower, size_t max_len__d )

    Function returns a newly allocated string, converted to uppercase or lowercase.

    :param str: String to convert.
    :param to_upper_or_lower: Flag, convert to uppercase (:c:data:`true`), or lowercase (:c:data:`false`).
    :param max_len__d: *Optional*, maximum length to convert; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated, converted string if successful, :c:data:`NULL` otherwise.

.. c:function:: char const * cc_str_end( char const * start, char const * end__d, size_t max_len__d )

    Function returns pointer to the end of a string, optionally capped at maximum
    length and/or delimited by optional (sub-)string end.

    If both optional arguments (:c:`end__d`, :c:`max_len__d`) are given, together
    they limit traversing to first condition met (end of (sub-)string, or maximum
    length, respectively).

    .. note::

        If no optional arguments (:c:`end__d`, :c:`max_len__d`) were given, string
        to traverse (:c:`start`) has to be null-terminated.

    :param start: Pointer to a start of a (sub-)string.
    :param end__d: *Optional*, pointer to an end of a (sub-)string; can be :c:data:`NULL`.
    :param max_len__d: *Optional*, maximum length of a string to traverse; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: Pointer to the end of a string if successful, :c:data:`NULL` otherwise.

.. c:function:: size_t cc_str_len( char const * start, char const * end__d, size_t max_len__d )

    Function returning length of a string, optionally capped at maximum length
    and/or delimited by optional (sub-)string end.

    If both optional arguments (:c:`end__d`, :c:`max_len__d`) are given, together
    they limit traversing to first condition met (end of (sub-)string, or maximum
    length, respectively).

    .. note::

        If no optional arguments (:c:`end__d`, :c:`max_len__d`) were given, string
        to traverse (:c:`start`) has to be null-terminated.

    :param start: Pointer to a start of a (sub-)string.
    :param end__d: *Optional*, pointer to an end of a (sub-)string; can be :c:data:`NULL`.
    :param max_len__d: *Optional*, maximum length of a string to check; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: Length of a string if successful, ``0`` otherwise.

.. c:function:: int cc_str_len_fmt_va( char const * fmt, va_list args )

    Function returns length of a formatted string.

    :param fmt: A string format, as used by :c:func:`printf()` and friends.
    :param args: Variadic list, input for a string format.
    :returns: Length of a formatted string if non-negative, error code
              if negative.
    :returns: Output returned is direct result of a :c:func:`vsnprintf()`
              found in ``<stdio.h>``.
    :seealso: https://en.cppreference.com/w/c/io/vfprintf

.. c:function:: int cc_str_len_format( char const * fmt, ... )

    Function returns length of a formatted string.

    :param fmt: A string format, as used by :c:func:`printf()` and friends.
    :param ...: variadic input for a string format.
    :returns: Length of a formatted string if non-negative, error code
              if negative.
    :returns: Output returned is direct result of a :c:func:`vsnprintf()`
              found in ``<stdio.h>``.
    :seealso: https://en.cppreference.com/w/c/io/vfprintf

.. c:function:: bool cc_str_is_equal( char const * start_1, char const * end_1__d, char const * start_2, char const * end_2__d, size_t max_len__d )

    Function checks if two (sub-)strings are equal, up to a given maximum length.

    .. note::

        Strings to traverse (:c:`start_1`, :c:`start_2`) missing their corresponding
        end pointer (:c:`end_1__d`, :c:`end_2__d`) and their length (:c:`max_len__d`)
        has to be null-terminated.

    :param start_1: A starting character of a first (sub-)string.
    :param end_1__d: *Optional*, end of a first (sub-)string; can be :c:data:`NULL`.
    :param start_2: A starting character of a second (sub-)string.
    :param end_2__d: *Input/output*, end of a second (sub-)string.
    :param max_len__d: *Optional*, maximum length to compare; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: :c:data:`true` if two given (sub-)strings are equal, :c:data:`false` otherwise.
    :seealso: https://en.cppreference.com/w/c/string/byte/strncmp

.. c:function:: size_t cc_str_copy( char const * start, char const * end__d, size_t max_len__d, char * dest__o, char const * dest_end__d, size_t size_dest__d )

    Function copies (sub-)string into a char array, or already allocated string.

    Function will null-terminate copied string, if there is enough space.

    :param start: A (sub-)string to copy.
    :param end__d: *Optional*, pointer to the end of a (sub-)string; can be :c:data:`NULL`.
    :param max_len__d: *Optional*, maximum length to copy; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param dest__o: Pointer to destination.
    :param dest_end__d: *Optional*, pointer to the end of destination; can be :c:data:`NULL`.
    :param size_dest__d: A starting character of a first (sub-)string.
    :returns: Count of characters copied (not including ``'\0'``) if successful, ``0`` otherwise.
    :seealso: https://en.cppreference.com/w/c/string/byte/strncpy

.. c:function:: char * cc_str_copy__new( char const * start, char const * end__d, size_t max_len__d )

    Function copies (sub-)string into a newly allocated string.

    :param start: A (sub-)string to copy.
    :param end__d: *Optional*, pointer to the end of a (sub-)string.
    :param max_len__d: *Optional*, maximum length to copy; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: Pointer to a newly allocated copy of a given string if successful, :c:data:`NULL` otherwise.
    :seealso: https://en.cppreference.com/w/c/string/byte/strncpy

.. c:function:: char * cc_str_fmt_va__new( size_t max_len__d, char const * fmt, va_list args )

    Function returns a newly allocated string containing formatted variadic input,
    optionally capped at given maximum length.

    :param max_len__d: *Optional*, maximum length to overwrite; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt: A string format, as used by :c:func:`printf()` and friends.
    :param args: Variadic input list for a string format.
    :returns: A newly allocated string if successful, :c:data:`NULL` otherwise.
    :returns: Output returned is direct result of a :c:func:`vsnprintf()`
              found in ``<stdio.h>``.
    :seealso: https://en.cppreference.com/w/c/io/vfprintf

.. c:function:: char * cc_str_fmt__new( size_t max_len__d, char const * fmt, ... )

    Function returns a newly allocated string containing formatted variadic input,
    optionally capped at given maximum length.

    :param max_len__d: *Optional*, maximum length to overwrite; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt: A string format, as used by :c:func:`printf()` and friends.
    :param ...: Variadic input for a string format.
    :returns: A newly allocated string if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_str_fmt_va__new()`

.. c:function:: char * cc_str_duplicate__new( char const * str, bool do_reverse, size_t max_len__d )

    Function returns a newly allocated duplicate of a given string,
    optionally capped at given maximum length.

    :param str: A string to duplicate.
    :param do_reverse: Flag, whether returned string should be reversed.
    :param max_len__d: *Optional*, maximum length to duplicate; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated, duplicated string if successful, :c:data:`NULL` otherwise.

.. c:function:: char * cc_str_append_into( char * start_str__io, char const * end_str__d, size_t size_dest__d, char const * start_sub_str, char const * end_sub_str__d, size_t max_len__d )

    Appends second string into a buffer, in-place.

    Function skips all :c:`char`\s found in a buffer until null-terminating :c:`char`
    (``'\0'``) is reached, before it starts appending substring :c:var:`start_sub_str`.

    .. warning::

        Destination buffer :c:var:`start_str__io` must always be null-terminated (``'\0'``),
        so that function can determine from where to start appending given string
        :c:var:`start_sub_str`.

    Either *optional* :c:var:`end_str__d` or :c:var:`size_dest__d` (or both) can be given;
    if so they limit how much of a given buffer :c:var:`start_str__io` will be used,
    whichever is encountered first.

    .. note::

        At least one of *optional* parameters :c:var:`end_str__d` and :c:var:`size_dest__d`
        has to be given, if both are not (they are :c:`NULL` and :c:macro:`CC_SIZE_IGNORE`,
        respectively) operation is aborted, and :c:`NULL` is returned.

    Either *optional* :c:var:`end_sub_str__d` or :c:var:`max_len__d` (or both) can be given;
    if so they limit how much of a given substring :c:var:`start_sub_str` will be copied,
    whichever is encountered first.

    .. warning::

        If both *optional* parameters are not given (:c:var:`end_sub_str__d` is :c:`NULL`,
        and :c:var:`max_len__d` is :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`), substring
        :c:`start_sub_str` must be null-terminated (``'\0'``), and is appended in its entirety.

    Destination :c:`start_str__io` after appending string is always null-terminated (``'\0'``).
    Function returns weak pointer to that null-terminating :c:`char`.

    Walking pointer over destination buffer can be used in succession, or in a
    loop. This is so because function seeks null-terminating :c:`char` in
    a given destination :c:`start_str__io`, and returns a weak pointer to the new
    null-terminating :c:`char` after appending in that same destination
    buffer.

    For instance:

    .. code-block:: C
        :force:

        // Must always contain null-terminated string,
        // initialize to '\0' if it doesn't (e.g. just allocated).
        buffer__a[ 0 ] = '\0';

        char * walking = buffer__a;

        do {
            walking = cc_str_append_into( walking, ... );
        } while ( walking );

    :param start_str__io: *Input/output*, a string into which to append.
    :param end_str__d: *Optional*, *input/output*, an end pointer to a string into which to append.
    :param size_dest__d: *Optional*, size of a destination; can be :c:macro:`CC_SIZE_IGNORE`.
    :param start_sub_str: A string to append.
    :param end_sub_str__d: *Optional*, an end pointer of a string to append.
    :param max_len__d: *Optional*, maximum length of resulting string; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A weak pointer to null-terminating char inside a given buffer :c:var:`start_str__io`
        if successful, :c:data:`NULL` otherwise.

.. c:function:: char * cc_str_append__new( char const * str_1__d, char const * str_2__d, size_t max_len__d )

    Function appends strings into newly allocated string, optionally
    capped at a given maximum length.

    :param str_1__d: *Optional* string to copy first, can be :c:data:`NULL`.
    :param str_2__d: *Optional* string to concatenate, can be :c:data:`NULL`.
    :param max_len__d: *Optional*, maximum length to duplicate; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated, appended string if successful, :c:data:`NULL` otherwise.

.. c:function:: char * cc_str_append_free__new( char ** str_1__d_f, char ** str_2__d_f, size_t max_len__d )

    Function appends strings into newly allocated, null-terminated string,
    optionally capped at a given maximum length.

    Given string are :c:func:`free()`\ed, and their inner pointer set to
    :c:data:`NULL` only if valid result is produced.

    :param str_1__d_f: *Optional*, a string to append to. It is :c:func:`free()`\ed, if given.
    :param str_2__d_f: *Optional*, a string to append. It is :c:func:`free()`\ed, if given.
    :param max_len__d: *Optional*, maximum length to duplicate; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated, appended string if successful, :c:data:`NULL` otherwise.

.. c:function:: char * cc_str_append_fmt_va__new( char ** str__d_f, size_t max_len__d, char const * fmt, va_list args )

    Function appends formatted variadic input to a given string, into
    newly allocated, null-terminated string, optionally capped at given
    maximum length.

    :param str__d_f: *Optional*, a string to append to. It is :c:func:`free()`\ed, if given.
    :param max_len__d: *Optional*, maximum length of resulting string; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt: A string format, as used by :c:func:`printf()` and friends.
    :param args: Variadic input list for a string format.
    :returns: A newly allocated string if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_str_append_free__new()`

.. c:function:: char * cc_str_append_fmt__new( char ** str__d_f, size_t max_len__d, char const * fmt, ... )

    Function appends formatted variadic input to a given string, into
    newly allocated, null-terminated string, optionally capped at given
    maximum length.

    :param str__d_f: *Optional*, a string to append to. It is :c:func:`free()`\ed, if given.
    :param max_len__d: *Optional*, maximum length of resulting string; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt: A string format, as used by :c:func:`printf()` and friends.
    :param ...: Variadic input for a string format.
    :returns: A newly allocated string if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_str_append_fmt_va__new()`

.. _lbl-libcc-ccstrutils-debug:

Debug
-----

.. c:function:: bool cc_str_print( char const * start, char const * end__d, size_t max_len__d, char const * fmt_str, size_t fmt_len__d, char const * fmt__d, ... )

    .. todo::

        Move out of library / return newly allocated, formatted string.
        Remove library dependency on ``<stdio.h>``.

    Function prints given (sub-)string, optionally followed by formatted variadic input.

    Compile-time constant under which this function is defined is :c:macro:`__CC_DEBUG__`.

    .. note::

        Format string :c:`fmt_str` has to have exactly one :c:`"%s"` string
        specifier, which is used to print given string :c:`start`.

        For instance:

        .. code-block:: C
            :force:

            cc_str_printf( , , , "No help entry: '%s'.\n", , );

    :param start: A string to print first.
    :param end__d: *Optional*, pointer to the end of a (sub-)string.
    :param max_len__d: *Optional*, maximum length of printed string :c:`start`; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt_str: A :c:func:`printf()`\-format string, used to format :c:`start` string.
    :param fmt_len__d: *Optional*, maximum length of variadic string to print; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt__d: *Optional*, a string format, as used by :c:func:`printf()` and friends.
    :param ...: Variadic input for a string format.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_str_fmt_va__new()`

.. c:macro:: CC_STR_PRINT(start,end__d,max_len__d,fmt_str,fmt_len__d,fmt__d,...)

    .. todo::

        Move out of library / return newly allocated, formatted string.
        Remove library dependency on ``<stdio.h>``.

    Macro to call :c:func:`cc_str_print()`, depending on a compile-time constant.

    Compile-time constant which controls definition of this macro is :c:macro:`__CC_DEBUG__`.

    :param start: A string to print first.
    :param end__d: *Optional*, pointer to the end of a (sub-)string.
    :param max_len__d: *Optional*, maximum length of printed string :c:`start`; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt_str: A :c:func:`printf()`\-format string, used to format :c:`start` string.
    :param fmt_len__d: *Optional*, maximum length of variadic string to print; can be :c:macro:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt__d: *Optional*, a string format, as used by :c:func:`printf()` and friends.
    :param ...: Variadic input for a string format.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.
    :seealso: :c:func:`cc_str_print()`

.. _lbl-libcc-ccstrutils-sourcecodeheader:

Header file
-----------

Included source header file is ``cc_str_utils.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_str_utils.h
    :language: C
    :linenos:

.. _lbl-libcc-ccstrutils-sourcecodefile:

Source code file
----------------

Included source code file is ``cc_str_utils.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_str_utils.c
    :language: C
    :linenos:
