.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccstrutils:

String utilities
================

Documents ``cc_str_utils.h`` and ``cc_str_utils.c`` files, which contain
strings, :c:expr:`char` arrays utility functions.

All functions which return newly allocated string, return them
zero-terminated (:c:`'\0'`).

Length of a string counts only content, without zero-terminating character.
This is the same as :c:`strlen()`, see
`<https://en.cppreference.com/w/c/string/byte/strlen>`_.

Size of a string **includes** terminating character.

Length of :c:expr:`char` arrays is the same as its size, regardless of length
of its content, if it's zero-terminated, or not.

When content in :c:expr:`char` array is shorter than the array itself, it is
zero-terminated.

Utility functions have string and its maximum length parameters in pairs.

When calling such functions, :c:expr:`CC_MAX_LEN_ZERO_TERMINATED` can be
used as an argument to maximum length parameter; if so, there is no limit
on string length, and string itself **must** be zero-terminated (:c:`'\0'`).

For :c:expr:`char` arrays always do use appropriate maximum length argument,
e.g. :c:expr:`CC_MAX_LEN_CHAR_8`.

.. _lbl-libcc-ccstrutils-sizeslengths:

String sizes, lengths
---------------------

.. c:macro:: CC_MAX_LEN_ZERO_TERMINATED

    Constant to ignore maximum length constraint in functions, equals to :c:`0`.

.. c:macro:: CC_SIZE_IGNORE

    Invalid size, to flag size argument to be ignored, equals to :c:`0`.

.. c:macro:: CC_SIZE_CHAR_8

    Size of an 8 :c:expr:`char` array, equals to :c:`8`.

.. c:macro:: CC_MAX_LEN_CHAR_8

    Maximum length of an 8 :c:expr:`char` array, equals to :c:expr:`CC_SIZE_CHAR_8`.

.. c:macro:: CC_SIZE_CHAR_16

    Size of an 16 :c:expr:`char` array, equals to :c:`16`.

.. c:macro:: CC_MAX_LEN_CHAR_16

    Maximum length of an 16 :c:expr:`char` array, equals to :c:expr:`CC_SIZE_CHAR_16`.

.. c:macro:: CC_SIZE_CHAR_32

    Size of an 32 :c:expr:`char` array, equals to :c:`32`.

.. c:macro:: CC_MAX_LEN_CHAR_32

    Maximum length of an 32 :c:expr:`char` array, equals to :c:expr:`CC_SIZE_CHAR_32`.

.. c:macro:: CC_SIZE_CHAR_64

    Size of an 64 :c:expr:`char` array, equals to :c:`64`.

.. c:macro:: CC_MAX_LEN_CHAR_64

    Maximum length of an 64 :c:expr:`char` array, equals to :c:expr:`CC_SIZE_CHAR_64`.

.. c:macro:: CC_SIZE_CHAR_128

    Size of an 128 :c:expr:`char` array, equals to :c:`128`.

.. c:macro:: CC_MAX_LEN_CHAR_128

    Maximum length of an 128 :c:expr:`char` array, equals to :c:expr:`CC_SIZE_CHAR_128`.

.. c:macro:: CC_SIZE_CHAR_256

    Size of an 256 :c:expr:`char` array, equals to :c:`256`.

.. c:macro:: CC_MAX_LEN_CHAR_256

    Maximum length of an 256 :c:expr:`char` array, equals to :c:expr:`CC_SIZE_CHAR_256`.

.. c:macro:: CC_SIZE_CHAR_512

    Size of an 512 :c:expr:`char` array, equals to :c:`512`.

.. c:macro:: CC_MAX_LEN_CHAR_512

    Maximum length of an 512 :c:expr:`char` array, equals to :c:expr:`CC_SIZE_CHAR_512`.

.. _lbl-libcc-ccstrutils-interfaces:

String utility interfaces
-------------------------

.. c:type:: int (*cc_ctype_fp_ischar_t)( int ch )

    Function interface, i.e. function pointer type; used to interface with all
    ``ctype.h`` filter functions, e.g. :c:`islower()`.

    :param ch: A single :c:expr:`char`\acter.
    :returns: Integer, meaning depends on interfaced function.
    :seealso: https://en.cppreference.com/w/c/string/byte

.. _lbl-libcc-ccstrutils-typesarrays:

String utility types, arrays
----------------------------

All arrays defined here have all their :c:expr:`char`\s initialized to :c:`'\0'`.

.. c:type:: char cc_char_8 [ CC_SIZE_CHAR_8 ]

    A :c:expr:`char` array type, size 8.

.. c:macro:: CC_CHAR_8_EMPTY

    An empty char array, size 8.

.. c:type:: char cc_char_16 [ CC_SIZE_CHAR_16 ]

    A :c:expr:`char` array type, size 16.

.. c:macro:: CC_CHAR_16_EMPTY

    An empty char array, size 16.

.. c:type:: char cc_char_32 [ CC_SIZE_CHAR_32 ]

    A :c:expr:`char` array type, size 32.

.. c:macro:: CC_CHAR_32_EMPTY

    An empty char array, size 32.

.. c:type:: char cc_char_64 [ CC_SIZE_CHAR_64 ]

    A :c:expr:`char` array type, size 64.

.. c:macro:: CC_CHAR_64_EMPTY

    An empty char array, size 64.

.. c:type:: char cc_char_128 [ CC_SIZE_CHAR_128 ]

    A :c:expr:`char` array type, size 128.

.. c:macro:: CC_CHAR_128_EMPTY

    An empty char array, size 128.

.. c:type:: char cc_char_256 [ CC_SIZE_CHAR_256 ]

    A :c:expr:`char` array type, size 256.

.. c:macro:: CC_CHAR_256_EMPTY

    An empty char array, size 256.

.. c:type:: char cc_char_512 [ CC_SIZE_CHAR_512 ]

    A :c:expr:`char` array type, size 512.

.. c:macro:: CC_CHAR_512_EMPTY

    An empty char array, size 512.

.. _lbl-libcc-ccstrutils-functions:

String utility functions
------------------------

.. c:function:: bool cc_str_clear( char * str__io, size_t max_len__d )

    Function to clear string, or char array, by writing :c:`'\0'` into every char.

    :param str__io: *Input/output*, string to overwrite with zeros.
    :param max_len__d: *Optional*, maximum length to overwrite.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. c:function:: bool cc_str_is_empty( char const * str, bool ignore_spaces )

    Function checks if string is empty, i.e. does not contain printable :c:expr:`char`\s.

    :param str: A string to check.
    :param ignore_spaces: Flag, whether to disregard spaces, or not.
    :returns: :c:`true` if empty, :c:`false` otherwise.

.. c:function:: bool cc_str_count_chars( char const * str, cc_ctype_fp_ischar_t fp_is_char, size_t max_len__d, size_t * count__o )

    Function counts characters in a string, based on a given filtering function.

    :param str: A string.
    :param fp_is_char: Function pointer, used to filter characters.
    :param max_len__d: *Optional*, maximum length to count over.
    :param count__o: *Output*, result.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. c:function:: char const * cc_str_contains_char( char c, bool case_sensitive, char const * start, char const * end__d, size_t max_len__d )

    Function returns a pointer to character found in a (sub-)string.

    If both optional arguments (:c:`end__d`, :c:`max_len__d`) are given, together
    they limit search to first condition met (end of (sub-)string, or maximum legth,
    respectively).

    .. note::

        If no optional arguments (:c:`end__d`, :c:`max_len__d`) were given, string
        to search (:c:`start`) has to be zero-terminated.

    :param c: Character to be found.
    :param case_sensitive: Flag, whether search is case-sensitive, or not.
    :param start: Pointer to a start of a (sub-)string.
    :param end__d: *Optional*, pointer to an end of a (sub-)string; can be :c:`NULL`.
    :param max_len__d: *Optional*, maximum length of a string to check; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: Pointer to a character if successful, :c:`NULL` otherwise.

.. c:function:: char const * cc_str_traverse_chars( char const * str, cc_ctype_fp_ischar_t fp_is_char, bool skip_or_stop_at, size_t max_len__d )

    Function traverses given string, by either skipping over filtered characters,
    or stopping at first of those encountered.

    If there are no filtered characters in a string, function returns pointer to:

        - the terminating character (i.e. :c:`'\0'`) of a given string,
        - :c:expr:`char` at maximum length, if :c:`max_len__d` was given

    whichever comes first.

    :param str: A string.
    :param fp_is_char: A function pointer, used to filter characters.
    :param skip_or_stop_at: A flag, whether to skip (if :c:`true`) or stop at (if :c:`false`) filtered character.
    :param max_len__d: *Optional*, maximum length to traverse; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A string pointer within a given string if successful, :c:`NULL` otherwise.

.. c:function:: bool cc_str_to_case( char * str__io, bool to_upper_or_lower, size_t max_len__d )

    Function converting a string in-place, to uppercase or lowercase.

    :param str__io: *Input/output*, string to convert.
    :param to_upper_or_lower: Flag, convert to uppercase (:c:`true`), or lowercase (:c:`false`) string.
    :param max_len__d: *Optional*, maximum length to convert; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. c:function:: char * cc_str_to_case__new( char const * str, bool to_upper_or_lower, size_t max_len__d )

    Function returns a newly allocated string, converted to uppercase or lowercase.

    :param str: String to convert.
    :param to_upper_or_lower: Flag, convert to uppercase (:c:`true`), or lowercase (:c:`false`).
    :param max_len__d: *Optional*, maximum length to convert; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated, converted string if successful, :c:`NULL` otherwise.

.. c:function:: char const * cc_str_end( char const * start, char const * end__d, size_t max_len__d )

    Function returns pointer to the end of a string, optionally capped at maximum
    length and/or delimited by optional (sub-)string end.

    If both optional arguments (:c:`end__d`, :c:`max_len__d`) are given, together
    they limit traversing to first condition met (end of (sub-)string, or maximum
    legth, respectively).

    .. note::

        If no optional arguments (:c:`end__d`, :c:`max_len__d`) were given, string
        to traverse (:c:`start`) has to be zero-terminated.

    :param start: Pointer to a start of a (sub-)string.
    :param end__d: *Optional*, pointer to an end of a (sub-)string; can be :c:`NULL`.
    :param max_len__d: *Optional*, maximum length of a string to traverse; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: Pointer to the end of a string if successful, :c:`NULL` otherwise.

.. c:function:: size_t cc_str_len( char const * start, char const * end__d, size_t max_len__d )

    Function returning length of a string, optionally capped at maximum length
    and/or delimited by optional (sub-)string end.

    If both optional arguments (:c:`end__d`, :c:`max_len__d`) are given, together
    they limit traversing to first condition met (end of (sub-)string, or maximum
    legth, respectively).

    .. note::

        If no optional arguments (:c:`end__d`, :c:`max_len__d`) were given, string
        to traverse (:c:`start`) has to be zero-terminated.

    :param start: Pointer to a start of a (sub-)string.
    :param end__d: *Optional*, pointer to an end of a (sub-)string; can be :c:`NULL`.
    :param max_len__d: *Optional*, maximum length of a string to check; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: Length of a string if successful, :c:`0` otherwise.

.. c:function:: int cc_str_len_fmt_va( char const * fmt, va_list args )

    Function returns length of a formatted string.

    :param fmt: A string format, as used by :c:`printf()` and friends.
    :param args: Variadic list, input for a string format.
    :returns: Length of a formatted string if non-negative, error code
              if negative.
    :returns: Output returned is direct result of a :c:`vsnprintf()`
              found in ``<stdio.h>``.
    :seealso: https://en.cppreference.com/w/c/io/vfprintf

.. c:function:: int cc_str_len_format( char const * fmt, ... )

    Function returns length of a formatted string.

    :param fmt: A string format, as used by :c:`printf()` and friends.
    :param ...: variadic input for a string format.
    :returns: Length of a formatted string if non-negative, error code
              if negative.
    :returns: Output returned is direct result of a :c:`vsnprintf()`
              found in ``<stdio.h>``.
    :seealso: https://en.cppreference.com/w/c/io/vfprintf

.. c:function:: bool cc_str_is_equal( char const * start_1, char const * end_1__d, char const * start_2, char const * end_2__d, size_t max_len__d )

    Function checks if two (sub-)strings are equal, up to a given maximum length.

    .. note::

        Strings to traverse (:c:`start_1`, :c:`start_2`) missing their corresponding
        end pointer (:c:`end_1__d`, :c:`end_2__d`) and their length (:c:`max_len__d`)
        has to be zero-terminated.

    :param start_1: A starting character of a first (sub-)string.
    :param end_1__d: *Optional*, end of a first (sub-)string; can be :c:`NULL`.
    :param start_2: A starting character of a second (sub-)string.
    :param end_2__d: *Input/output*, end of a second (sub-)string.
    :param max_len__d: *Optional*, maximum length to compare; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: :c:`true` if two given (sub-)strings are equal, :c:`false` otherwise.
    :seealso: https://en.cppreference.com/w/c/string/byte/strncmp

.. c:function:: size_t cc_str_copy( char const * start, char const * end__d, size_t max_len__d, char * dest__o, char const * dest_end__d, size_t size_dest__d )

    Function copies (sub-)string into a char array, or already allocated string.

    Function will zero-terminate copied string, if there is enough space.

    :param start: A (sub-)string to copy.
    :param end__d: *Optional*, pointer to the end of a (sub-)string; can be :c:`NULL`.
    :param max_len__d: *Optional*, maximum length to copy; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param dest__o: Pointer to destination.
    :param dest_end__d: *Optional*, pointer to the end of destination; can be :c:`NULL`.
    :param size_dest__d: A starting character of a first (sub-)string.
    :returns: Count of characters copied (not including :c:`'\0'`) if successful, :c:`0` otherwise.
    :seealso: https://en.cppreference.com/w/c/string/byte/strncpy

.. c:function:: char * cc_str_copy__new( char const * start, char const * end__d, size_t max_len__d )

    Function copies (sub-)string into a newly allocated string.

    :param start: A (sub-)string to copy.
    :param end__d: *Optional*, pointer to the end of a (sub-)string.
    :param max_len__d: *Optional*, maximum length to copy; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: Pointer to a newly allocated copy of a given string if successful, :c:`NULL` otherwise.
    :seealso: https://en.cppreference.com/w/c/string/byte/strncpy

.. c:function:: char * cc_str_fmt_va__new( size_t max_len__d, char const * fmt, va_list args )

    Function returns a newly allocated string containing formatted variadic input,
    optionally capped at given maximum length.

    :param max_len__d: *Optional*, maximum length to overwrite; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt: A string format, as used by :c:`printf()` and friends.
    :param args: Variadic input list for a string format.
    :returns: A newly allocated string if successful, :c:`NULL` otherwise.
    :returns: Output returned is direct result of a :c:`vsnprintf()`
              found in ``<stdio.h>``.
    :seealso: https://en.cppreference.com/w/c/io/vfprintf

.. c:function:: char * cc_str_fmt__new( size_t max_len__d, char const * fmt, ... )

    Function returns a newly allocated string containing formatted variadic input,
    optionally capped at given maximum length.

    :param max_len__d: *Optional*, maximum length to overwrite; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :param fmt: A string format, as used by :c:`printf()` and friends.
    :param ...: Variadic input for a string format.
    :returns: A newly allocated string if successful, :c:`NULL` otherwise.
    :seealso: :c:expr:`cc_str_fmt_va__new()`

.. c:function:: char * cc_str_duplicate__new( char const * str, bool do_reverse, size_t max_len__d )

    Function returns a newly allocated duplicate of a given string,
    optionally capped at given maximum length.

    :param str: A string to duplicate.
    :param do_reverse: Flag, whether returned string should be reversed.
    :param max_len__d: *Optional*, maximum length to duplicate; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated, duplicated string if successful, :c:`NULL` otherwise.

.. c:function:: char * cc_str_append_into( char * str__io, size_t size_dest__d, char const * str, size_t max_len__d )

    Function appends second string into a first one, in-place.

    .. warning::

        Destination buffer :c:`str__io` must always be zero-terminated (:c:`'\0'`),
        so that function can determine from where to start appending given string
        :c:`str`.

    .. warning::

        Rest of a destination buffer :c:`str__io`, behind that zero-terminating
        :c:expr:`char` found earlier, must be large enough to store appending
        string :c:`str`; taking into account :c:`size_dest__d` and :c:`max_len__d`,
        if given.

    *Optional* :c:`max_len__d` can be  :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`,
    if so string :c:`str` must be zero-terminated, and is appended in its entirety.

    Destination :c:`str__io` after appending string is always zero-terminated.
    Function returns weak pointer to that zero-terminating :c:expr:`char`.

    Walking pointer over destination buffer can be used in succession, or in a
    loop. This is so because function seeks zero-terminating :c:expr:`char` in
    a given destination :c:`str__io`, and returns a weak pointer to the new
    zero-terminating :c:expr:`char` after appending in that same destination
    buffer.

    For instance:

    .. code-block:: C
        :force:

        // Must always contain zero-terminated string,
        // initialize to '\0' if it doesn't (e.g. just allocated).
        buffer__a[ 0 ] = '\0';

        char * walking = buffer__a;

        while ( iter( ... ) ) {
            walking = cc_str_append_into( walking, ... );
            if ( !walking ) break;
        }

    :param str__io: *Input/output*, a string into which to append.
    :param size_dest__d: *Optional*, size of a destination; can be :c:expr:`CC_SIZE_IGNORE`.
    :param str: A string to append.
    :param max_len__d: *Optional*, maximum length to append; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A weak pointer to zero-terminating char if successful, :c:`NULL` otherwise.

.. c:function:: char * cc_str_append__new( char const * str_1__d, char const * str_2__d, size_t max_len__d )

    Function appends strings into newly allocated string, optionally
    capped at a given maximum length.

    :param str_1__d: *Optional* string to copy first, can be :c:`NULL`.
    :param str_2__d: *Optional* string to concatenate, can be :c:`NULL`.
    :param max_len__d: *Optional*, maximum length to duplicate; can be :c:expr:`CC_MAX_LEN_ZERO_TERMINATED`.
    :returns: A newly allocated, appended string if successful, :c:`NULL` otherwise.
