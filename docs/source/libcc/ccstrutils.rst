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

    :param str__io: A string to overwrite with zeros.
    :param max_len__d: Maximum length to overwrite.
    :returns: :c:`true` if successful, :c:`false` otherwise.

.. c:function:: bool cc_str_is_empty( char const * str, bool ignore_spaces )

    Function checks if string is empty, i.e. does not contain printable :c:expr:`char`\s.

    :param str: A string to check.
    :param ignore_spaces: Flag, whether to disregard spaces, or not.
    :returns: :c:`true` if empty, :c:`false` otherwise.
