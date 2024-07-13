.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-ccstrings:

Strings
=======

Documents ``cc_strings.h`` and ``cc_strings.c`` files, which contain
strings linked list definitions and functions.

.. _lbl-libcc-ccstrings-types:

String types
------------

.. c:struct:: CcStrings

    String :c:`struct`, linked list.

    .. c:member:: char * str

        Pointer to a string.

    .. c:member:: struct CcStrings * next

        Next string, in a linked list.

    :c:`Struct` is tagged with the same :c:enum:`CcStrings` name.

.. _lbl-libcc-ccstrings-functions:

String functions
----------------

.. c:function:: CcStrings * cc_strings__new( char const * str, size_t max_len__d )

    Returns a newly allocated string link, i.e. one element in a linked list.

    :param str: A string to copy, and initialize allocated link.
    :param max_len__d: *Optional*, maximum length to copy.
    :returns: A newly allocated string link if successful, :c:data:`NULL` otherwise.

.. c:function:: CcStrings * cc_strings_append( CcStrings ** strings__iod_a, char const * str, size_t max_len__d )

    Appends a newly allocated string element to a linked list.

    If linked list :c:`*strings__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated string link as its only element.

    :param strings__iod_a: **Ownership**, *optional* *input/ouptut*; linked list of strings.
    :param str: A string to copy, and initialize allocated link.
    :param max_len__d: *Optional*, maximum length to copy.
    :returns: Weak pointer to a newly allocated string if successful, :c:data:`NULL` otherwise.

.. c:function:: CcStrings * cc_strings_append_fmt_va( CcStrings ** strings__iod_a, size_t max_len__d, char const * fmt, va_list args )

    Appends a newly allocated string element to a linked list.

    If linked list :c:`*strings__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated string link as its only element.

    :param strings__iod_a: **Ownership**, *optional* *input/ouptut*; linked list of strings.
    :param max_len__d: *Optional*, maximum length to copy.
    :param fmt: Formatting string, as defined for :c:func:`printf()`.
    :param args: Variadic input list for a string format.
    :returns: Weak pointer to a newly allocated string if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_strings_append()`

.. c:function:: CcStrings * cc_strings_append_fmt( CcStrings ** strings__iod_a, size_t max_len__d, char const * fmt, ... )

    Appends a newly allocated string element to a linked list.

    If linked list :c:`*strings__iod_a` is :c:data:`NULL`, it will be initialized
    with a newly allocated string link as its only element.

    :param strings__iod_a: **Ownership**, *optional* *input/ouptut*; linked list of strings.
    :param max_len__d: *Optional*, maximum length to copy.
    :param fmt: Formatting string, as defined for :c:func:`printf()`.
    :param ...: Variadic input for a string format.
    :returns: Weak pointer to a newly allocated string if successful, :c:data:`NULL` otherwise.
    :seealso: :c:func:`cc_strings_append_fmt_va()`

.. c:function:: CcStrings * cc_strings_duplicate_all__new( CcStrings * strings )

    Duplicates all string elements in a given linked list.

    :param strings: Linked list of strings.
    :returns: Pointer to a newly allocated linked list if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_strings_free_all( CcStrings ** strings__f )

    :c:func:`free()`\s all string links in a linked list.

    :param strings__f: Linked list of strings.
    :returns: :c:data:`true` if successful, :c:data:`false` otherwise.
    :seealso: https://en.cppreference.com/w/c/memory/free

.. _lbl-libcc-ccstrings-sourcecodeheader:

Strings source code header
--------------------------

Included source code file is ``cc_strings.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_strings.h
    :language: C
    :linenos:

.. _lbl-libcc-ccstrings-sourcecodefile:

Strings source code file
------------------------

Included source code file is ``cc_strings.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_strings.c
    :language: C
    :linenos:
