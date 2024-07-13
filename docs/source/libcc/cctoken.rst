.. Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
   This text is Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See accompanying LICENSING, COPYING files for details.
   Included source code files are licensed under GNU GPL v3+ license. See LICENSING, COPYING files in root folder of the project for details.

.. include:: ../defines.rst

.. _lbl-libcc-cctoken:

Token
=====

Documents ``cc_token.h`` and ``cc_token.c`` files, which contain
simple tokenizer, constants, and related functions.

Tokenizer is used by console applications, not in library.

.. _lbl-libcc-cctoken-constants:

Token constants
---------------

.. c:type:: char const CC_TOKEN_SEPARATORS_WHITESPACE[]

    Token whitespace constants, equals to ``" \t\v\f\r\n"``.

.. c:type:: char const CC_TOKEN_SEPARATORS_PUNCTUATION[]

    Token punctuation constants, equals to ``"!\"#$%%&'()*+,-./"``.

.. _lbl-libcc-cctoken-functions:

Token functions
---------------

.. c:function:: bool cc_char_in( char c, char const * seps )

    Function checks if character is in a given string.

    :param c: Character to check.
    :param seps: String of characters.
    :returns: :c:data:`true` if character is found in a given string,
              :c:data:`false` otherwise.

.. c:function:: char const * cc_traverse_chars( char const * pos, char const * seps, bool skip_or_stop_at )

    Function traversing over string, returning next position within it.

    :param pos: String to traverse.
    :param seps: Separators to check.
    :param skip_or_stop_at: Whether to skip separators (if :c:data:`true`), or stop at them (if :c:data:`false`).
    :returns: Next position within :c:`pos` string if successful, :c:data:`NULL` otherwise.

.. c:function:: char const * cc_skip_chars( char const * pos, char const * seps )

    Function traversing over string, skipping separators, returning next position within string.

    :param pos: String to traverse.
    :param seps: Separators to skip.
    :returns: Next position within :c:`pos` string if successful, :c:data:`NULL` otherwise.

.. c:function:: char const * cc_stop_at_chars( char const * pos, char const * seps )

    Function traversing over string, stopping at separators, returning next position within string.

    :param pos: String to traverse.
    :param seps: Separators to stop at.
    :returns: Next position within :c:`pos` string if successful, :c:data:`NULL` otherwise.

.. c:function:: bool cc_iter_token( char const * str, char const * seps, char const ** start__io, char const ** end__io )

    Iterator traversing over string, returning next token as a pair of pointers.

    Both *input* / *output* arguments :c:`start__io` and :c:`end__io` has to be
    valid pointer variables, not expressions.

    Both inner pointers has to be :c:data:`NULL` for the first call (i.e.
    :c:expr:`*start__io == NULL`, :c:expr:`*end__io == NULL`) .

    At subsequent calls, both inner pointers has to be valid pointers.
    It is error if one inner pointer is valid, and the other :c:data:`NULL`.

    Iterator will continue to return next token on each subsequent call, until
    end of a string is reached, or both inner pointers are reinitialized to :c:data:`NULL`.

    Upon reaching the end of a given string, both inner pointers (:c:`*start__io`
    and :c:`*end__io`) are reset to :c:data:`NULL`.

    So, if nothing changes, next calls (or, next loop) will again start from the
    beginning of a given string :c:`str`.

    :param str: String to traverse.
    :param seps: Separators between tokens.
    :param start__io: *Input/output*, first :c:`char` of found token.
    :param end__io: *Input/output*, end of a token, i.e. first :c:`char` which does not belong to found token.
    :returns: :c:data:`true` if next token was found, :c:data:`false` otherwise.

.. c:function:: char * cc_trim_str__new( char const * str, char const * chars )

    Function returning newly allocated string, with :c:`char`\s trimmed from the
    beginning and the end.

    :param str: String to trim, on both ends.
    :param chars: Characters to trim from string.
    :returns: Newly allocated, trimmed string if successful, :c:data:`NULL` otherwise.

.. _lbl-libcc-cctoken-sourcecodeheader:

Token source code header
------------------------

Included source code file is ``cc_token.h``.

.. literalinclude:: ../../../ws/libcrochess/inc/cc_token.h
    :language: C
    :linenos:

.. _lbl-libcc-cctoken-sourcecodefile:

Token source code file
----------------------

Included source code file is ``cc_token.c``.

.. literalinclude:: ../../../ws/libcrochess/src/cc_token.c
    :language: C
    :linenos:
