.. Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
   Public Domain work, under CC0 1.0 Universal Public Domain Dedication. See LICENSING, COPYING files for details.

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

    Token whitespace constants, equals to :c:`" \t\v\f\r\n"`.

.. c:type:: char const CC_TOKEN_SEPARATORS_PUNCTUATION[]

    Token punctuation constants, equals to :c:`"!\"#$%%&'()*+,-./"`.

.. _lbl-libcc-cctoken-functions:

Token functions
---------------

.. c:function:: bool cc_char_in( char c, char const * seps )

    Function checks if character is in a given string.

    :param c: Character to check.
    :param seps: String of characters.
    :returns: :c:`true` if character is found in a given string,
              :c:`false` otherwise.

.. c:function:: char const * cc_traverse_chars( char const * pos, char const * seps, bool skip_or_stop_at )

    Function traversing over string, returning next position within it.

    :param pos: String to traverse.
    :param seps: Separators to check.
    :param skip_or_stop_at: Whether to skip separators (if :c:`true`), or stop at them (if :c:`false`).
    :returns: Next position within :c:`pos` string if successful, :c:`NULL` otherwise.

.. c:function:: char const * cc_skip_chars( char const * pos, char const * seps )

    Function traversing over string, skipping separators, returning next position within string.

    :param pos: String to traverse.
    :param seps: Separators to skip.
    :returns: Next position within :c:`pos` string if successful, :c:`NULL` otherwise.

.. c:function:: char const * cc_stop_at_chars( char const * pos, char const * seps )

    Function traversing over string, stopping at separators, returning next position within string.

    :param pos: String to traverse.
    :param seps: Separators to stop at.
    :returns: Next position within :c:`pos` string if successful, :c:`NULL` otherwise.

.. c:function:: bool cc_iter_token( char const * str, char const * seps, char const ** start__io, char const ** end__io )

    Iterator traversing over string, returning next token as a pair of pointers.

    Both *input* / *output* arguments :c:`start__io` and :c:`end__io` has to be
    valid pointer variables, not expressions.

    Both inner pointers has to be :c:`NULL` for the first call (i.e.
    :c:expr:`*start__io == NULL`, :c:expr:`*end__io == NULL`) .

    At subsequent calls, both inner pointers has to be valid pointers.
    It is error if one inner pointer is valid, and the other :c:`NULL`.

    Iterator will continue to return next token on each subsequent call, until
    end of a string is reached, or both inner pointers are reinitialized to :c:`NULL`.

    Upon reaching the end of a given string, both inner pointers (:c:`*start__io`
    and :c:`*end__io`) are reset to :c:`NULL`.

    So, if nothing changes, next calls (or, next loop) will again start from the
    beginning of a given string :c:`str`.

    :param str: String to traverse.
    :param seps: Separators between tokens.
    :param start__io: *Input/output*, first :c:`char` of found token.
    :param end__io: *Input/output*, end of a token, i.e. first :c:`char` which does not belong to found token.
    :returns: :c:`true` if next token was found, :c:`false` otherwise.

.. c:function:: char * cc_trim_str__new( char const * str, char const * chars )

    Function returning newly allocated string, with :c:`char`\s trimmed from the
    beginning and the end.

    :param str: String to trim, on both ends.
    :param chars: Characters to trim from string.
    :returns: Newly allocated, trimmed string if successful, :c:`NULL` otherwise.
