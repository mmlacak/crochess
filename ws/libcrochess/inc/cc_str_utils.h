// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STR_UTILS_H__
#define __CC_STR_UTILS_H__

#include <stdbool.h>
#include <stdlib.h>

/**
    @file cc_str_utils.h
    @brief Strings, char arrays utility functions.

    All allocated strings are assumed to be zero-terminated (``'\0'``).

    For zero-terminated strings argument `CC_MAX_LEN_ZERO_TERMINATED` can be
    used for max len parameters, if so entirety of a string will be used
    (copied, cleared, ...).

    Char arrays are zero-terminated if data is shorter than array, but
    generally are not; so an appropriate max len argument must be used,
    e.g. `CC_MAX_LEN_STR_8`.

    All functions which return allocated string, return them zero-terminated.

    Length of an allocated string is as returned by `strlen()`,
    i.e. without zero-terminating character.

    Size of an allocated string includes terminating character.

    Length of a char arrays is the same as its size,
    and does not include zero-terminating char (``'\0'``).

    Length of data in a char array can be smaller than length of array,
    if so data is zero-terminated.
*/


/**
    Value to ignore maximum length constraint on various functions.
*/
#define CC_MAX_LEN_ZERO_TERMINATED (0)

/**
    Size of an 8 char array.
*/
#define CC_STR_8_SIZE (8)

/**
    Maximum length of an 8 char array.
*/
#define CC_MAX_LEN_STR_8 (8)


/**
    Function interface, i.e. function pointer type;
    used to interface with all `ctype.h` filter functions, e.g. `islower()`.

    @param ch A single character.

    @return Integer, meaning depends on interfaced function.
*/
typedef int (*cc_ctype_fp_ischar_t)( int ch );

/**
    A char array type, size 8.
*/
typedef char str_8 [ CC_STR_8_SIZE ];


/**
    Function to clear string, or char array, by writing ``'\0'`` into every char.

    @param str__io A string to overwrite with zeros.
    @param max_len__d _Optional_, maximum length to overwrite.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_clear( char * restrict str__io,
                   size_t max_len__d );

/**
    Function counts characters in a string, based on a given filtering function.

    @param str A string.
    @param fp_is_char A function pointer, used to filter characters.
    @param max_len__d _Optional_, maximum length to count over.
    @param count__o An _output_ parameter, used to hold result.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_count_chars( char const * restrict str,
                         cc_ctype_fp_ischar_t fp_is_char,
                         size_t max_len__d,
                         size_t * restrict count__o );

/**
    Function returns a string pointer, by traversing a given string,
    and either skiping over filtered characters, or stoping at first of those.

    @param str A string.
    @param fp_is_char A function pointer, used to filter characters.
    @param skip_or_stop_at A flag, whether to skip (if `true`) or stop at (if `false`) filtered character.
    @param max_len__d _Optional_, maximum length to traverse.

    @return A string pointer within a given string, if successful, `NULL` otherwise.

    If there is no searched-for characters in a string,
    function returns pointer to the terminating character (i.e. ``'\0'``) of a given string.
*/
char const * cc_str_traverse_chars( char const * restrict str,
                                    cc_ctype_fp_ischar_t fp_is_char,
                                    bool skip_or_stop_at,
                                    size_t max_len__d );


/**
    Function converting a string in-place, to uppercase or lowercase.

    @param str__io String to convert.
    @param to_upper_or_lower Flag, convert to uppercase (`true`), or lowercase (`false`) string.
    @param max_len__d _Optional_, maximum length to convert.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_to_case( char * restrict str__io,
                     bool to_upper_or_lower,
                     size_t max_len__d );

/**
    Function returning a newly allocated string, converted to uppercase or lowercase.

    @param str String to convert.
    @param to_upper_or_lower Flag, convert to uppercase (`true`), or lowercase (`false`) string.
    @param max_len__d _Optional_, maximum length to convert.

    @return A newly allocated, converted string if successful, `NULL` otherwise.
*/
char * cc_str_to_case__new( char const * restrict str,
                            bool to_upper_or_lower,
                            size_t max_len__d );

/**
    Function returning length of a string, optionally capped at maximum length.

    @param first Pointer to a first `char` of a (sub-)string.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so length of a whole zero-terminated string is returned.
    @param max_len__d _Optional_ parameter, maximum length of a string to check. Can be `0`, if so length of whole zero-terminated string is returned.

    @warning
    If no optional arguments (`end__d`, `max_len__d`) are given, given
    string (`first`) has to be zero-terminated.

    @note
    End of a string is first `char` that does not belong to a (sub-)string.
    For a whole string (when `end__d` is `NULL`) it's a `char` past ``'\0'``.

    @return Length of a string if successful, `0` otherwise.
*/
size_t cc_str_len( char const * restrict first,
                   char const * restrict end__d,
                   size_t max_len__d );

/**
    Function returns length of a formatted variadic input.

    @param fmt A string format to append.
    @param ... Variadic input for a string format.

    @note
    Output returned is direct result of a `vsnprintf` found in `<stdio.h>`,
    see C Standard Library reference for details.

    @return Length of a formatted variadic input if non-negative,
            error code if negative.
*/
int cc_str_len_format( char const * restrict fmt, ... );

/**
    Function returns end of a given (sub-)string.

    @param str A string.
    @param max_len__d _Optional_ parameter, maximum length of a string to check. Can be `0`, if so end of whole zero-terminated string is returned.

    @warning
    If optional argument `max_len__d` is `0`, given string (`str`) has
    to be zero-terminated.

    @note
    End of a string is first `char` that does not belong to a (sub-)string,
    i.e. one past ``'\0'``, if `max_len__d` is `0`.

    @return End of a (sub-)string if successful, `NULL` otherwise.
*/
char const * cc_str_end( char const * restrict str,
                         size_t max_len__d );

/**
    Compares two (sub-)strings, returns index of a first difference.

    @param first_1 A first character of a first (sub-)string.
    @param end_1__d An _optional_ parameter, end of a first (sub-)string, can be `NULL`.
    @param first_2 A first character of a second (sub-)string.
    @param end_2__d An _optional_ parameter, end of a second (sub-)string, can be `NULL`.
    @param max_len__d An _optional_ parameter, maximum length of a strings to check. Can be `0`, if so strings are checked in their entirety.
    @param index__o An _output_ parameter, index of a first difference found.

    @note
    End of a string is a pointer to a first byte (`char`) that does not belong to a given string.
    If not given, string(s) are tested until terminating character (``'\0'``) is encountered.

    @note
    _Output_ argument contains index of a characters that differ,
    zero if given strings are equal.

    @note
    Index will be negative if, in lexographical order, first string preceedes second;
    positive if otherwise.

    @warning
    If strings are different, index starts at `1`, not `0`.
    To fetch first pair of characters that differ, use:
    ~~~{.c}
    char c1 = first_1[ llabs(index__o) - 1 ];
    char c2 = first_2[ llabs(index__o) - 1 ];
    ~~~

    @return `true` if successful, `false` otherwise.
    Index of a first difference found is returned via _output_ parameter `index__o`.
*/
bool cc_str_compare( char const * restrict first_1,
                     char const * restrict end_1__d,
                     char const * restrict first_2,
                     char const * restrict end_2__d,
                     size_t max_len__d,
                     long long * restrict index__o );

/**
    Function checks if two (sub-)strings are equal, up to a given maximum length.

    @param first_1 A first character of a first (sub-)string.
    @param end_1__d An _optional_ parameter, end of a first (sub-)string.
    @param first_2 A first character of a second (sub-)string.
    @param end_2__d An _optional_ parameter, end of a second (sub-)string.
    @param max_len__d An _optional_ parameter, maximum length of a strings to check. Can be `0`, if so strings are checked in their entirety.

    @note
    End of a string is a pointer to a first byte (`char`) that does not belong to a given string.
    If not given, string(s) are tested until terminating character (``'\0'``) is encountered.

    @return `true` if two given (sub-)strings are equal up to a maximum length, `false` otherwise.
*/
bool cc_str_is_equal( char const * restrict first_1,
                      char const * restrict end_1__d,
                      char const * restrict first_2,
                      char const * restrict end_2__d,
                      size_t max_len__d );

/**
    Function copies (sub-)string into a newly allocated string.

    @param first A (sub-)string to copy.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so entirety of a whole zero-terminated string is copied.
    @param max_len__d _Optional_, maximum length to copy, if length of string is greater than given argument. Can be `0`, if so entirety of given string is copied.

    @return Pointer to a newly allocated copy of a given string if successful, `NULL` otherwise.
*/
char * cc_str_copy__new( char const * restrict first,
                         char const * restrict end__d,
                         size_t max_len__d );

/**
    Function returns a newly allocated string containing formatted variadic input,
    capped at given maximum length.

    @param max_len__d _Optional_, maximum length to append, if length of strings is greater than given argument. Can be `0`, if so entirety of formatted string is returned.
    @param fmt A string format to append.
    @param ... Variadic input for a string format.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_str_format__new( size_t max_len__d,
                           char const * restrict fmt, ... );

/**
    Function duplicating a string, by returning a newly allocated string,
    copied from a given string, at maximum first `max_len__d` characters.

    @param str A string to duplicate.
    @param do_reverse Flag, whether returned string should be reversed.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.

    @return A newly allocated, duplicated string if successful, `NULL` otherwise.
*/
char * cc_str_duplicate__new( char const * restrict str,
                              bool do_reverse,
                              size_t max_len__d );

/**
    Function concatenating strings, by returning a newly allocated string,
    capped at a given maximum length.

    @param str_1__d An _optional_ string to copy first, can be `NULL`.
    @param str_2__d An _optional_ string to concatenate, can be `NULL`.
    @param max_len__d _Optional_, maximum length to concatenate, if a given strings are longer than that. Can be `0`, if so strings are concatenated in their entirety.

    @return A newly allocated, concatenated string if successful, `NULL` otherwise.
*/
char * cc_str_concatenate__new( char const * restrict str_1__d,
                                char const * restrict str_2__d,
                                size_t max_len__d );

/**
    Function extending existing string, by returning a newly allocated string,
    capped at given maximum length.

    @param str_1__f A string, can be unallocated.
    @param str_2__d An _optional_ string to concatenate, can be `NULL`.
    @param max_len__d _Optional_, maximum length to concatenate, if a given strings are longer than that. Can be `0`, if so zero-terminated strings are concatenated in their entirety.

    @note
    If first string is not allocated, only second string is copied into a newly allocated string.

    @note
    Allocated first string is freed, and its inner pointer is set to `NULL`, if valid result is produced.

    @note
    If no valid result is produced, allocated first string is not freed.

    @return A newly allocated, extended string if successful, `NULL` otherwise.
*/
char * cc_str_extend__new( char ** restrict str_1__f,
                           char const * restrict str_2__d,
                           size_t max_len__d );

/**
    Function appending strings, by returning a newly allocated string,
    capped at given maximum length.

    @param str_1__f A string, can be unallocated.
    @param str_2__f A string to append, can be unallocated.
    @param max_len__d _Optional_, maximum length to concatenate, if length of strings is greater than given argument. Can be `0`, if so strings are appended in their entirety.

    @note
    If both strings are allocated, resulting string is concatenating the two.

    @note
    If either string is not allocated, only allocated string is copied into a newly allocated string.

    @note
    Allocated string arguments are freed, and their inner pointers set to `NULL`, if valid result is produced.

    @note
    If no valid result is produced, allocated string arguments are not freed.

    @return A newly allocated, appended string if successful, `NULL` otherwise.
*/
char * cc_str_append__new( char ** restrict str_1__f,
                           char ** restrict str_2__f,
                           size_t max_len__d );

/**
    Function appending string and formatted variadic input, by returning a newly allocated string,
    capped at given maximum length.

    @param str__f A string, can be unallocated.
    @param max_len__d _Optional_, maximum length to append, if length of strings is greater than given argument. Can be `0`, if so strings are appended in their entirety.
    @param fmt A string format to append.
    @param ... Variadic input for a string format.

    @note
    If string is not allocated, only formatted string is copied into a newly allocated string.

    @note
    If allocated, string argument is freed, and its inner pointer set to `NULL`, if valid result is produced.

    @note
    If no valid result is produced, allocated string argument is not freed.

    @return A newly allocated, appended string if successful, `NULL` otherwise.
*/
char * cc_str_append_format__new( char ** restrict str__f,
                                  size_t max_len__d,
                                  char const * restrict fmt, ... );

/**
    Function prints (sub-)string.

    @param first A (sub-)string to copy.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so entirety of a whole zero-terminated string is printed.
    @param max_len__d _Optional_, maximum length to copy, if length of string is greater than given argument. Can be `0`, if so entirety of given string is printed.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_print( char const * restrict first,
                   char const * restrict end__d,
                   size_t max_len__d );


#endif /* __CC_STR_UTILS_H__ */
