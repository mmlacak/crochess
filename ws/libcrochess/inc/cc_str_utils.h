// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STR_UTILS_H__
#define __CC_STR_UTILS_H__

#include <stdbool.h>
#include <stdlib.h>

/**
    @file cc_str_utils.h
    @brief String utility functions.

    All strings are assumed to be null-terminated (``'\0'``) byte strings.

    All functions which return string, return them null-terminated.

    Length of a string is as returned by `strlen()`, i.e. without terminating character.

    Size of a string includes terminating character.
*/


/**
    Value to ignore maximum length constraint on various functions.
*/
#define CC_MAX_LEN_IGNORE (0)

/**
    Function interface, i.e. function pointer type;
    used to interface with all `ctype.h` filter functions, e.g. `islower()`.

    @param ch A single character.

    @return Integer, meaning depends on interfaced function.
*/
typedef int (*cc_ctype_fp_ischar_t)( int ch );

/**
    Function counts characters in a string, based on a given filtering function.

    @param str A string.
    @param fp_is_char A function pointer, used to filter characters.
    @param count_o An _output_ parameter, used to hold result.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_count_chars( char const * const restrict str,
                         cc_ctype_fp_ischar_t fp_is_char,
                         size_t * const restrict count_o );

/**
    Function returns a string pointer, by traversing a given string,
    and either skiping over filtered characters, or stoping at first of those.

    @param str A string.
    @param fp_is_char A function pointer, used to filter characters.
    @param skip_or_stop_at A flag, whether to skip (if `true`) or stop at (if `false`) filtered character.

    @return A string pointer within a given string, if successful, `NULL` otherwise.

    If there is no searched-for characters in a string,
    function returns pointer to the terminating character (i.e. ``'\0'``) of a given string.
*/
char const * cc_str_traverse_chars( char const * const restrict str,
                                    cc_ctype_fp_ischar_t fp_is_char,
                                    bool const skip_or_stop_at );


/**
    Function converting a string in-place, to uppercase or lowercase.

    @param str_io String to convert.
    @param to_upper_or_lower Whether to uppercase (`true`), or lowercase (`false`) string.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_to_case( char * const restrict str_io,
                     bool const to_upper_or_lower );

/**
    Function returning a newly allocated string, converted to uppercase or lowercase.

    @param str String to convert.
    @param to_upper_or_lower Whether to uppercase (`true`), or lowercase (`false`) string.

    @return A newly allocated, converted string if successful, `NULL` otherwise.
*/
char * cc_str_to_case_new( char const * const restrict str,
                           bool const to_upper_or_lower );

/**
    Function returning length of a string, optionally capped at maximum length.

    @param first Pointer to a first `char` of a (sub-)string.
    @param end_d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so length of a whole zero-terminated string is returned.
    @param max_len_d _Optional_ parameter, maximum length of a string to check. Can be `0`, if so length of whole zero-terminated string is returned.

    @warning
    If no optional arguments (`end_d`, `max_len_d`) are given, given
    string (`first`) has to be zero-terminated.

    @note
    End of a string is first `char` that does not belong to a (sub-)string.
    For a whole string (when `end_d` is `NULL`) it's a `char` past ``'\0'``.

    @return Length of a string if successful, `0` otherwise.
*/
size_t cc_str_len( char const * const restrict first,
                   char const * const restrict end_d,
                   size_t const max_len_d );

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
int cc_str_len_format( char const * const restrict fmt, ... );

/**
    Function returns end of a given (sub-)string.

    @param str A string.
    @param max_len_d _Optional_ parameter, maximum length of a string to check. Can be `0`, if so end of whole zero-terminated string is returned.

    @warning
    If optional argument `max_len_d` is `0`, given string (`str`) has
    to be zero-terminated.

    @note
    End of a string is first `char` that does not belong to a (sub-)string,
    i.e. one past '\0', if `max_len_d` is `0`.

    @return End of a (sub-)string if successful, `NULL` otherwise.
*/
char const * cc_str_end( char const * const restrict str,
                         size_t const max_len_d );

/**
    Compares two (sub-)strings, returns index of a first difference.

    @param first_1 A first character of a first (sub-)string.
    @param end_1_d An _optional_ parameter, end of a first (sub-)string, can be `NULL`.
    @param first_2 A first character of a second (sub-)string.
    @param end_2_d An _optional_ parameter, end of a second (sub-)string, can be `NULL`.
    @param max_len_d An _optional_ parameter, maximum length of a strings to check. Can be `0`, if so strings are checked in their entirety.
    @param index_o An _output_ parameter, index of a first difference found.

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
    char c1 = first_1[ llabs(index_o) - 1 ];
    char c2 = first_2[ llabs(index_o) - 1 ];
    ~~~

    @return `true` if successful, `false` otherwise.
    Index of a first difference found is returned via _output_ parameter `index_o`.
*/
bool cc_str_compare( char const * const restrict first_1,
                     char const * const restrict end_1_d,
                     char const * const restrict first_2,
                     char const * const restrict end_2_d,
                     size_t const max_len_d,
                     long long * const restrict index_o );

/**
    Function checks if two (sub-)strings are equal, up to a given maximum length.

    @param first_1 A first character of a first (sub-)string.
    @param end_1_d An _optional_ parameter, end of a first (sub-)string.
    @param first_2 A first character of a second (sub-)string.
    @param end_2_d An _optional_ parameter, end of a second (sub-)string.
    @param max_len_d An _optional_ parameter, maximum length of a strings to check. Can be `0`, if so strings are checked in their entirety.

    @note
    End of a string is a pointer to a first byte (`char`) that does not belong to a given string.
    If not given, string(s) are tested until terminating character (``'\0'``) is encountered.

    @return `true` if two given (sub-)strings are equal up to a maximum length, `false` otherwise.
*/
bool cc_str_is_equal( char const * const restrict first_1,
                      char const * const restrict end_1_d,
                      char const * const restrict first_2,
                      char const * const restrict end_2_d,
                      size_t const max_len_d );

/**
    Function copies (sub-)string into a newly allocated string.

    @param str A (sub-)string to copy.
    @param length Length to copy.
    @param str_o An _output_ string.

    @note
    _Output_ argument has to be valid pointer, with inner pointer set to `NULL`,
    i.e. `*str_o == NULL` has to be true.

    @note
    _Output_ argument is always allocated as 1 bigger than a given length.
    Extra character is always set to string terminator, i.e. ``'\0'``.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_copy_new( char const * const restrict str,
                      size_t const length,
                      char ** const restrict str_o );

/**
    Function copies (sub-)string into a newly allocated string.

    @param str A (sub-)string to copy.
    @param end An end of a (sub-)string to copy.
    @param str_o An _output_ string.

    @note
    Argument `end` points to first byte which will not be copied.

    @return `true` if successful, `false` otherwise.

    @see `cc_str_copy_new()`
*/
bool cc_str_copy_until_end_new( char const * const restrict str,
                                char const * const restrict end,
                                char ** const restrict str_o );


/**
    Function returns a newly allocated string containing formatted variadic input,
    capped at given maximum length.

    @param max_len_d _Optional_, maximum length to append, if length of strings is greater than given argument. Can be `0`, if so entirety of formatted string is returned.
    @param fmt A string format to append.
    @param ... Variadic input for a string format.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_str_format_new( size_t const max_len_d,
                          char const * const restrict fmt, ... );

/**
    Function duplicating a string, by returning a newly allocated string,
    copied from a given string, at maximum first `max_len_d` characters.

    @param str A string to duplicate.
    @param do_reverse Flag, whether returned string should be reversed.
    @param max_len_d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.

    @return A newly allocated, duplicated string if successful, `NULL` otherwise.
*/
char * cc_str_duplicate_new( char const * const restrict str,
                             bool const do_reverse,
                             size_t const max_len_d );

/**
    Function concatenating strings, by returning a newly allocated string,
    capped at a given maximum length.

    @param str_1_d An _optional_ string to copy first, can be `NULL`.
    @param str_2_d An _optional_ string to concatenate, can be `NULL`.
    @param max_len_d _Optional_, maximum length to concatenate, if a given strings are longer than that. Can be `0`, if so strings are concatenated in their entirety.

    @return A newly allocated, concatenated string if successful, `NULL` otherwise.
*/
char * cc_str_concatenate_new( char const * const restrict str_1_d,
                               char const * const restrict str_2_d,
                               size_t const max_len_d );

/**
    Function appending character to a string.

    @param str_io__r An input and output string.
    @param chr A character to concatenate.

    @note
    If allocated, string will get reallocated to accomodate additional character.

    @note
    If not yet allocated, only character is copied into a newly allocated string.

    @return `true` if successful, `false` otherwise.

    @return
    Resulting string is returned via output parameter `str_io__r`.
*/
bool cc_str_append_char( char ** const restrict str_io__r,
                         char const chr );

/**
    Function appending strings, by returning a newly allocated string,
    capped at given maximum length.

    @param str_1__f A string, can be unallocated.
    @param str_2__f A string to append, can be unallocated.
    @param max_len_d _Optional_, maximum length to concatenate, if length of strings is greater than given argument. Can be `0`, if so strings are appended in their entirety.

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
char * cc_str_append_new( char ** const restrict str_1__f,
                          char ** const restrict str_2__f,
                          size_t const max_len_d );

/**
    Function appending string and formatted variadic input, by returning a newly allocated string,
    capped at given maximum length.

    @param str__f A string, can be unallocated.
    @param max_len_d _Optional_, maximum length to append, if length of strings is greater than given argument. Can be `0`, if so strings are appended in their entirety.
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
char * cc_str_append_format_new( char ** const restrict str__f,
                                 size_t const max_len_d,
                                 char const * const restrict fmt, ... );


#endif /* __CC_STR_UTILS_H__ */
