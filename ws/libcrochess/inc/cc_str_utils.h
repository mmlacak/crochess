// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

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
    Function returning length of a string.

    @param str A string.

    @return Length of a string if successful, `0` otherwise.
*/
size_t cc_str_len( char const * const restrict str );

/**
    Function returning length of a null-terminated string, capped at maximum length.

    @param str A string.
    @param max_len Maximum length to return, if string is longer than that.

    @return Capped length of a string if successful, `0` otherwise.
*/
size_t cc_str_len_min( char const * const restrict str,
                       size_t const max_len );


/**
    Function duplicating a string, by returning a newly allocated string,
    copied from a given string.

    @param str A string to duplicate.
    @param do_reverse Flag, whether returned string should be reversed.

    @return A newly allocated, duplicated string if successful, `NULL` otherwise.
*/
char * cc_str_duplicate_new( char const * const restrict str,
                             bool const do_reverse );

/**
    Function duplicating a string, by returning a newly allocated string,
    copied from a given string, at maximum first `max_len` characters.

    @param str A string to duplicate.
    @param do_reverse Flag, whether returned string should be reversed.
    @param max_len Maximum length to copy, if a given string is longer than that.

    @return A newly allocated, duplicated string if successful, `NULL` otherwise.
*/
char * cc_str_duplicate_len_new( char const * const restrict str,
                                 bool const do_reverse,
                                 size_t const max_len );

/**
    Function concatenating strings, by returning a newly allocated string.

    @param str_1 A string to copy first, can be `NULL`.
    @param str_2 A string to concatenate, can be `NULL`.

    @return A newly allocated, concatenated string if successful, `NULL` otherwise.
*/
char * cc_str_concatenate_new( char const * const restrict str_1,
                               char const * const restrict str_2 );

/**
    Function concatenating strings, by returning a newly allocated string,
    capped at a given maximum length.

    @param str_1 A string to copy first, can be `NULL`.
    @param str_2 A string to concatenate, can be `NULL`.
    @param max_len Maximum length to concatenate, if a given strings are longer than that.

    @return A newly allocated, concatenated string if successful, `NULL` otherwise.
*/
char * cc_str_concatenate_len_new( char const * const restrict str_1,
                                   char const * const restrict str_2,
                                   size_t const max_len );

/**
    Function concatenating character to a string, by returning a newly allocated string.

    @param str A string.
    @param chr A character to concatenate.

    @note
    Given string can be `NULL`, if so, only character is copied to a newly allocated string.

    @return A newly allocated, concatenated string if successful, `NULL` otherwise.
*/
char * cc_str_concatenate_char_new( char const * const restrict str,
                                    char const chr );

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
    Function appending strings, by returning a newly allocated string.

    @param str_1__f A string, can be `NULL`.
    @param str_2__f A string to append, can be `NULL`.

    @note
    If both strings are allocated, resulting string is concatenation of the two.

    @note
    If either string is not allocated, only allocated string is copied into a newly allocated string.

    @note
    Allocated string arguments are freed, and their inner pointers set to `NULL`, if valid result is produced.

    @note
    If no valid result is produced, allocated string arguments are not freed.

    @return A newly allocated, appended string if successful, `NULL` otherwise.
*/
char * cc_str_append_new( char ** const restrict str_1__f,
                          char ** const restrict str_2__f );

/**
    Function appending strings, by returning a newly allocated string,
    capped at given maximum length.

    @param str_1__f A string, can be unallocated.
    @param str_2__f A string to append, can be unallocated.
    @param max_len Maximum length to concatenate, if length of strings is greater than given argument.

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
char * cc_str_append_len_new( char ** const restrict str_1__f,
                              char ** const restrict str_2__f,
                              size_t const max_len );

/**
    Function appending string and formatted variadic input, by returning a newly allocated string.

    @param str__f A string, can be unallocated.
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
                                 char const * const restrict fmt, ... );

/**
    Function appending string and formatted variadic input, by returning a newly allocated string,
    capped at given maximum length.

    @param str__f A string, can be unallocated.
    @param max_len Maximum length to append, if length of strings is greater than given argument.
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
char * cc_str_append_format_len_new( char ** const restrict str__f,
                                     size_t const max_len,
                                     char const * const restrict fmt, ... );


#endif /* __CC_STR_UTILS_H__ */
