// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STR_UTILS_H__
#define __CC_STR_UTILS_H__

#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>

/**
    @file cc_str_utils.h
    @brief Strings, char arrays utility functions.

    All allocated strings are assumed to be zero-terminated (``'\0'``).

    For zero-terminated strings argument `CC_MAX_LEN_ZERO_TERMINATED` can be
    used for max len parameters, if so entirety of a string will be used
    (copied, cleared, ...).

    Char arrays are zero-terminated if data is shorter than array, but
    generally are not; so an appropriate max len argument must be used,
    e.g. `CC_MAX_LEN_CHAR_8`.

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
    Invalid size, to flag argument to be ignored.
*/
#define CC_SIZE_IGNORE (0)

/**
    Size of an 8 char array.
*/
#define CC_SIZE_CHAR_8 (8)

/**
    Maximum length of an 8 char array.
*/
#define CC_MAX_LEN_CHAR_8 (CC_SIZE_CHAR_8)

/**
    Size of an 16 char array.
*/
#define CC_SIZE_CHAR_16 (16)

/**
    Maximum length of an 16 char array.
*/
#define CC_MAX_LEN_CHAR_16 (CC_SIZE_CHAR_16)

/**
    Size of an 32 char array.
*/
#define CC_SIZE_CHAR_32 (32)

/**
    Maximum length of an 32 char array.
*/
#define CC_MAX_LEN_CHAR_32 (CC_SIZE_CHAR_32)

/**
    Size of an 64 char array.
*/
#define CC_SIZE_CHAR_64 (64)

/**
    Maximum length of an 64 char array.
*/
#define CC_MAX_LEN_CHAR_64 (CC_SIZE_CHAR_64)

/**
    Size of an 128 char array.
*/
#define CC_SIZE_CHAR_128 (128)

/**
    Maximum length of an 128 char array.
*/
#define CC_MAX_LEN_CHAR_128 (CC_SIZE_CHAR_128)

/**
    Size of an 256 char array.
*/
#define CC_SIZE_CHAR_256 (256)

/**
    Maximum length of an 256 char array.
*/
#define CC_MAX_LEN_CHAR_256 (CC_SIZE_CHAR_256)

/**
    Size of an 512 char array.
*/
#define CC_SIZE_CHAR_512 (512)

/**
    Maximum length of an 512 char array.
*/
#define CC_MAX_LEN_CHAR_512 (CC_SIZE_CHAR_512)



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
typedef char cc_char_8 [ CC_SIZE_CHAR_8 ];

/**
    An empty char array, size 8.
*/
#define CC_CHAR_8_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

/**
    A char array type, size 16.
*/
typedef char cc_char_16 [ CC_SIZE_CHAR_16 ];

/**
    An empty char array, size 16.
*/
#define CC_CHAR_16_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

/**
    A char array type, size 32.
*/
typedef char cc_char_32 [ CC_SIZE_CHAR_32 ];

/**
    An empty char array, size 32.
*/
#define CC_CHAR_32_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

/**
    A char array type, size 64.
*/
typedef char cc_char_64 [ CC_SIZE_CHAR_64 ];

/**
    An empty char array, size 64.
*/
#define CC_CHAR_64_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                           '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

/**
    A char array type, size 128.
*/
typedef char cc_char_128 [ CC_SIZE_CHAR_128 ];

/**
    An empty char array, size 128.
*/
#define CC_CHAR_128_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

/**
    A char array type, size 256.
*/
typedef char cc_char_256 [ CC_SIZE_CHAR_256 ];

/**
    An empty char array, size 256.
*/
#define CC_CHAR_256_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }

/**
    A char array type, size 512.
*/
typedef char cc_char_512 [ CC_SIZE_CHAR_512 ];

/**
    An empty char array, size 512.
*/
#define CC_CHAR_512_EMPTY { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', \
                            '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' }



/**
    Function to clear string, or char array, by writing ``'\0'`` into every char.

    @param str__io _Input/output_ parameter; a string to overwrite with zeros.
    @param max_len__d _Optional_, maximum length to overwrite.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_clear( char * restrict str__io,
                   size_t max_len__d );

/**
    Function checks if string is empty, i.e. does not contain printable characters.

    @param str A string to check.
    @param ignore_spaces Flag, whether to disregard spaces, or not.

    @return `true` if empty, `false` otherwise.
*/
bool cc_str_is_empty( char const * restrict str, bool ignore_spaces );

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
    Function returns a pointer to character found in a (sub-)string.

    @param c Character to be found.
    @param case_sensitive Flag, whether search is case-sensitive, or not.
    @param start Pointer to a start of a (sub-)string.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so end of a whole zero-terminated string is searched.
    @param max_len__d _Optional_ parameter, maximum length of a string to check. Can be `0`, if so end of whole zero-terminated string is searched.

    @warning
    If no optional arguments (`end__d`, `max_len__d`) are given, given
    string (`start`) has to be zero-terminated.

    @note
    End of a string is first `char` that does not belong to a (sub-)string.
    For a whole string (when `end__d` is `NULL`) it's a ``'\0'``, i.e. zero-terminating `char`.

    @return Pointer to a character if successful, `NULL` otherwise.
*/
char const * cc_str_contains_char( char c,
                                   bool case_sensitive,
                                   char const * restrict start,
                                   char const * restrict end__d,
                                   size_t max_len__d );

/**
    Function returns a string pointer, by traversing a given string,
    and either skyping over filtered characters, or stopping at first of those.

    @param str A string.
    @param fp_is_char A function pointer, used to filter characters.
    @param skip_or_stop_at A flag, whether to skip (if `true`) or stop at (if `false`) filtered character.
    @param max_len__d _Optional_, maximum length to traverse.

    @return A string pointer within a given string if successful, `NULL` otherwise.

    If there is no searched-for characters in a string,
    function returns pointer to the terminating character (i.e. ``'\0'``) of a given string.
*/
char const * cc_str_traverse_chars( char const * restrict str,
                                    cc_ctype_fp_ischar_t fp_is_char,
                                    bool skip_or_stop_at,
                                    size_t max_len__d );


/**
    Function converting a string in-place, to uppercase or lowercase.

    @param str__io _Input/output_ parameter; string to convert.
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
    Function returning pointer to the end of a string, optionally capped at maximum length and/or delimited by optional (sub-)string end.

    @param start Pointer to a starting `char` of a (sub-)string.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so end of a whole zero-terminated string is returned.
    @param max_len__d _Optional_ parameter, maximum length of a string to check. Can be `0`, if so end of whole zero-terminated string is returned.

    @warning
    If no optional arguments (`end__d`, `max_len__d`) are given, given
    string (`start`) has to be zero-terminated.

    @note
    End of a string is first `char` that does not belong to a (sub-)string.
    For a whole string (when `end__d` is `NULL`) it's a ``'\0'``, i.e. zero-terminating `char`.

    @return Pointer to the end of a string if successful, `NULL` otherwise.
*/
char const * cc_str_end( char const * restrict start,
                         char const * restrict end__d,
                         size_t max_len__d );

/**
    Function returning length of a string, optionally capped at maximum length.

    @param start Pointer to a starting `char` of a (sub-)string.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so length of a whole zero-terminated string is returned.
    @param max_len__d _Optional_ parameter, maximum length of a string to check. Can be `0`, if so length of whole zero-terminated string is returned.

    @warning
    If no optional arguments (`end__d`, `max_len__d`) are given, given
    string (`start`) has to be zero-terminated.

    @note
    End of a string is first `char` that does not belong to a (sub-)string.
    For a whole string (when `end__d` is `NULL`) it's a ``'\0'``, zero-terminating `char`.

    @return Length of a string if successful, `0` otherwise.
*/
size_t cc_str_len( char const * restrict start,
                   char const * restrict end__d,
                   size_t max_len__d );

/**
    Function returns length of a formatted variadic input.

    @param fmt A string format to append.
    @param args Variadic list, input for a string format.

    @note
    Output returned is direct result of a `vsnprintf` found in `<stdio.h>`,
    see C Standard Library reference for details.

    @return Length of a formatted variadic input if non-negative,
            error code if negative.
*/
int cc_str_len_fmt_va( char const * restrict fmt, va_list args );

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
    Function checks if two (sub-)strings are equal, up to a given maximum length.

    @param start_1 A starting character of a first (sub-)string.
    @param end_1__d An _optional_ parameter, end of a first (sub-)string.
    @param start_2 A starting character of a second (sub-)string.
    @param end_2__d An _optional_ parameter, end of a second (sub-)string.
    @param max_len__d An _optional_ parameter, maximum length of a strings to check. Can be `0`, if so strings are checked in their entirety.

    @note
    End of a string is a pointer to a first byte (`char`) that does not belong to a given (sub-)string.
    If not given, string(s) are tested until terminating character (``'\0'``) is encountered.

    @return `true` if two given (sub-)strings are equal up to a maximum length, `false` otherwise.
*/
bool cc_str_is_equal( char const * restrict start_1,
                      char const * restrict end_1__d,
                      char const * restrict start_2,
                      char const * restrict end_2__d,
                      size_t max_len__d );


/**
    Function copies (sub-)string into a char array, or already allocated string.

    @param start A (sub-)string to copy.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL`, if so entirety of a whole zero-terminated string is copied.
    @param max_len__d _Optional_, maximum length to copy, if length of string is greater than given argument. Can be `0`, if so entirety of given string is copied.
    @param dest__o Pointer to destination.
    @param dest_end__d _Optional_, pointer to an end of destination; can be `NULL`.
    @param size_dest__d _Optional_, size of a destination, if it's char array, or if it's allocated for less than size of a (sub-)string to copy.

    @note
    Function will zero-terminate copied string, if there is enough space.

    @return Count of characters copied (not including ``'\0'``) if successful, `0` otherwise.
*/
size_t cc_str_copy( char const * restrict start,
                    char const * restrict end__d,
                    size_t max_len__d,
                    char * restrict dest__o,
                    char const * restrict dest_end__d,
                    size_t size_dest__d );

/**
    Function copies (sub-)string into a newly allocated string.

    @param start A (sub-)string to copy.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so entirety of a whole zero-terminated string is copied.
    @param max_len__d _Optional_, maximum length to copy, if length of string is greater than given argument. Can be `0`, if so entirety of given string is copied.

    @return Pointer to a newly allocated copy of a given string if successful, `NULL` otherwise.
*/
char * cc_str_copy__new( char const * restrict start,
                         char const * restrict end__d,
                         size_t max_len__d );

/**
    Function returns a newly allocated string containing formatted variadic input,
    capped at given maximum length.

    @param max_len__d _Optional_, maximum length to append, if length of strings is greater than given argument. Can be `0`, if so entirety of formatted string is returned.
    @param fmt A string format to append.
    @param args Variadic input list for a string format.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_str_fmt_va__new( size_t max_len__d,
                           char const * restrict fmt,
                           va_list args );

/**
    Function returns a newly allocated string containing formatted variadic input,
    capped at given maximum length.

    @param max_len__d _Optional_, maximum length to append, if length of strings is greater than given argument. Can be `0`, if so entirety of formatted string is returned.
    @param fmt A string format to append.
    @param ... Variadic input for a string format.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
char * cc_str_fmt__new( size_t max_len__d,
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
    Function appends second string into a first one.

    @param str__io _Input/output_ parameter; a string into which to append.
    @param size_dest__d _Optional_, size of a destination.
    @param str A string to append.
    @param max_len__d _Optional_, maximum length to append, if a given string is longer than that.

    @note
    An _input/output_ `str__io` buffer must always be zero-terminated.

    @note
    If size is given (i.e. `size_dest__d` is not `0`), zero-terminating char
    must be within sized buffer, i.e. before `str__io + size_dest__d` is reached.

    @note
    _Optional_ `size_dest__d` can be `0` (use `CC_SIZE_IGNORE`),
    if so destination string array/allocation (i.e. `str__io`) is assumed
    to be large enough to accommodate complete appending string `str`.

    @note
    _Optional_ `max_len__d` can be `0` (use `CC_MAX_LEN_ZERO_TERMINATED`),
    if so string `str` must be zero-terminated, and is appended in its entirety.

    @note
    _Output_ string `str__io` after appending string is always zero-terminated.
    Function returns weak pointer to that zero-terminating char.

    Walking pointer over destination buffer can be used in succession, or in a loop.
    This is so because function seeks terminating char from a given destination
    (i.e. `str__io`), and returns a weak pointer to a new zero-terminating char
    after appending in that same destination buffer.

    For instance:
    @code{.c}
    buffer__a[ 0 ] = '\0'; // Must always contain zero-terminated string, initialize to '\0' if it doesn't (e.g. just allocated).

    char * walking = buffer__a;

    while ( iter( ... ) ) {
        walking = cc_str_append_into( walking, ... );
        if ( !walking ) break; }
    @endcode

    @see
    CC_SIZE_IGNORE, CC_MAX_LEN_ZERO_TERMINATED

    @return A weak pointer to zero-terminating char if successful, `NULL` otherwise.
*/
char * cc_str_append_into( char * restrict str__io,
                           size_t size_dest__d,
                           char const * restrict str,
                           size_t max_len__d );

/**
    Function appends strings, by returning a newly allocated string,
    capped at a given maximum length.

    @param str_1__d An _optional_ string to copy first, can be `NULL`.
    @param str_2__d An _optional_ string to concatenate, can be `NULL`.
    @param max_len__d _Optional_, maximum length to concatenate, if a given strings are longer than that. Can be `0`, if so strings are concatenated in their entirety.

    @return A newly allocated, appended string if successful, `NULL` otherwise.
*/
char * cc_str_append__new( char const * restrict str_1__d,
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
char * cc_str_append_free__new( char ** restrict str_1__f,
                                char ** restrict str_2__f,
                                size_t max_len__d );

/**
    Function appending string and formatted variadic input, by returning a newly allocated string,
    capped at given maximum length.

    @param str__f A string, can be unallocated.
    @param max_len__d _Optional_, maximum length to append, if length of strings is greater than given argument. Can be `0`, if so strings are appended in their entirety.
    @param fmt A string format to append.
    @param args Variadic input list for a string format.

    @note
    If string is not allocated, only formatted string is copied into a newly allocated string.

    @note
    If allocated, string argument `str__f` is freed, and its inner pointer set to `NULL`, if valid result is produced.

    @note
    If no valid result is produced, allocated string argument `str__f` is not freed.

    @return A newly allocated, appended string if successful, `NULL` otherwise.
*/
char * cc_str_append_fmt_va__new( char ** restrict str__f,
                                  size_t max_len__d,
                                  char const * restrict fmt,
                                  va_list args );

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
    If allocated, string argument `str__f` is freed, and its inner pointer set to `NULL`, if valid result is produced.

    @note
    If no valid result is produced, allocated string argument `str__f` is not freed.

    @return A newly allocated, appended string if successful, `NULL` otherwise.
*/
char * cc_str_append_fmt__new( char ** restrict str__f,
                               size_t max_len__d,
                               char const * restrict fmt, ... );

// TODO :: (?) move / return newly allocated string (?)
//
/**
    Function prints (sub-)string, followed by formatted variadic input.

    @param start A (sub-)string to copy.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so entirety of a whole zero-terminated string is printed.
    @param max_len__d _Optional_, can be `0`; maximum length of string to print.
    @param fmt_str A string format to print copied (sub-)string.
    @param fmt_len__d _Optional_, can be `0`; maximum length of formatted string to print.
    @param fmt__d _Optional_, can be `NULL`; string format to print variadic input.
    @param ... Variadic input for a string format.

    @note
    Substring supplied via `start` and `end__d` arguments is copied into newly allocated, zero-terminated string.

    @note
    That internal string is passed as the only argument to `printf` using `fmt_str` formatting,
    which must not contain any other formatting parameters.

    @warning
    So, `fmt_str` must have the only one formatting specified as string (i.e. `%%s`),
    to handle internal string as the only argument to `printf`.

    Note, format specifier in this example, the only `%%s` corresponds to internal string.
    @code{.c}
    cc_str_printf( token_start, token_end, BUFSIZ, "No help entry: '%s'.\n", CC_MAX_LEN_ZERO_TERMINATED, NULL );
    @endcode

    @note
    Variadic input is handled by `printf` following the first one, and is formatted by `fmt__d`, e.g.:
    @code{.c}
    cc_str_print( start, c, 128, "Ply link: '%s'", " --> %d.\n", ple );
    @endcode

    <div></div><!-- Enforces the two notes are rendered separately. -->
    @note
    Compile-time constant which controls definition of this function is `__CC_STR_PRINT_INFO__`.

    @return `true` if successful, `false` otherwise.
*/
bool cc_str_print( char const * restrict start,
                   char const * restrict end__d,
                   size_t max_len__d,
                   char const * restrict fmt_str,
                   size_t fmt_len__d,
                   char const * restrict fmt__d, ... );

/**
    Macro to call `cc_str_print()`, depending on a compile-time constant.

    @param start A (sub-)string to copy.
    @param end__d _Optional_, pointer to an end of a (sub-)string. Can be `NULL` if so entirety of a whole zero-terminated string is printed.
    @param max_len__d _Optional_, can be `0`; maximum length of string to print.
    @param fmt_str A string format to print copied (sub-)string.
    @param fmt_len__d _Optional_, can be `0`; maximum length of formatted string to print.
    @param fmt__d _Optional_, can be `NULL`; string format to print variadic input.
    @param ... Variadic input for a string format.

    @note
    Compile-time constant which controls definition of this macro is `__CC_STR_PRINT_INFO__`.

    @see cc_str_print()

    @return `true` if successful, `false` otherwise.
*/
#ifdef __CC_STR_PRINT_INFO__
#define CC_STR_PRINT_IF_INFO(start,end__d,max_len__d,fmt_str,fmt_len__d,fmt__d,...)                   \
    cc_str_print( start, end__d, max_len__d, fmt_str, fmt_len__d, fmt__d __VA_OPT__(,) __VA_ARGS__ )
#else // __CC_STR_PRINT_INFO__
#define CC_STR_PRINT_IF_INFO(start,end__d,max_len__d,fmt_str,fmt__d,...) true
#endif // __CC_STR_PRINT_INFO__
//
// TODO :: (?) move / return newly allocated string (?)


#endif /* __CC_STR_UTILS_H__ */
