// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STRING_H__
#define __CC_STRING_H__

#include <stdbool.h>
#include <stdlib.h>

/**
    @file cc_string.h
    @brief Strings (linked list), and related functions.
*/


/**
    String structure, linked list.
*/
typedef struct CcStrings
{
    char * str; /**< A string. */
    struct CcStrings * next; /**< Next string, in a linked list. */
} CcStrings;

/**
    Returns a newly allocated string.

    @param str String.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
CcStrings * cc_string__new( char const * restrict str,
                            size_t max_len__d );

/**
    Appends a newly allocated string to a linked list.

    @param strings__io Linked list of strings, to which a newly allocated string is appended.
    @param str String.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.

    @return
    Weak pointer to a newly allocated string, is successful, `NULL` otherwise.
*/
CcStrings * cc_string_append( CcStrings * restrict strings__io,
                              char const * restrict str,
                              size_t max_len__d );

/**
    Allocates a new string, appends it to a linked list.

    @param strings__io Linked list of strings, to which a newly allocated string is appended, can be `NULL`.
    @param str String.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.

    @note
    Linked list `*strings__io` can be `NULL`, a string will still be allocated, and returned.

    @note
    If linked list `*strings__io` is `NULL`, it will be initialized,
    with a newly allocated string as its first element.

    @return
    Weak pointer to a newly allocated string, is successful, `NULL` otherwise.
*/
CcStrings * cc_string_append_or_init( CcStrings ** restrict strings__io,
                                      char const * restrict str,
                                      size_t max_len__d );

/**
    Allocates a new string, appends it to a linked list.

    @param strings__io Linked list of strings, to which a newly allocated string is appended, can be `NULL`.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.
    @param fmt Formatting string, as defined for `printf`.
    @param ... Variadic format arguments, as used for `printf`.

    @note
    Linked list `*strings__io` can be `NULL`, a string will still be allocated, and returned.

    @note
    If linked list `*strings__io` is `NULL`, it will be initialized,
    with a newly allocated string as its first element.

    @return
    Weak pointer to a newly allocated string, is successful, `NULL` otherwise.
*/
CcStrings * cc_string_append_or_init_format( CcStrings ** restrict strings__io,
                                             size_t max_len__d,
                                             char const * restrict fmt, ... );

/**
    Frees all strings, and associated resources, in a linked list.

    @param strings__f Linked list of strings.

    @return `true` if successful, `false` otherwise.

*/
bool cc_string_free_all( CcStrings ** restrict strings__f );


#endif /* __CC_STRING_H__ */
