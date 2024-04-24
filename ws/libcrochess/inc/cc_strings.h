// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_STRING_H__
#define __CC_STRING_H__

#include <stdbool.h>
#include <stdlib.h>

/**
    @file cc_strings.h
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

    @param str String to copy.
    @param max_len__d _Optional_, maximum length to copy.

    @return A newly allocated string if successful, `NULL` otherwise.
*/
CcStrings * cc_strings__new( char const * str,
                             size_t max_len__d );

/**
    Appends a newly allocated string to a linked list.

    @param strings__iod_a **Ownership**, _optional_ _input/ouptut_ parameter; linked list of strings.
    @param str String to copy.
    @param max_len__d _Optional_, maximum length to copy.

    @note
    Linked list `*strings__iod_a` can be `NULL`, a newly allocated string will
    still be allocated, and weak pointer to it returned.

    @note
    If linked list `*strings__iod_a` is `NULL`, it will be initialized,
    with a newly allocated string as its only element.

    @note
    Pointer `strings__iod_a` has ownership over given linked list, takes ownership
    over newly allocated string, and retains ownership after function returns.

    @return
    Weak pointer to a newly allocated string if successful, `NULL` otherwise.
*/
CcStrings * cc_strings_append( CcStrings ** strings__iod_a,
                               char const * str,
                               size_t max_len__d );

/**
    Allocates a new string, appends it to a linked list, which might not be allocated yet.

    @param strings__iod_a **Ownership**, _optional_ _input/ouptut_ parameter; linked list of strings.
    @param max_len__d _Optional_, maximum length to copy.
    @param fmt Formatting string, as defined for `printf`.
    @param args Variadic format arguments, as used for `printf`.

    @see cc_strings_append()

    @return
    Weak pointer to a newly allocated string if successful, `NULL` otherwise.
*/
CcStrings * cc_strings_append_fmt_va( CcStrings ** strings__iod_a,
                                      size_t max_len__d,
                                      char const * fmt,
                                      va_list args );

/**
    Allocates a new string, appends it to a linked list, which might not be allocated yet.

    @param strings__iod_a **Ownership**, _optional_ _input/ouptut_ parameter; linked list of strings.
    @param max_len__d _Optional_, maximum length to copy.
    @param fmt Formatting string, as defined for `printf`.
    @param ... Variadic format arguments, as used for `printf`.

    @see cc_strings_append(), cc_strings_append_fmt_va()

    @return
    Weak pointer to a newly allocated string if successful, `NULL` otherwise.
*/
CcStrings * cc_strings_append_fmt( CcStrings ** strings__iod_a,
                                   size_t max_len__d,
                                   char const * fmt, ... );

/**
    Duplicates all strings in a linked list.

    @param strings Linked list of strings.

    @return Pointer to a newly allocated linked list if successful, `NULL` otherwise.
*/
CcStrings * cc_strings_duplicate_all__new( CcStrings * strings );

/**
    Frees all strings, and associated resources, in a linked list.

    @param strings__f Linked list of strings.

    @return `true` if successful, `false` otherwise.
*/
bool cc_strings_free_all( CcStrings ** strings__f );


#endif /* __CC_STRING_H__ */
