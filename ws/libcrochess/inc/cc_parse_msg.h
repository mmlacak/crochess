// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_MSG_H__
#define __CC_PARSE_MSG_H__

#include <stdbool.h>
#include <stdlib.h>

/**
    @file cc_parse_msg.h
    @brief Parse messages linked list.
*/


/**
    Parser message enumeration.
*/
typedef enum CcParseMsgTypeEnum {
    CC_PMTE_Debug,
    CC_PMTE_Info,
    CC_PMTE_Warning,
    CC_PMTE_Error,
    CC_PMTE_Fatal,
} CcParseMsgTypeEnum;

/**
    Parser message structure, linked list.
*/
typedef struct CcParseMsg {
    CcParseMsgTypeEnum type; /**< Type of a parser message. */
    char * msg; /**< Parser message. */
    struct CcParseMsg * next; /**< Next parser message, in a linked list. */
} CcParseMsg;

/**
    Returns a newly allocated parser message.

    @param type Type of a parser message.
    @param msg Parser message to copy.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.

    @return A newly allocated parser message if successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg__new( CcParseMsgTypeEnum type,
                                char const * msg,
                                size_t max_len__d );

/**
    Appends a newly allocated parser message to a linked list, which might not be allocated yet.

    @param parse_msgs__iod_a **Ownership**, _optional_ _input/output_ parameter; linked list of parser messages.
    @param type Type of a parser message.
    @param msg Parser message to copy.
    @param max_len__d _Optional_, maximum length to copy.

    @note
    Linked list `*parse_msgs__iod_a` can be `NULL`, a parser message will still be allocated,
    and weak pointer to it returned.

    @note
    If linked list `*parse_msgs__iod_a` is `NULL`, it will be initialized,
    with a newly allocated parser message as its only element.

    @note
    Pointer `parse_msgs__iod_a` has ownership over given linked list, takes ownership
    over newly allocated message, and retains ownership after function returns.

    @return
    Weak pointer to a newly allocated parser message if successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_append( CcParseMsg ** parse_msgs__iod_a,
                                  CcParseMsgTypeEnum type,
                                  char const * msg,
                                  size_t max_len__d );

/**
    Allocates a new parser message, appends it to a linked list, which might not be allocated yet.

    @param parse_msgs__iod_a **Ownership**, _optional_ _input/output_ parameter; linked list of parser messages.
    @param type Type of a parser message.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.
    @param fmt Formatting string, as defined for `printf`.
    @param args Variadic format arguments, as used for `printf`.

    @see cc_parse_msg_append()

    @return
    Weak pointer to a newly allocated parser message if successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_append_fmt_va( CcParseMsg ** parse_msgs__iod_a,
                                         CcParseMsgTypeEnum type,
                                         size_t max_len__d,
                                         char const * fmt,
                                         va_list args );

/**
    Allocates a new parser message, appends it to a linked list, which might not be allocated yet.

    @param parse_msgs__iod_a **Ownership**, _optional_ _input/output_ parameter; linked list of parser messages.
    @param type Type of a parser message.
    @param max_len__d _Optional_, maximum length to copy, if a given string is longer than that. Can be `0`, if so entirety of a given string is duplicated.
    @param fmt Formatting string, as defined for `printf`.
    @param ... Variadic format arguments, as used for `printf`.

    @see cc_parse_msg_append(), cc_parse_msg_append_fmt_va()

    @return
    Weak pointer to a newly allocated parser message if successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_append_fmt( CcParseMsg ** parse_msgs__iod_a,
                                      CcParseMsgTypeEnum type,
                                      size_t max_len__d,
                                      char const * fmt, ... );

/**
    Frees all parser messages, and associated resources, in a linked list.

    @param parse_msgs__f Linked list of parser messages.

    @return `true` if successful, `false` otherwise.

*/
bool cc_parse_msg_free_all( CcParseMsg ** parse_msgs__f );

/**
    Function returning last parse message from a given linked list.

    @param parse_msgs Linked list of parse messages.

    @return Last parse message in a given linked list if successful; `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_get_last( CcParseMsg * parse_msgs );


#endif /* __CC_PARSE_MSG_H__ */
