// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PARSER_H__
#define __CC_PARSER_H__

#include <stdbool.h>
#include <stdlib.h>

/**
    @file cc_parse_msg.h
    @brief Parse message enumeration, structure, and related functions.
*/


/**
    Parser message enumeration.
*/
typedef enum CcParseMsgEnum
{
    CC_PME_Debug,
    CC_PME_Info,
    CC_PME_Warning,
    CC_PME_Error,
    CC_PME_Fatal,
} CcParseMsgEnum;

/**
    Parser message structure, linked list.
*/
typedef struct CcParseMsg
{
    CcParseMsgEnum type; /**< Type of a parser message. */
    size_t pos; /**< Position within string, e.g. user input. */
    char const * msg; /**< Parser message. */
    struct CcParseMsg * next; /**< Next parser message, in a linked list. */
} CcParseMsg;

/**
    Returns a newly allocated parser message.

    @param type Type of a parser message.
    @param pos Position within string, e.g. user input.
    @param msg Parser message.

    @return A newly allocated parser message if successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_new( CcParseMsgEnum const type,
                               size_t const pos,
                               char const * const restrict msg );

/**
    Appends a newly Allocated parser message to a linked list.

    @param parse_msgs Linked list of parser messages, to which a newly allocated parser message is appended.
    @param type Type of a parser message.
    @param pos Position within string, e.g. user input.
    @param msg Parser message.

    @return
    A newly allocated parser message, is successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_append_new( CcParseMsg * const restrict parse_msgs,
                                      CcParseMsgEnum const type,
                                      size_t const pos,
                                      char const * const restrict msg );

/**
    Allocates a new parser message, appends it to a linked list.

    @param parse_msgs_io Linked list of parser messages, to which a newly allocated parser message is appended.
    @param type Type of a parser message.
    @param pos Position within string, e.g. user input.
    @param msg Parser message.

    @note
    Linked list `*parse_msgs_io` can be `NULL`, a parser message will still be allocated, and returned.

    @note
    If linked list `*parse_msgs_io` is `NULL`, it will be initialized,
    with a newly allocated parser message as its first element.

    @return
    A newly allocated parser message, is successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_init_or_append_new( CcParseMsg ** const restrict parse_msgs_io,
                                              CcParseMsgEnum const type,
                                              size_t const pos,
                                              char const * const restrict msg );

/**
    Frees all parser messages, and associated resources, in a linked list.

    @param parse_msgs_f Linked list of parser messages.

    @return `true` if successful, `false` otherwise.

*/
bool cc_parse_msg_free_all( CcParseMsg ** const restrict parse_msgs_f );

/**
    Function returning last parse message from a given linked list.

    @param parse_msgs Linked list of parse messages.

    @return Last parse message in a given linked list, if successful; `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_get_last( CcParseMsg const * const restrict parse_msgs );


#endif /* __CC_PARSER_H__ */
