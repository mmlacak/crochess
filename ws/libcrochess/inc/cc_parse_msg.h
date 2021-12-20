// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

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
    char * msg; /**< Parser message. */
    struct CcParseMsg * next; /**< Next parser message, in a linked list. */
} CcParseMsg;

/**
    Returns a newly allocated parser message.

    @param type Type of a parser message.
    @param msg Parser message.

    @return A newly allocated parser message if successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_new( CcParseMsgEnum type,
                               char const * restrict msg );

/**
    Appends a newly allocated parser message to a linked list.

    @param parse_msgs Linked list of parser messages, to which a newly allocated parser message is appended.
    @param type Type of a parser message.
    @param msg Parser message.

    @return
    Weak pointer to a newly allocated parser message, is successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_append( CcParseMsg * restrict parse_msgs__io,
                                  CcParseMsgEnum type,
                                  char const * restrict msg );

/**
    Allocates a new parser message, appends it to a linked list.

    @param parse_msgs_io Linked list of parser messages, to which a newly allocated parser message is appended, can be `NULL`.
    @param type Type of a parser message.
    @param msg Parser message.

    @note
    Linked list `*parse_msgs_io` can be `NULL`, a parser message will still be allocated, and returned.

    @note
    If linked list `*parse_msgs_io` is `NULL`, it will be initialized,
    with a newly allocated parser message as its first element.

    @return
    Weak pointer to a newly allocated parser message, is successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_append_or_init( CcParseMsg ** restrict parse_msgs__io,
                                          CcParseMsgEnum type,
                                          char const * restrict msg );

/**
    Allocates a new parser message, appends it to a linked list.

    @param parse_msgs_io Linked list of parser messages, to which a newly allocated parser message is appended, can be `NULL`.
    @param type Type of a parser message.
    @param fmt Formatting string, as defined for `printf`.
    @param ... Variadic format arguments, as used for `printf`.

    @note
    Linked list `*parse_msgs_io` can be `NULL`, a parser message will still be allocated, and returned.

    @note
    If linked list `*parse_msgs_io` is `NULL`, it will be initialized,
    with a newly allocated parser message as its first element.

    @note
    Maximum length of a formatted string output is limited at `BUFSIZ`, constant from `<stdio.h>`.

    @return
    Weak pointer to a newly allocated parser message, is successful, `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_append_or_init_format( CcParseMsg ** restrict parse_msgs__io,
                                                 CcParseMsgEnum type,
                                                 char const * restrict fmt, ... );

/**
    Frees all parser messages, and associated resources, in a linked list.

    @param parse_msgs__f Linked list of parser messages.

    @return `true` if successful, `false` otherwise.

*/
bool cc_parse_msg_free_all( CcParseMsg ** restrict parse_msgs__f );

/**
    Function returning last parse message from a given linked list.

    @param parse_msgs Linked list of parse messages.

    @return Last parse message in a given linked list, if successful; `NULL` otherwise.
*/
CcParseMsg * cc_parse_msg_get_last( CcParseMsg * restrict parse_msgs );


#endif /* __CC_PARSER_H__ */
