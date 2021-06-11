// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_PARSER_H__
#define __CC_PARSER_H__

#include <stdbool.h>

typedef enum CcParseMsgEnum
{
    CC_PE_Empty, // Unintialized.
    CC_PE_Debug,
    CC_PE_Info,
    CC_PE_Warning,
    CC_PE_Error,
    CC_PE_Fatal,
} CcParseMsgEnum;

typedef struct CcParseMsg
{
    CcParseMsgEnum type;
    char const * sub;
    char const * msg;
    struct CcParseMsg * next;
} CcParseMsg;

CcParseMsg * cc_parse_msg_empty_new();

CcParseMsg * cc_parse_msg_new( CcParseMsgEnum type,
                               char const * const restrict sub,
                               char const * const restrict msg );

CcParseMsg * cc_parse_msg_append_new( CcParseMsg * const restrict parse_msgs,
                                      CcParseMsgEnum type,
                                      char const * const restrict sub,
                                      char const * const restrict msg );

bool cc_parse_msg_free_all( CcParseMsg ** const restrict parse_msgs );

#endif /* __CC_PARSER_H__ */
