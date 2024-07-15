// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_MSG_H__
#define __CC_PARSE_MSG_H__

#include <stdbool.h>
#include <stdlib.h>


typedef enum CcParseMsgTypeEnum {
    CC_PMTE_Debug,
    CC_PMTE_Info,
    CC_PMTE_Warning,
    CC_PMTE_Error,
    CC_PMTE_Fatal,
} CcParseMsgTypeEnum;

typedef struct CcParseMsg {
    CcParseMsgTypeEnum type;
    char * msg;
    struct CcParseMsg * next;
} CcParseMsg;

CcParseMsg * cc_parse_msg__new( CcParseMsgTypeEnum type,
                                char const * msg,
                                size_t max_len__d );

CcParseMsg * cc_parse_msg_append( CcParseMsg ** parse_msgs__iod_a,
                                  CcParseMsgTypeEnum type,
                                  char const * msg,
                                  size_t max_len__d );

CcParseMsg * cc_parse_msg_append_fmt_va( CcParseMsg ** parse_msgs__iod_a,
                                         CcParseMsgTypeEnum type,
                                         size_t max_len__d,
                                         char const * fmt,
                                         va_list args );

CcParseMsg * cc_parse_msg_append_fmt( CcParseMsg ** parse_msgs__iod_a,
                                      CcParseMsgTypeEnum type,
                                      size_t max_len__d,
                                      char const * fmt, ... );

bool cc_parse_msg_free_all( CcParseMsg ** parse_msgs__f );

CcParseMsg * cc_parse_msg_get_last( CcParseMsg * parse_msgs );


#endif /* __CC_PARSE_MSG_H__ */
