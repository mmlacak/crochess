// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __TEST_MSGS_H__
#define __TEST_MSGS_H__

#include <stdbool.h>


typedef enum TestMsgEnum
{
    TME_Debug,
    TME_Info,
    TME_Warning,
    TME_Error,
    TME_Fatal,
} TestMsgEnum;

char const * test_msg_enum_label( TestMsgEnum tme );

bool test_print_failure( bool expr,
                         TestMsgEnum type,
                         char const * const restrict msg,
                         char const * const restrict file,
                         size_t line,
                         char const * const restrict func );


typedef struct TestMsg
{
    TestMsgEnum type;
    char const * msg;
    char const * file;
    size_t line;
    char const * func;
    struct TestMsg * next;
} TestMsg;

TestMsg * test_msg_new( TestMsgEnum type,
                        char const * const restrict msg,
                        char const * const restrict file,
                        size_t line,
                        char const * const restrict func );

TestMsg * test_msg_append_new( TestMsg * const restrict test_msgs,
                               TestMsgEnum type,
                               char const * const restrict msg,
                               char const * const restrict file,
                               size_t line,
                               char const * const restrict func );

TestMsg * test_msg_init_or_append_new( TestMsg ** const restrict test_msgs,
                                       TestMsgEnum type,
                                       char const * const restrict msg,
                                       char const * const restrict file,
                                       size_t line,
                                       char const * const restrict func );

bool test_msg_free_all( TestMsg ** const restrict test_msgs_f );

bool test_msg_print_all( TestMsg const * const restrict test_msgs,
                         TestMsgEnum level );


#endif /* __TEST_MSGS_H__ */
