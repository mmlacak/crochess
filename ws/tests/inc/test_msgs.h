// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

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

char * test_msg_enum_label( TestMsgEnum tme );

bool test_print_failure( bool expr,
                         TestMsgEnum type,
                         char const * restrict msg,
                         char const * restrict file,
                         size_t line,
                         char const * restrict func );


typedef struct TestMsg
{
    TestMsgEnum type;
    char * msg;
    char * file;
    size_t line;
    char * func;
    struct TestMsg * next;
} TestMsg;

TestMsg * test_msg__new( TestMsgEnum type,
                         char const * restrict msg,
                         char const * restrict file,
                         size_t line,
                         char const * restrict func );

TestMsg * test_msg_append( TestMsg * restrict test_msgs,
                           TestMsgEnum type,
                           char const * restrict msg,
                           char const * restrict file,
                           size_t line,
                           char const * restrict func );

TestMsg * test_msg_init_or_append( TestMsg ** restrict test_msgs,
                                   TestMsgEnum type,
                                   char const * restrict msg,
                                   char const * restrict file,
                                   size_t line,
                                   char const * restrict func );

bool test_msg_free_all( TestMsg ** restrict test_msgs__f );

bool test_msg_print_all( TestMsg * restrict test_msgs,
                         TestMsgEnum level );


#endif /* __TEST_MSGS_H__ */
