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
    TME_Fatal, } TestMsgEnum;

char * test_msgs_enum_label( TestMsgEnum tme );

bool test_print_failure( bool expr,
                         TestMsgEnum type,
                         char const * restrict msg,
                         char const * restrict file,
                         size_t line,
                         char const * restrict func );


typedef struct TestMsgs
{
    TestMsgEnum type;
    char * msg;
    char * file;
    size_t line;
    char * func;
    struct TestMsgs * next; } TestMsgs;

TestMsgs * test_msgs__new( TestMsgEnum type,
                           char const * restrict msg,
                           char const * restrict file,
                           size_t line,
                           char const * restrict func );

TestMsgs * test_msgs_append( TestMsgs * restrict test_msgs,
                             TestMsgEnum type,
                             char const * restrict msg,
                             char const * restrict file,
                             size_t line,
                             char const * restrict func );

TestMsgs * test_msgs_init_or_append( TestMsgs ** restrict test_msgs,
                                     TestMsgEnum type,
                                     char const * restrict msg,
                                     char const * restrict file,
                                     size_t line,
                                     char const * restrict func );

bool test_msgs_free_all( TestMsgs ** restrict test_msgs__f );

bool test_msgs_print_all( TestMsgs * restrict test_msgs,
                          TestMsgEnum level );


#endif /* __TEST_MSGS_H__ */
