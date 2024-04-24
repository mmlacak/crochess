// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __TEST_MSGS_H__
#define __TEST_MSGS_H__

#include <stdbool.h>


typedef enum TestMsgEnum {
    TME_Debug,
    TME_Info,
    TME_Warning,
    TME_Error,
    TME_Fatal,
} TestMsgEnum;

char * test_msgs_enum_label( TestMsgEnum tme );

bool test_print_failure( bool expr,
                         TestMsgEnum type,
                         char const * msg,
                         char const * file,
                         size_t line,
                         char const * func );


typedef struct TestMsgs {
    TestMsgEnum type;
    char * msg;
    char * file;
    size_t line;
    char * func;
    struct TestMsgs * next;
} TestMsgs;

TestMsgs * test_msgs__new( TestMsgEnum type,
                           char const * msg,
                           char const * file,
                           size_t line,
                           char const * func );

TestMsgs * test_msgs_append( TestMsgs ** test_msgs__iod,
                             TestMsgEnum type,
                             char const * msg,
                             char const * file,
                             size_t line,
                             char const * func );

bool test_msgs_free_all( TestMsgs ** test_msgs__f );

bool test_msgs_print_all( TestMsgs * test_msgs,
                          TestMsgEnum level );


#endif /* __TEST_MSGS_H__ */
