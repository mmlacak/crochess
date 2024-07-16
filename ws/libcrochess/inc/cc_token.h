// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_TOKEN_H__
#define __CC_TOKEN_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>


extern char const CC_TOKEN_SEPARATORS_WHITESPACE[];

extern char const CC_TOKEN_SEPARATORS_PUNCTUATION[];


bool cc_char_in( char c, char const * seps );

char const * cc_traverse_chars( char const * pos,
                                char const * seps,
                                bool skip_or_stop_at );

char const * cc_skip_chars( char const * pos,
                            char const * seps );

char const * cc_stop_at_chars( char const * pos,
                               char const * seps );


bool cc_iter_token( char const * str,
                    char const * seps,
                    char const ** start__io,
                    char const ** end__io );

char * cc_trim_str__new( char const * str,
                         char const * chars );

// size_t cc_flush_stdin( void );


#endif /* __CC_TOKEN_H__ */
