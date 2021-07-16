// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_TOKENIZER_H__
#define __CC_TOKENIZER_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

extern char const CC_TOKEN_SEPARATORS_WHITEPSACE[];
extern char const CC_TOKEN_SEPARATORS_PUNCTUATION[];


bool cc_char_in( char c, char const * const restrict seps );

char const * cc_traverse_chars( char const * const restrict pos,
                                char const * const restrict seps,
                                bool skip_or_stop_at );

char const * cc_skip_chars( char const * const restrict pos, char const * const restrict seps );
char const * cc_stop_at_chars( char const * const restrict pos, char const * const restrict seps );


char * cc_next_token_new( char const * const restrict str /* = NULL */,
                          char const * const restrict seps /* = NULL */ );

char * cc_str_trim_new( char const * const restrict str, char const * const restrict chars );

// size_t cc_flush_stdin();

#endif /* __CC_TOKENIZER_H__ */
