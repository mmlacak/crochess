// Copyright (c) 2021, 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_DEFS_H__
#define __CC_PARSE_DEFS_H__


/**
    @file cc_parse_defs.h
    @brief Macro, enum definitions.
*/


#define CC_CHAR_IS_PLY_GATHER(char_c) ( ( (char_c) == '[' ) || ( (char_c) == ']' ) )
#define CC_CHAR_IS_PLY_GATHER_START(char_c) ( (char_c) == '[' )
#define CC_CHAR_IS_PLY_GATHER_END(char_c) ( (char_c) == ']' )

#define CC_CHAR_IS_PIECE_SYMBOL(char_c) ( isupper( (char_c) ) )

#define CC_MAX_LEN_STEP_POS (3)
#define CC_MAX_LEN_DISAMBIGUATION (3)
#define CC_MAX_LEN_DISAMBIGUATION_STEP (6)


#endif /* __CC_PARSE_DEFS_H__ */
