// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_DEFINES_H__
#define __CC_DEFINES_H__

/**
    @file cc_defines.h
    @brief Defines and macros used in project.
*/

/**
    Smallest, invalid off-board coordinate.

    No valid trance-journey starting from any chessboard field could get to this coordinate,
    and still make it back to any on-board field.

    Used for e.g. missing coordinates, for which invalid value ascertains no useage.
*/
#define CC_INVALID_OFF_BOARD_COORD_MIN (-99)

/**
    Invalid off-board coordinate, used as an empty argument.

    Used to pass empty (non-initializing) values to _static_ parameters,
    which would otherwise initialize _static_ variables in a function body.
*/
#define CC_STATIC_NONE (-99)

/**
    Off-board coordinate.

    Used for e.g. missing coordinates, if notation contains only destination field.
*/
#define CC_OFF_BOARD_COORD (-1)

/**
    The smallest valid on-board coordinate.
*/
#define CC_MIN_BOARD_COORD (0)

/**
    The largest valid on-board coordinate.

    This is the largest valid coordinate for the largest board, used by One variant.
    For other variants actual upper limit is smaller, see `cc_variant_board_size()`.
*/
#define CC_MAX_BOARD_COORD (25)

/**
    The largest valid board size.

    This is the largest valid board size for the largest board, used by One variant.
    For other variants actual upper limit is smaller, see `cc_variant_board_size()`.
*/
#define CC_MAX_BOARD_SIZE (26)

/**
    Macro to inline comparing, producing smaller value.
*/
#define CC_MIN(x,y) ( ( (y) > (x) ) ? (x) : (y) )

/**
    Macro to inline comparing, producing larger value.
*/
#define CC_MAX(x,y) ( ( (x) > (y) ) ? (x) : (y) )

/**
    Macro to inline sign function.
*/
#define CC_SIGN(i) ( ( (i) > 0 ) ? (1) : ( ( (i) < 0 ) ? (-1) : (0) ) )


#endif /* __CC_DEFINES_H__ */
