// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

#ifndef __CC_DEFINES_H__
#define __CC_DEFINES_H__


#define CC_OFF_BOARD_COORD (-1)
#define CC_MIN_BOARD_COORD (0)
#define CC_MAX_BOARD_COORD (25)
#define CC_MAX_BOARD_SIZE (26)

#define CC_MIN(x,y) ( ( (y) > (x) ) ? (x) : (y) )
#define CC_MAX(x,y) ( ( (x) > (y) ) ? (x) : (y) )


#endif /* __CC_DEFINES_H__ */
