// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_POS_H__
#define __CC_POS_H__

#include <stdbool.h>


// DOCS
typedef struct CcPos {
    int i;
    int j;
} CcPos;

// DOCS
CcPos cc_pos( int i, int j );
CcPos cc_pos_empty();

// DOCS
CcPos cc_pos_add( CcPos augend, CcPos addend );
CcPos cc_pos_subtract( CcPos minuend, CcPos subtrahend );

// DOCS
bool cc_pos_is_equal( CcPos pos_1, CcPos pos_2 );
bool cc_pos_is_not_equal( CcPos pos_1, CcPos pos_2 );


// DOCS
typedef struct CcPosLink {
    int i;
    int j;

    struct CcPosLink * next;
} CcPosLink;

// DOCS
CcPosLink * cc_pos_link_new( int i, int j );
CcPosLink * cc_pos_link_from_pos_new( CcPos pos );

CcPos cc_pos_from_pos_link( CcPosLink * restrict pos_link );

// DOCS
CcPosLink * cc_pos_link_append( CcPosLink * restrict pos_link__io,
                                int i,
                                int j );

// DOCS
CcPosLink * cc_pos_link_append_or_init( CcPosLink ** restrict pos_link__io,
                                        int i,
                                        int j );

// DOCS
CcPosLink * cc_pos_link_append_pos( CcPosLink * restrict pos_link__io,
                                    CcPos pos );

// DOCS
CcPosLink * cc_pos_link_append_pos_or_init( CcPosLink ** restrict pos_link__io,
                                            CcPos pos );

// DOCS
bool cc_pos_link_free_all( CcPosLink ** restrict pos_link__f );


#endif /* __CC_POS_H__ */
