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
CcPos cc_pos( int const i, int const j );
CcPos cc_pos_empty();

// DOCS
CcPos cc_pos_add( CcPos const augend, CcPos const addend );
CcPos cc_pos_subtract( CcPos const minuend, CcPos const subtrahend );

// DOCS
bool cc_pos_is_equal( CcPos const pos_1, CcPos const pos_2 );
bool cc_pos_is_not_equal( CcPos const pos_1, CcPos const pos_2 );


// DOCS
typedef struct CcPosLink {
    int i;
    int j;

    struct CcPosLink * next;
} CcPosLink;

// DOCS
CcPosLink * cc_pos_link_new( int const i, int const j );
CcPosLink * cc_pos_link_from_pos_new( CcPos const pos );

CcPos cc_pos_from_pos_link( CcPosLink const * const restrict pos_link );

// DOCS
CcPosLink * cc_pos_link_append( CcPosLink * const restrict pos_link__io,
                                int const i,
                                int const j );

// DOCS
CcPosLink * cc_pos_link_append_or_init( CcPosLink ** const restrict pos_link__io,
                                        int const i,
                                        int const j );

// DOCS
CcPosLink * cc_pos_link_append_pos( CcPosLink * const restrict pos_link__io,
                                    CcPos const pos );

// DOCS
CcPosLink * cc_pos_link_append_pos_or_init( CcPosLink ** const restrict pos_link__io,
                                            CcPos const pos );


#endif /* __CC_POS_H__ */
