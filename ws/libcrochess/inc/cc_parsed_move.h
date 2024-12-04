// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_MOVE_H__
#define __CC_MOVE_H__

#include "cc_parsed_ply.h"


typedef enum CcMoveStatusEnum {
    CC_MSE_None,
    CC_MSE_DrawOffer,
    CC_MSE_DrawOffer_Revoked,

    CC_MSE_Check,
    CC_MSE_Check_DrawOffer,
    CC_MSE_Check_DrawOffer_Revoked,

    CC_MSE_Checkmate,
    CC_MSE_SelfCheckmate,

    CC_MSE_Resign,
    CC_MSE_DrawAccepted,
    CC_MSE_DrawByRules,
} CcMoveStatusEnum;


typedef struct CcMove {
    char * notation;
    CcParsedPly * plies;
    // CcChessboard * cb; // TODO :: if adding, pieces and tags enums **should** be based on char, not int
    CcMoveStatusEnum status;

    struct CcMove * prev__w;
    struct CcMove * next;
} CcMove;


CcMove * cc_move__new( char const * notation,
                       size_t max_len__d,
                       CcParsedPly ** plies__d_n,
                       CcMoveStatusEnum status );

CcMove * cc_move_append( CcMove ** moves__iod_a,
                         char const * notation,
                         size_t max_len__d,
                         CcParsedPly ** plies__d_n,
                         CcMoveStatusEnum status );

CcMove * cc_move_duplicate_all__new( CcMove * moves );

bool cc_move_free_all( CcMove ** moves__f );

size_t cc_move_plies_count( CcMove * move );

size_t cc_move_all_notations_size( CcMove * move, bool is_score );

char * cc_move_as_string__new( CcMove * move, bool is_score );


#endif /* __CC_MOVE_H__ */
