// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSED_MOVE_H__
#define __CC_PARSED_MOVE_H__

#include "cc_parsed_ply.h"


typedef enum CcParsedMoveStatusEnum {
    CC_PMSE_None,
    CC_PMSE_DrawOffer,
    CC_PMSE_DrawOffer_Revoked,

    CC_PMSE_Check,
    CC_PMSE_Check_DrawOffer,
    CC_PMSE_Check_DrawOffer_Revoked,

    CC_PMSE_Checkmate,
    CC_PMSE_SelfCheckmate,

    CC_PMSE_Resign,
    CC_PMSE_DrawAccepted,
    CC_PMSE_DrawByRules,
} CcParsedMoveStatusEnum;


typedef struct CcParsedMove {
    char * notation;
    CcParsedPly * plies;
    // CcChessboard * cb; // TODO :: if adding, pieces and tags enums **should** be based on char, not int
    CcParsedMoveStatusEnum status;

    struct CcParsedMove * prev__w;
    struct CcParsedMove * next;
} CcParsedMove;


CcParsedMove * cc_parsed_move__new( char const * notation,
                                    size_t max_len__d,
                                    CcParsedPly ** plies__d_n,
                                    CcParsedMoveStatusEnum status );

CcParsedMove * cc_parsed_move_append( CcParsedMove ** moves__iod_a,
                                      char const * notation,
                                      size_t max_len__d,
                                      CcParsedPly ** plies__d_n,
                                      CcParsedMoveStatusEnum status );

CcParsedMove * cc_parsed_move_duplicate_all__new( CcParsedMove * moves );

bool cc_parsed_move_free_all( CcParsedMove ** moves__f );

size_t cc_parsed_move_plies_count( CcParsedMove * move );

// TODO :: DOCS
size_t cc_parsed_move_all_notations_size( CcParsedMove * move, bool is_score );


#endif /* __CC_PARSED_MOVE_H__ */
