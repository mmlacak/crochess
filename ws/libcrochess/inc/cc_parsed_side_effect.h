// Copyright (c) 2021, 2022, 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSED_SIDE_EFFECT_H__
#define __CC_PARSED_SIDE_EFFECT_H__

#include <stddef.h>

#include "cc_str_utils.h"

#include "cc_piece.h"
#include "cc_tag.h"
#include "cc_pos.h"


typedef enum CcParsedSideEffectEnum {
    CC_PSEE_None, /* No side effects. */
    CC_PSEE_Capture, /* Corresponds to `*`. */
    CC_PSEE_Displacement, /* Corresponds to `<`. */
    CC_PSEE_EnPassant, /* Corresponds to `:`. */
    CC_PSEE_Castle, /* Corresponds to `&`. */
    CC_PSEE_Promotion, /* Corresponds to `=`, ``. */
    CC_PSEE_TagForPromotion, /* Corresponds to `=`. */
    CC_PSEE_Conversion, /* Corresponds to `%`. */
    CC_PSEE_FailedConversion, /* Corresponds to `%%`. */
    CC_PSEE_Transparency, /* Corresponds to `^`. */
    CC_PSEE_Divergence, /* Corresponds to `/`. */
    CC_PSEE_DemoteToPawn, /* Corresponds to `>`. */
    CC_PSEE_Resurrection, /* Corresponds to `$`. */
    CC_PSEE_ResurrectingOpponent, /* Corresponds to `$$`. */
    CC_PSEE_FailedResurrection, /* Corresponds to `$$$`. */
} CcParsedSideEffectEnum;

#define CC_PARSED_SIDE_EFFECT_ENUM_IS_CASTLING(see) ( (see) == CC_PSEE_Castle )

#define CC_MAX_LEN_PARSED_SIDE_EFFECT_SYMBOL (3)

char const * cc_parsed_side_effect_symbol( CcParsedSideEffectEnum see );


typedef struct CcParsedSideEffect
{
    CcParsedSideEffectEnum type; /* Type of side-effect. */

    union {
        struct {
            CcPieceType piece; /* Piece which has been captured. */
            CcLosingTagEnum lost_tag; /* Flag, whether captured piece has lost its tag. */
        } capture; /* Capture. */

        struct {
            CcPieceType piece; /* Piece which has been displaced. */
            CcLosingTagEnum lost_tag; /* Flag, whether displaced piece has lost its tag. */
            CcPos destination; /* Displacement destination. */
        } displacement; /* Displacement, used during light Shaman's trance-journey. */

        struct {
            CcPieceType pawn; /* Pawn which has been captured. */
            CcPos distant; /* Position at which Pawn has been captured. */
        } en_passant; /* En passant. */

        struct {
            CcPieceType rook; /* Rook which castled. */
            CcPos start; /* Starting position of a Rook. */
            CcPos destination; /* Castling Rook destination. */
        } castle; /* Castling. */

        struct {
            CcPieceType captured; /* Piece which has been captured, if any. */
            CcLosingTagEnum lost_tag; /* Flag, whether captured piece has lost its tag. */
            CcPieceType promoted_to; /* Piece to which Pawn has been promoted. */
        } promote; /* Promotion. */

        struct {
            CcPieceType captured; /* Piece which has been captured, if any. */
            CcLosingTagEnum lost_tag; /* Flag, whether captured piece has lost its tag. */
        } tag_for_promotion; /* Tag for promotion. */

        struct {
            CcPieceType piece; /* Piece which has been converted. */
            CcLosingTagEnum lost_tag; /* Flag, if converted piece has lost its tag. */
        } convert; /* Conversion. */

        struct {
            CcPieceType piece; /* Piece which has been "passed-over". */
        } transparency; /* Transparency. */

        struct {
            CcPieceType piece; /* Piece from which currently moving piece has been diverged. */
        } diversion; /* Divergence. */

        struct {
            CcPieceType piece; /* Piece which has been demoted to Pawn. */
            CcLosingTagEnum lost_tag; /* Flag, whether demoted piece has lost its tag. */
            CcPos distant; /* Position at which piece has been demoted. */
        } demote; /* Demoting. */

        struct {
            CcPieceType piece; /* Piece which has been resurrected. */
            CcPos destination; /* Position at which Wave, Starchild has been resurrected. */
        } resurrect; /* Resurrection. */
    }; /* Union of all substructures used by different step side-effects. */
} CcParsedSideEffect;

CcParsedSideEffect cc_parsed_side_effect( CcParsedSideEffectEnum type,
                                          CcPieceType piece,
                                          CcLosingTagEnum lost_tag,
                                          CcPos start,
                                          CcPos destination,
                                          CcPieceType promoted_to );

CcPieceType cc_parsed_side_effect_piece( CcParsedSideEffect se );

CcPos cc_parsed_side_effect_destination( CcParsedSideEffect se );

bool cc_parsed_side_effect_to_short_str( CcParsedSideEffect se,
                                         cc_char_16 * se_str__o );


CcParsedSideEffect cc_parsed_side_effect_none( void );
CcParsedSideEffect cc_parsed_side_effect_capture( CcPieceType piece, CcLosingTagEnum lost_tag );
CcParsedSideEffect cc_parsed_side_effect_displacement( CcPieceType piece, CcLosingTagEnum lost_tag, CcPos destination );
CcParsedSideEffect cc_parsed_side_effect_en_passant( CcPieceType pawn, CcPos distant );
CcParsedSideEffect cc_parsed_side_effect_castle( CcPieceType rook, CcPos start, CcPos destination );
CcParsedSideEffect cc_parsed_side_effect_promote( CcPieceType captured, CcLosingTagEnum lost_tag, CcPieceType promoted_to );
CcParsedSideEffect cc_parsed_side_effect_tag_for_promotion( CcPieceType captured, CcLosingTagEnum lost_tag );
CcParsedSideEffect cc_parsed_side_effect_convert( CcPieceType piece, CcLosingTagEnum lost_tag );
CcParsedSideEffect cc_parsed_side_effect_failed_conversion( void );
CcParsedSideEffect cc_parsed_side_effect_transparency( CcPieceType piece );
CcParsedSideEffect cc_parsed_side_effect_diversion( CcPieceType piece );
CcParsedSideEffect cc_parsed_side_effect_demote( CcPieceType piece, CcLosingTagEnum lost_tag, CcPos distant );
CcParsedSideEffect cc_parsed_side_effect_resurrect( CcPieceType piece, CcPos destination );
CcParsedSideEffect cc_parsed_side_effect_failed_resurrection( void );


#endif /* __CC_PARSED_SIDE_EFFECT_H__ */
