// Copyright (c) 2022 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#include "cc_setup_misc.h"

#include "cc_checks.h"
#include "cc_parse_utils.h"
#include "cc_parse_side_effect.h"


// static bool cc_parse_side_effect_error_msg( char const * restrict side_effect_an,
//                                             char const * restrict step_end_an,
//                                             CcParseMsg ** restrict parse_msgs__iod,
//                                             char const * restrict fmt, ... ) {
//     char * step_an__a = cc_str_copy__new( side_effect_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );

//     va_list args;
//     va_start( args, fmt );

//     cc_parse_msg_expand_fmt_va( parse_msgs__iod,
//                                       CC_PMTE_Error,
//                                       CC_MAX_LEN_ZERO_TERMINATED,
//                                       fmt,
//                                       args );

//     va_end( args );

//     CC_FREE( step_an__a );

//     return true;
// }


static bool cc_check_piece_has_congruent_type( char piece_symbol,
                                               CcPieceEnum piece,
                                               char const * restrict step_start_an,
                                               char const * restrict step_end_an,
                                               CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !cc_piece_has_congruent_type( piece_symbol, piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, false, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Piece '%c' not found at step-field, encountered %s, in step '%s'.\n", piece_symbol, piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_piece_can_be_captured( CcPieceEnum piece,
                                            char const * restrict step_start_an,
                                            char const * restrict step_end_an,
                                            CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_CAPTURED( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, true, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s at step-field cannot be captured, in step '%s'.\n", piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_piece_symbol_is_valid( char piece_symbol,
                                            char const * restrict step_start_an,
                                            char const * restrict step_end_an,
                                            CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !cc_piece_symbol_is_valid( piece_symbol ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Character '%c' is not valid piece symbol, in step '%s'.\n", piece_symbol, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_promote_to_piece_is_valid( CcPieceEnum promote_to_piece,
                                                char const * restrict step_start_an,
                                                char const * restrict step_end_an,
                                                CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PAWN_CAN_BE_PROMOTED_TO( promote_to_piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( promote_to_piece, false, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Pawn cannot be promoted to %s, in step '%s'.\n", piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_piece_can_be_displaced( CcPieceEnum piece,
                                             char const * restrict step_start_an,
                                             char const * restrict step_end_an,
                                             CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_DISPLACED( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, true, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s at step-field cannot be displaced, in step '%s'.\n", piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_parse_and_check_position( char const * pos_an,
                                         CcPos * pos__o,
                                         char const ** pos_end_an__o,
                                         char const * restrict msg_fmt,
                                         char const * restrict step_start_an,
                                         char const * restrict step_end_an,
                                         CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !cc_parse_pos( pos_an, pos__o, pos_end_an__o ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_position_is_on_board( CcPos pos,
                                           CcChessboard * restrict cb,
                                           char const * msg_fmt,
                                           char const * restrict step_start_an,
                                           char const * restrict step_end_an,
                                           CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !cc_chessboard_is_pos_on_board( cb, pos.i, pos.j ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_piece_en_passant( CcPieceEnum piece,
                                       bool is_capturing,
                                       char const * restrict msg_fmt,
                                       char const * restrict step_start_an,
                                       char const * restrict step_end_an,
                                       CcParseMsg ** restrict parse_msgs__iod ) {
    bool is_en_passant = is_capturing ? CC_PIECE_CAN_CAPTURE_EN_PASSANT( piece )
                                      : CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( piece );

    if ( !is_en_passant ) {
        char const * piece_str = cc_piece_as_string( piece, false, true );
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_en_passant_file( CcPos en_passant_location,
                                      CcChessboard * restrict cb,
                                      char const * restrict step_start_an,
                                      char const * restrict step_end_an,
                                      CcParseMsg ** restrict parse_msgs__iod ) {
    if ( ( !cc_chessboard_is_coord_on_board( cb, en_passant_location.j ) )
         || ( CC_IS_COORD_VALID( en_passant_location.i )
              && ( !cc_chessboard_is_coord_on_board( cb, en_passant_location.i ) ) ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "En passant location, if given, has to be on chessboard, in step '%s'.\n", step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_captured_en_passant( CcPieceEnum capturing,
                                          CcPos step,
                                          CcPos en_passant_location,
                                          CcPieceEnum maybe_captured,
                                          CcPos * restrict captured_at__o,
                                          CcChessboard * restrict cb,
                                          char const * restrict step_start_an,
                                          char const * restrict step_end_an,
                                          CcParseMsg ** restrict parse_msgs__iod ) {
    int stepping = cc_piece_is_light( capturing ) ? -1 : 1; // Rank direction, where to search for captured private.
    int max_rank = cb->size / 2;

    CcPos captured_at = CC_POS_CAST_INVALID;
    CcPieceEnum captured = CC_PE_None;

    for ( int rank = step.j; rank <= max_rank; rank += stepping ) {
        CcPieceEnum pe = cc_chessboard_get_piece( cb, step.i, rank );

        if ( CC_PIECE_IS_NONE( pe ) ) continue;

        if ( rank == step.j ) { // Step destination must be empty.
            char const * piece_str = cc_piece_as_string( pe, false, true );
            char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
            cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "En passant destination must be empty, encountered %s in step '%s'.\n", piece_str, step_an__a );
            CC_FREE( step_an__a );
            return false;
        }

        if ( !CC_PIECE_CAN_BE_CAPTURED_EN_PASSANT( pe ) ) {
            char const * piece_str = cc_piece_as_string( pe, false, true );
            char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
            cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Only Pawns, Scouts, Grenadiers can be captured en passant, encountered %s in step '%s'.\n", piece_str, step_an__a );
            CC_FREE( step_an__a );
            return false;
        }

        CcTagEnum te = cc_chessboard_get_tag( cb, step.i, rank );

        if ( !CC_TAG_CAN_EN_PASSANT( te ) ) {
            char const * piece_str = cc_piece_as_string( pe, false, true );
            cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Private %s has not been rushed in the very previous move.\n", piece_str );
            return false;
        }

        captured_at = CC_POS_CAST( step.i, rank );
        captured = pe;
        break;
    }

    if ( CC_PIECE_IS_NONE( captured ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Could not find piece captured en passant, in step '%s'.\n", step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    if ( !cc_chessboard_is_pos_on_board( cb, captured_at.i, captured_at.j ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Could not find location where piece was captured en passant, in step '%s'.\n", step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    if ( !CC_PIECE_IS_NONE( maybe_captured ) ) {
        if ( !CC_PIECE_IS_THE_SAME( maybe_captured, captured ) ) {
            char const * maybe_str = cc_piece_as_string( maybe_captured, false, true );
            char const * captured_str = cc_piece_as_string( captured, true, true );

            char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
            cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s should have been captured en passant, encountered %s, in step '%s'.\n", maybe_str, captured_str, step_an__a );
            CC_FREE( step_an__a );
            return false;
        }
    }

    if ( ( captured_at.j != en_passant_location.j )
         || ( CC_IS_COORD_VALID( en_passant_location.i )
              && ( captured_at.i != en_passant_location.i ) ) ) {
        cc_char_8 step_c8 = CC_CHAR_8_EMPTY;
        if ( !cc_pos_to_short_string( step, &step_c8 ) ) return false;

        cc_char_8 epl_c8 = CC_CHAR_8_EMPTY;
        if ( !cc_pos_to_short_string( en_passant_location, &epl_c8 ) ) return false;

        char const * capturing_str = cc_piece_as_string( capturing, false, true );
        char const * captured_str = cc_piece_as_string( captured, true, true );

        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s at '%s' to be captured en passant has to be on the same file (or field, if given) as ply destination '%s' of capturing %s, in step '%s'.\n", captured_str, epl_c8, step_c8, capturing_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    *captured_at__o = captured_at;
    return true;
}


static bool cc_check_field_is_empty( CcPieceEnum piece,
                                     char const * restrict msg_fmt,
                                     char const * restrict step_start_an,
                                     char const * restrict step_end_an,
                                     CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_IS_NONE( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, false, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_promoting_piece_is_pawn( CcPieceEnum piece,
                                              char const * restrict msg_fmt,
                                              char const * restrict step_start_an,
                                              char const * restrict step_end_an,
                                              CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_IS_PAWN( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, false, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, msg_fmt, piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_piece_can_be_converted( CcPieceEnum piece,
                                             char const * restrict step_start_an,
                                             char const * restrict step_end_an,
                                             CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_CONVERTED( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, true, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s can't be converted, in step '%s'.\n", piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_failed_conversion( CcPieceEnum piece,
                                        char const * restrict step_start_an,
                                        char const * restrict step_end_an,
                                        CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_IS_STARCHILD( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, false, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Conversion can fail only againt Starchild, encountered %s in step '%s'.\n", piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_piece_can_be_resurrected( CcPieceEnum piece,
                                               char const * restrict step_start_an,
                                               char const * restrict step_end_an,
                                               CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_CAN_BE_RESURRECTED( piece ) ) {
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        char const * piece_str = cc_piece_as_string( piece, true, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot be resurrected, in step '%s'.\n", piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_piece_is_castling_king( CcPosPieceTag before_ply_start,
                                             char const * restrict step_start_an,
                                             char const * restrict step_end_an,
                                             CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_IS_KING( before_ply_start.piece ) ) {
        char const * piece_str = cc_piece_as_string( before_ply_start.piece, false, true );
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Only Kings can initiate castling, encountered %s in step '%s'.\n", piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    if ( !CC_TAG_CAN_CASTLE( before_ply_start.tag ) ) {
        char const * piece_str = cc_piece_as_string( before_ply_start.piece, true, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot castle anymore (has lost its castling tag).\n", piece_str );
        return false;
    }

    return true;
}

static bool cc_check_piece_is_rook_to_castle( CcPieceEnum piece,
                                              char const * restrict step_start_an,
                                              char const * restrict step_end_an,
                                              CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !CC_PIECE_IS_ROOK( piece ) ) {
        char const * piece_str = cc_piece_as_string( piece, false, true );
        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Only Rooks can castle with King, encountered %s in step '%s'.\n", piece_str, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    return true;
}

static bool cc_check_king_and_rook_can_castle( CcPosPieceTag before_ply_start,
                                               CcChessboard * restrict cb,
                                               CcPos * restrict step_pos__io,
                                               CcPos * restrict rook_dest__io,
                                               CcPieceEnum * restrict rook__o,
                                               CcPos * restrict rook_init__o,
                                               char const * restrict step_start_an,
                                               char const * restrict step_end_an,
                                               CcParseMsg ** restrict parse_msgs__iod ) {
    bool is_light = cc_piece_is_light( before_ply_start.piece );
    int init_i = cc_get_figure_initial_file( cb->type, before_ply_start.piece, false );
    int init_j = cc_get_initial_figure_rank( cb->type, is_light );

    if ( before_ply_start.pos.i != init_i || before_ply_start.pos.j != init_j ) {
        char const * piece_str = cc_piece_as_string( before_ply_start.piece, true, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot castle anymore (was moved out of its initial position).\n", piece_str );
        return false;
    }

    if ( CC_IS_COORD_VALID( step_pos__io->j ) ) {
        if ( step_pos__io->j != init_j ) {
            char const * piece_str = cc_piece_as_string( before_ply_start.piece, true, true );
            char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
            cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s can castle only on its inital rank, not rank %d in step '%s'.\n", piece_str, step_pos__io->j+1, step_an__a );
            CC_FREE( step_an__a );
            return false;
        }
    } else
        step_pos__io->j = init_j; // King can also have just a file for castling destination; parsed rank is then invalid.

    bool is_queen_side = false;
    int min_i = CC_INVALID_COORD;
    int max_i = CC_INVALID_COORD;

    if ( !cc_check_pos_is_king_castling_step( cb->type, before_ply_start.piece, step_pos__io->i, step_pos__io->j, &is_queen_side, &min_i, &max_i ) ) {
        char const * piece_str = cc_piece_as_string( before_ply_start.piece, true, true );

        cc_char_8 pos_c8 = CC_CHAR_8_EMPTY;
        if ( !cc_pos_to_short_string( *step_pos__io, &pos_c8 ) ) return false;

        char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot castle onto field %s (out of bounds), in step '%s'.\n", piece_str, pos_c8, step_an__a );
        CC_FREE( step_an__a );
        return false;
    }

    CcPos step = is_queen_side ? cc_pos( -1, 0 ) : cc_pos( 1, 0 );
    CcPieceEnum rook = is_light ? CC_PE_LightRook : CC_PE_DarkRook;
    int rook_i = cc_get_figure_initial_file( cb->type, rook, is_queen_side );
    CcPos pos = cc_pos_add( before_ply_start.pos, step, 1 ); // First step from King's initial position.
    int limit = is_queen_side ? pos.i - rook_i : rook_i - pos.i;

    if ( !cc_check_step_fields_are_empty( cb, pos, step, limit ) ) {
        char const * piece_str = cc_piece_as_string( before_ply_start.piece, true, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot castle, all step-fields between the King and a Rook has to be empty.\n", piece_str );
        return false;
    }

    CcPieceEnum maybe_rook = cc_chessboard_get_piece( cb, rook_i, init_j );

    if ( maybe_rook != rook ) {
        char const * rook_str = cc_piece_as_string( rook, true, true );
        char const * piece_str = cc_piece_as_string( maybe_rook, false, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s not found on its initial position, encountered %s.\n", rook_str, piece_str );
        return false;
    }

    CcTagEnum maybe_tag = cc_chessboard_get_tag( cb, rook_i, init_j );

    if ( !CC_TAG_CAN_CASTLE( maybe_tag ) ) {
        char const * piece_str = cc_piece_as_string( rook, true, true );
        cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "%s cannot castle anymore (has lost its castling tag).\n", piece_str );
        return false;
    }

    int rook_end_i = is_queen_side ? step_pos__io->i + 1 : step_pos__io->i - 1;
    CcPos rook_end = cc_pos( rook_end_i, init_j );

    if ( cc_pos_is_disambiguation( *rook_dest__io ) ) {
        if ( !cc_pos_is_congruent( rook_end, *rook_dest__io ) ) {
            char const * piece_str = cc_piece_as_string( rook, false, true );
            char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
            cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, "Destination of %s is not valid (next to castling King, on the inner side), in step '%s'.\n", piece_str, step_an__a );
            CC_FREE( step_an__a );
            return false;
        }
    }

    *rook_dest__io = rook_end;
    *rook__o = rook;
    *rook_init__o = cc_pos( rook_i, init_j );

    return true;

    // TODO :: for variants < Nineteen, check all King's step-fields are not under attack, i.e. between min_i and max_i

    // TODO :: for variants >= Nineteen, check King's destination field is not under attack, i.e. step_pos__io->i
}


bool cc_parse_side_effect( char const * restrict side_effect_an,
                           char const * restrict step_start_an,
                           char const * restrict step_end_an,
                           CcGame * restrict game,
                           CcPosPieceTag before_ply_start,
                           CcChessboard * restrict cb,
                           CcStepLinkEnum sle,
                           CcPos * restrict step_pos__io,
                           CcSideEffect * restrict side_effect__o,
                           CcParseMsg ** restrict parse_msgs__iod ) {
    if ( !side_effect_an ) return false;
    if ( !step_start_an ) return false;
    if ( !step_end_an ) return false;
    if ( !game ) return false;
    if ( !cb ) return false;
    if ( !step_pos__io ) return false;
    if ( !side_effect__o ) return false;
    if ( !parse_msgs__iod ) return false;

    // Just sanity checks.
    if ( sle == CC_SLE_None ) return false;

    if ( !game->chessboard ) return false;
    if ( cb->type != game->chessboard->type ) return false;
    if ( cb->size != game->chessboard->size ) return false;

    if ( !cc_chessboard_is_pos_on_board( cb, step_pos__io->i, step_pos__io->j ) )
        return false;

    CcPieceEnum step_piece = cc_chessboard_get_piece( cb, step_pos__io->i, step_pos__io->j );
    bool has_promotion_sign = false;
    CcSideEffectEnum see = cc_parse_side_effect_type( side_effect_an, &has_promotion_sign );
    char const * se_an = side_effect_an + cc_side_effect_type_len( see, has_promotion_sign );

    switch ( see ) {
        case CC_SEE_None : {

            // TODO :: default interactions
            //
            //      -- if King is moving horizontally, for 2+ fields, it's castling
            //      -- if Pawn, Scout, or Grenadier is moving diagonally, onto empty field, it's en passant
            //      -- if Pyramid is moving onto own Pawn, on a field on opponent's side
            //         of a chessboard, it's tagging for promotion
            //      -- Starchild moving onto empty field, in a syzygy, it's failed resurrection --> ignore (?)
            //      -- otherwise, it's capture
            //      -- if it's a capture made by Pawn, check if it's also a tag for promotion;
            //         it can't be promotion, if promote-to piece is missing

            if ( CC_PIECE_IS_NONE( step_piece ) ) {
                *side_effect__o = cc_side_effect_none();
                return true;
            } else {
                if ( sle == CC_SLE_Start ) {
                    // Starting position, piece is the one found in destination of last ply, or the one starting a move.

                    // TODO :: too early for this :: UNCOMMENT when before_ply_start.piece is valid
                    //
                    // if ( step_piece != before_ply_start.piece ) // TODO :: piece starting a move
                    // {
                    //     char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                    //     char sp = cc_piece_as_char( step_piece );
                    //     char lpdp = cc_piece_as_char( before_ply_start.piece );

                    //     cc_parse_msg_expand_fmt( parse_msgs__iod,
                    //                                 CC_PMTE_Error,
                    //                                 CC_MAX_LEN_ZERO_TERMINATED,
                    //                                 "Piece '%c' found at step-field, in step '%s'; expected '%c'.\n",
                    //                                 sp,
                    //                                 step_an__a,
                    //                                 lpdp );
                    //     CC_FREE( step_an__a );
                    //     return false;
                    // }
                    //
                    // TODO :: too early for this :: UNCOMMENT when before_ply_start.piece is valid

                    *side_effect__o = cc_side_effect_none();
                    return true;
                } else {
                    // TODO :: silent capture ::
                    //      -- if piece found on step-field has other owner
                    //      -- if ply piece (the one currently moving) can capture
                    //      -- if piece found on step-field can be captured
                    //      --> then it's silent capture (!)

                    // TODO ::  >>> TODO >>> Piece '%c' found at step-field, should be empty, in step '%s'.\n
                    char * step_an__a = cc_str_copy__new( step_start_an, step_end_an, CC_MAX_LEN_ZERO_TERMINATED );
                    char sp = cc_piece_as_char( step_piece );
                    cc_parse_msg_expand_fmt( parse_msgs__iod, CC_PMTE_Error, CC_MAX_LEN_ZERO_TERMINATED, " >>> TODO >>> Piece '%c' found at step-field, should be empty, in step '%s'.\n", sp, step_an__a );
                    CC_FREE( step_an__a );
                    return false;
                }
            }
            // TODO
        } case CC_SEE_Capture : {
            // TODO
            //
            // -- moving promotion :: if it's a capture made by Pawn, check if it's also a promotion

            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) ) {
                if ( !cc_check_piece_has_congruent_type( piece_symbol, step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            if ( !cc_check_piece_can_be_captured( step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );
            char const * promo_an = se_an + cc_losing_tag_len( lte );

            if ( CC_PIECE_IS_PAWN( before_ply_start.piece ) ) {
                bool has_promo_sign = false;
                CcSideEffectEnum promo = cc_parse_side_effect_type( promo_an, &has_promo_sign );

                if ( promo == CC_SEE_Promotion ) {
                    promo_an += cc_side_effect_type_len( promo, has_promo_sign );

                    char promote_to_symbol = ' ';

                    if ( !cc_fetch_piece_symbol( promo_an, &promote_to_symbol, true, false ) )
                        return false;

                    if ( !cc_check_piece_symbol_is_valid( promote_to_symbol, step_start_an, step_end_an, parse_msgs__iod ) )
                        return false;

                    bool is_light = cc_piece_is_light( step_piece );
                    CcPieceEnum promote_to = cc_piece_from_symbol( promote_to_symbol, is_light );

                    if ( !cc_check_promote_to_piece_is_valid( promote_to, step_start_an, step_end_an, parse_msgs__iod ) )
                        return false;

                    *side_effect__o = cc_side_effect_promote( step_piece, lte, promote_to );
                    return true;
                } else if ( promo == CC_SEE_TagForPromotion ) {
                    // TODO :: add flag
                }
            }

            *side_effect__o = cc_side_effect_capture( step_piece, lte );
            return true;
        } case CC_SEE_Displacement : {
            // TODO -- add Serpent
            //      -- check light entranced Shaman

            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) ) {
                if ( !cc_check_piece_has_congruent_type( piece_symbol, step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            if ( !cc_check_piece_can_be_displaced( step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );
            char const * pos_an = se_an + cc_losing_tag_len( lte );

            CcPos pos = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( !cc_parse_and_check_position( pos_an, &pos, &pos_end_an, "Error parsing displacement destination, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            if ( !cc_check_position_is_on_board( pos, cb, "Displacement destination has to be complete (not a disambiguation), in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_displacement( step_piece, lte, pos );
            return true;
        } case CC_SEE_EnPassant : {
            if ( !cc_check_piece_en_passant( before_ply_start.piece, true, "Only Pawns, Scouts, Grenadiers can capture en passant, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            if ( !cc_check_field_is_empty( step_piece, "Capturing by en passant can be performed only on an empty field, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            char piece_symbol = ' ';
            CcPieceEnum maybe_captured = CC_PE_None;

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, false, true ) ) {
                bool is_light = !cc_piece_is_light( before_ply_start.piece ); // !light because capturing opponent's piece.
                CcPieceEnum maybe_private = cc_piece_from_symbol( piece_symbol, is_light );

                if ( !cc_check_piece_en_passant( maybe_private, false, "Only Pawns, Scouts, Grenadiers can be captured en passant, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                maybe_captured = maybe_private;
                ++se_an;
            }

            CcPos maybe_en_passant_location = CC_POS_CAST_INVALID;
            char const * pos_end_an = NULL;

            if ( !cc_parse_and_check_position( se_an, &maybe_en_passant_location, &pos_end_an, "Error parsing en passant location, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            if ( CC_IS_COORD_VALID( maybe_en_passant_location.j ) ) { // If location is given, at least rank must be valid.
                if ( !cc_check_en_passant_file( maybe_en_passant_location, cb, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;
            }

            CcPos captured_at = CC_POS_CAST_INVALID;

            if ( !cc_check_captured_en_passant( before_ply_start.piece, *step_pos__io, maybe_en_passant_location, maybe_captured, &captured_at, cb, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_en_passant( step_piece, captured_at );
            return false; // TODO :: en passant
        } case CC_SEE_Castle : {
            if ( !cc_check_piece_is_castling_king( before_ply_start, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, false, true ) ) {
                bool is_light = cc_piece_is_light( before_ply_start.piece );
                CcPieceEnum maybe_rook = cc_piece_from_symbol( piece_symbol, is_light );

                if ( !cc_check_piece_is_rook_to_castle( maybe_rook, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            CcPos rook_dest = CC_POS_CAST_INVALID;
            char const * end = NULL;

            if ( cc_parse_pos( se_an, &rook_dest, &end ) )
                se_an = end;

            CcPieceEnum rook = CC_PE_None;
            CcPos rook_start = CC_POS_CAST_INVALID;

            if ( !cc_check_king_and_rook_can_castle( before_ply_start, cb, step_pos__io, &rook_dest, &rook, &rook_start, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_castle( rook, rook_start, rook_dest );
            return true;
        } case CC_SEE_Promotion : {
            // TODO -- static promotion
            //      -- moving promotion
            //      -- silent capture before promotion

            if ( !cc_check_promoting_piece_is_pawn( step_piece, "Only Pawn can be promoted, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            char piece_symbol = ' ';

            if ( !cc_fetch_piece_symbol( se_an, &piece_symbol, true, false ) )
                return false;

            if ( !cc_check_piece_symbol_is_valid( piece_symbol, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            bool is_light = cc_piece_is_light( step_piece );
            CcPieceEnum promote_to = cc_piece_from_symbol( piece_symbol, is_light );

            if ( !cc_check_promote_to_piece_is_valid( promote_to, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_promote( CC_PE_None, CC_LTE_None, step_piece );
            return true;
        } case CC_SEE_TagForPromotion : {
            // TODO -- silent capture before promotion

            if ( !cc_check_promoting_piece_is_pawn( step_piece, "Only Pawn can be tagged for promotion, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_tag_for_promotion( CC_PE_None, CC_LTE_None );
            return true;
        } case CC_SEE_Conversion : {
            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, true, true ) ) {
                if ( !cc_check_piece_has_congruent_type( piece_symbol, step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            if ( !cc_check_piece_can_be_converted( step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            CcPieceEnum convert_to = cc_piece_opposite( step_piece );
            CcLosingTagEnum lte = cc_parse_losing_tag( se_an );

            *side_effect__o = cc_side_effect_convert( convert_to, lte );
            return true;
        } case CC_SEE_FailedConversion : {
            if ( !cc_check_failed_conversion( step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            *side_effect__o = cc_side_effect_failed_conversion();
            return true;
        } case CC_SEE_Transparency : { // Intentional fall-through ...
        } case CC_SEE_Divergence : {
            char piece_symbol = ' ';

            if ( cc_fetch_piece_symbol( se_an, &piece_symbol, false, true ) ) {
                if ( !cc_check_piece_has_congruent_type( piece_symbol, step_piece, step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                ++se_an;
            }

            if ( see == CC_SEE_Transparency )
                *side_effect__o = cc_side_effect_transparency( step_piece );
            else if ( see == CC_SEE_Divergence )
                *side_effect__o = cc_side_effect_diversion( step_piece );
            else
                return false; // In case some other, unexpected side-effect gets here.

            return true;
        } case CC_SEE_DemoteToPawn : {
            // TODO :: demote to Pawn
            return false;
        } case CC_SEE_Resurrection : { // Intentional fall-through ...
        } case CC_SEE_ResurrectingOpponent : {
            if ( !cc_check_field_is_empty( step_piece, "Resurrection can be performed only on an empty field, encountered %s in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            char piece_symbol = ' ';

            if ( !cc_fetch_piece_symbol( se_an, &piece_symbol, true, false ) )
                return false;

            if ( !cc_check_piece_symbol_is_valid( piece_symbol, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            bool is_light = true;

            if ( CC_GAME_STATUS_IS_LIGHT_TURN( game->status ) )
                is_light = ( see == CC_SEE_Resurrection );
            else if ( CC_GAME_STATUS_IS_DARK_TURN( game->status ) )
                is_light = ( see == CC_SEE_ResurrectingOpponent );
            else
                return false; // Should check status, within caller stack.

            CcPieceEnum resurrecting = cc_piece_from_symbol( piece_symbol, is_light );

            if ( !cc_check_piece_can_be_resurrected( resurrecting, step_start_an, step_end_an, parse_msgs__iod ) )
                return false;

            CcPos pos = CC_POS_CAST_INVALID;

            if ( CC_PIECE_IS_WAVE( resurrecting ) || CC_PIECE_IS_STARCHILD( resurrecting ) ) {
                char const * pos_an = se_an + 1;
                char const * pos_end_an = NULL;

                if ( !cc_parse_and_check_position( pos_an, &pos, &pos_end_an, "Error parsing resurrection destination, in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;

                if ( !cc_check_position_is_on_board( pos, cb, "Resurrection destination has to be complete (not a disambiguation), in step '%s'.\n", step_start_an, step_end_an, parse_msgs__iod ) )
                    return false;
            }

            *side_effect__o = cc_side_effect_resurrect( step_piece, pos );
            return true;
        } case CC_SEE_FailedResurrection : {
            *side_effect__o = cc_side_effect_failed_resurrection();
            return true;
        }

        default : return false;
    }
}
