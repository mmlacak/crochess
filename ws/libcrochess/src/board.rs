// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

// use crate::piece_type as pt;
use crate::piece_type::PieceType as PT;
use crate::board_type as bt;
use crate::board_type::BoardType as BT;

#[derive(Debug, Clone)]
pub struct Chessboard(Box<[ Box<[ PT ]> ]>);

#[derive(Debug, Clone)]
pub struct Board {
    variant: BT,
    // chessboard: Box<[ Box<[ PT ]> ]>,
    chessboard: Chessboard,
}

impl Board {

    pub fn new(board_type: bt::BoardType) -> Board {
        // fn new_chessboard(board_type: bt::BoardType) -> Box<[ Box<[ PT ]> ]> {
        fn new_chessboard(board_type: bt::BoardType) -> Chessboard {

            use crate::piece_type::PieceType::None as n;

            // fn new_cc_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_cc_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_ct_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_ct_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_ma_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_ma_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_aoa_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_aoa_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_mv_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_mv_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_n_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_n_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_hd_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_hd_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_tr_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_tr_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_cot_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_cot_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_d_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_d_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            // fn new_o_chessboard() -> Box<[ Box<[ PT ]> ]> {
            fn new_o_chessboard() -> Chessboard {
                return Chessboard( Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ] ),
                ] ) );
            }

            return match board_type {
                BT::ClassicalChess => new_cc_chessboard(),
                BT::CroatianTies => new_ct_chessboard(),
                BT::MayanAscendancy => new_ma_chessboard(),
                BT::AgeOfAquarius => new_aoa_chessboard(),
                BT::MirandasVeil => new_mv_chessboard(),
                BT::Nineteen => new_n_chessboard(),
                BT::HemerasDawn => new_hd_chessboard(),
                BT::TamoanchanRevisited => new_tr_chessboard(),
                BT::ConquestOfTlalocan => new_cot_chessboard(),
                BT::Discovery => new_d_chessboard(),
                BT::One => new_o_chessboard(),
            };
        }

        let cb = new_chessboard(board_type);
        let b = Board { variant: board_type,
                        chessboard: cb };
        return b;
    }

    pub fn variant(&self) -> BT {
        return self.variant;
    }

    // pub fn chessboard(&self) -> &Box<[ Box<[ PT ]> ]> {
    pub fn chessboard(&self) -> &Chessboard {
        return &self.chessboard;
    }

    pub fn is_on_chessboard(&self, i: i32, j: i32) -> bool {
        // return (0 <= i) && (i <= (*self.chessboard).len() as i32) &&
        //        (0 <= j) && (j <= (*(*self.chessboard)[0]).len() as i32);
        let size: i32 = self.variant.size() as i32;
        return (0 <= i) && (i <= size) &&
               (0 <= j) && (j <= size);
    }

    pub fn piece_at(&self, i: i32, j: i32) -> PT {
        if self.is_on_chessboard(i, j) {
            return self.chessboard.0[i as usize][j as usize];
        }
        else {
            return PT::None;
        }
    }

    pub fn set_piece_at(&mut self, i: i32, j: i32, pt: PT) -> bool {
        if self.is_on_chessboard(i, j) {
            self.chessboard.0[i as usize][j as usize] = pt;
            return true;
        }
        else {
            return false;
        }
    }

}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "({}, {})", self.variant, self.variant);
    }
}
