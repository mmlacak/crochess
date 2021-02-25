
use crate::piece_type as pt;
use crate::piece_type::PieceType as PT;
// use crate::piece_type::PieceType::None as n;
use crate::board_type as bt;
use crate::board_type::BoardType as BT;

#[derive(Debug, Clone)]
pub struct Board {
    pub variant: BT,
    pub chessboard: Box<[ Box<[ PT ]> ]>,
    // pub chessboard: &'static mut[ &'static mut [ PT ] ],
    // pub chessboard: &'static [ &'static mut [ PT ] ],
}

impl Board {

    pub fn new(board_type: bt::BoardType) -> Board {
        fn new_chessboard(board_type: bt::BoardType) -> Box<[ Box<[ PT ]> ]> {

            use crate::piece_type::PieceType::None as n;

            fn new_cc_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                    Box::new( [ n, n, n, n, n, n, n, n ] ),
                ] );
            }

            fn new_ct_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_ma_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_aoa_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_mv_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_n_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_hd_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_tr_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_cot_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_d_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            fn new_o_chessboard() -> Box<[ Box<[ PT ]> ]> {
                return Box::new( [
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
                ] );
            }

            return match board_type {
                BT::ClassicalChess => new_cc_chessboard(),
                BT::CroatianTies => new_cc_chessboard(),
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

        let size = board_type.size();
        let cb = new_chessboard(board_type);
        let b = Board { variant: board_type,
                        chessboard: cb };
        return b;
    }

}
