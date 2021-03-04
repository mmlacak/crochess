// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::board_type as bt;
use crate::board_type::BoardType as BT;

use crate::board as b;

// use crate::piece_flag as pf;
use crate::piece_flag::PieceFlag as PF;


#[derive(Debug, Clone)]
pub struct Flags(pub Box<[ Box<[ PF ]> ]>);


pub fn new_flags(board_type: bt::BoardType) -> Flags {

    use crate::piece_flag::PieceFlag::None as n;
    // use crate::piece_flag::PieceFlag::CanRush as R;
    // use crate::piece_flag::PieceFlag::CanCastle as C;
    // use crate::piece_flag::PieceFlag::TagForPromotion as P;

    fn new_cc_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_ct_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_ma_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_aoa_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_mv_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_n_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_hd_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_tr_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_cot_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_d_flags() -> Flags {
        return Flags( Box::new( [
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

    fn new_o_flags() -> Flags {
        return Flags( Box::new( [
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
        BT::ClassicalChess => new_cc_flags(),
        BT::CroatianTies => new_ct_flags(),
        BT::MayanAscendancy => new_ma_flags(),
        BT::AgeOfAquarius => new_aoa_flags(),
        BT::MirandasVeil => new_mv_flags(),
        BT::Nineteen => new_n_flags(),
        BT::HemerasDawn => new_hd_flags(),
        BT::TamoanchanRevisited => new_tr_flags(),
        BT::ConquestOfTlalocan => new_cot_flags(),
        BT::Discovery => new_d_flags(),
        BT::One => new_o_flags(),
    };
}


impl fmt::Display for Flags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let size = self.0.len();
        let len = 2 * size + 1;
        let divider = "-".to_string().repeat(len);
        let mut files = "".to_string();

        // Ranges don't include upper bound; '{'  is char positioned immediately after 'z' in ASCII table.
        for (i, c) in ('a' .. '{').enumerate() {
            if i >= size { break; }
            files += format!(" {}", c).as_str();
        }

        write!(f, "    {}\n", files) ?;
        write!(f, "    {}\n", divider) ?;

        for i in 0 .. size {
            let row = size - i;
            write!(f, "{:2} |", row) ?;

            for j in 0 .. size {
                let p = self.0[ j ][ size - i - 1 ];
                if p != PF::None {
                    write!(f, " {}", p) ?;
                }
                else {
                    if b::is_field_light(i as i32, j as i32) {
                        write!(f, " .") ?;
                    }
                    else {
                        write!(f, " ,") ?;
                    }
                }
            }

            write!(f, " | {:2}\n", row) ?;
        }

        write!(f, "    {}\n", divider) ?;
        write!(f, "    {}", files) ?;

        return Ok(());
    }
}
