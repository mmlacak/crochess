// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

// use crate::piece_type as pt;
// use crate::piece_type::PieceType as PT;
// use crate::board_type as bt;
// use crate::board_type::BoardType as BT;
// use crate::board as b


#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
// #[repr(i32)]
pub enum PieceFlag {
    None,
    CanRush, // Pawns
    CanCastle, // Rooks, Kings
    TagForPromotion, // Pawn
}

impl PieceFlag {

    pub fn as_char(&self) -> char {
        return match self {
            PieceFlag::None => ' ',
            PieceFlag::CanRush => 'R',
            PieceFlag::CanCastle => 'C',
            PieceFlag::TagForPromotion => 'P',
        };
    }
}

impl fmt::Display for PieceFlag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", self.as_char());
    }
}
