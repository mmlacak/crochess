// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::piece_type::PieceType as PT;
use crate::field::Field as F;

// use crate::field as f;


#[derive(Debug, Clone)]
pub struct Ply {
    pub piece: PT,
    pub steps: Vec<F>,
}


impl fmt::Display for Ply {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.piece) ?;

        for step in self.steps.iter() {
            write!(f, "{}", step) ?;
        }

        return fmt::Result::Ok(());
    }
}
