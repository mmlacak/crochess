// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::piece_type::PieceType as PT;
use crate::field::Field as F;
use crate::side_effects::SideEffect as SE;


#[derive(Debug, Copy, Clone)]
pub enum StepOrder {
    First,
    Next,
    Distant,
    // Last,

    Index( usize ),
}


impl fmt::Display for StepOrder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StepOrder as SO;

        return match self {
            SO::First => fmt::Result::Ok(()),
            SO::Next => write!(f, "."),
            SO::Distant => write!(f, ".."),

            SO::Index( idx ) => {
                if idx > 0 {
                    write!(f, ".")
                }
                else {
                    fmt::Result::Ok(())
                }
            },
        };
    }
}


#[derive(Debug, Copy, Clone)]
pub struct Step {
    pub order: StepOrder;
    pub piece: PT,
    pub field: F,
    pub side_effect: SE,
}


impl fmt::Display for Step {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}{}{}{}", self.order, self.piece, self.field, self.side_effect);
    }
}
