// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::piece_type::PieceType as PT;
use crate::field::Field as F;
use crate::side_effects::SideEffect as SE;


#[derive(Debug, Copy, Clone)]
pub enum StepOrder {
    Start,
    Next,
    Distant,
    Destination,
    Listed,
    ListedPiece { piece: PT },

    Index( usize ),
}


impl fmt::Display for StepOrder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StepOrder as SO;

        return match self {
            SO::Start => fmt::Result::Ok(()),
            SO::Next => write!(f, "."),
            SO::Distant => write!(f, ".."),
            SO::Destination => fmt::Result::Ok(()),
            SO::Listed => write!(f, ","),
            SO::ListedPiece { piece } => write!(f, ",{}", piece),

            SO::Index( idx ) => {
                if *idx > 0 {
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
    pub order: StepOrder,
    pub is_tag_lost: bool,
    pub field: F,
    pub side_effect: SE,
}


impl fmt::Display for Step {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}{}{}{}", self.order, if self.is_tag_lost { "==" } else { "" }, self.field, self.side_effect);
    }
}
