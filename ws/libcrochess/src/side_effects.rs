// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::piece_type::PieceType as PT;
use crate::field::Field as F;


#[derive(Debug, Copy, Clone)]
pub enum SideEffect {
    None,
    Capture { piece: PT },
    EnPassant { pawn_field: F },
    Castle { rook_field: F },
    Promote { to_figure: PT },
    Convert { piece: PT },
    FailedConversion,
    DemoteToPawn { piece: PT, field: F },
    Ressurect { piece: PT, field: F },
    FailedRessurection,
    Displace { piece: PT, field: F }
}


impl fmt::Display for SideEffect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SideEffect as SE;

        return match self {
            SE::Capture { piece } => write!(f, "*{}", piece),
            SE::EnPassant { pawn_field } => write!(f, ":{}", pawn_field.1),
            SE::Castle { rook_field } => write!(f, "&{}", rook_field.0),
            SE::Promote { to_figure } => {
                if to_figure.is_none() {
                    write!(f, "=")
                }
                else {
                    write!(f, "={}", to_figure)
                }
            },
            SE::Convert { piece } => write!(f, "%{}", piece),
            SE::FailedConversion => write!(f, "%%"),
            SE::DemoteToPawn { piece, field } => write!(f, ">{}{}", piece, field),
            SE::Ressurect { piece, field } => write!(f, "${}{}", piece, field),
            SE::FailedRessurection => write!(f, "$$"),
            SE::Displace { piece, field } => write!(f, "<{}{}", piece, field),
            _ => fmt::Result::Ok(()),
        };
    }
}
