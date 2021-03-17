// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::piece_type::PieceType as PT;
use crate::field::Field as F;


#[derive(Debug, Copy, Clone)]
pub enum SideEffect {
    None,
    Capture { piece: PT, is_tag_lost: bool },
    EnPassant { pawn_field: F },
    Castle { rook_field: F },
    Promote { to_figure: PT },
    TagForPromotion,
    Convert { piece: PT, is_tag_lost: bool },
    FailedConversion,
    DemoteToPawn { piece: PT, field: F },
    Ressurect { piece: PT, field: F },
    FailedRessurection,
    Displace { piece: PT, is_tag_lost: bool, field: F }
}


impl fmt::Display for SideEffect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SideEffect as SE;

        return match self {
            SE::Capture { piece, is_tag_lost } => write!(f, "*{}{}", piece, if piece.is_pawn() && *is_tag_lost { "==" } else { "" } ),
            SE::EnPassant { pawn_field } => write!(f, ":{}", pawn_field.1),
            SE::Castle { rook_field } => write!(f, "&{}", rook_field.0),
            SE::Promote { to_figure } => write!(f, "={}", to_figure),
            SE::TagForPromotion => write!(f, "="),
            SE::Convert { piece, is_tag_lost } => write!(f, "%{}{}", piece, if piece.is_pawn() && *is_tag_lost { "==" } else { "" }),
            SE::FailedConversion => write!(f, "%%"),
            SE::DemoteToPawn { piece, field } => write!(f, ">{}{}", piece, field),
            SE::Ressurect { piece, field } => write!(f, "${}{}", piece, field),
            SE::FailedRessurection => write!(f, "$$"),
            SE::Displace { piece, is_tag_lost, field } => write!(f, "<{}{}{}", piece, if piece.is_pawn() && *is_tag_lost { "==" } else { "" }, field),
            _ => fmt::Result::Ok(()),
        };
    }
}
