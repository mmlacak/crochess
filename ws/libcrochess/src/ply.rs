// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::piece_type::PieceType as PT;
use crate::field::Field as F;

// use crate::field as f;


#[derive(Debug, Copy, Clone)]
pub enum PlyType {
    First,
    Activation,
    Teleportation,
    FailedTeleportation,
    TranceJourney,
    DoubleTranceJourney,
    FailedTranceJourney,
    PawnSacrifice,
}


impl fmt::Display for PlyType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            PlyType::First => fmt::Result::Ok(()),
            PlyType::Activation => write!(f, "~"),
            PlyType::Teleportation => write!(f, "|"),
            PlyType::FailedTeleportation => write!(f, "||"),
            PlyType::TranceJourney => write!(f, "@"),
            PlyType::DoubleTranceJourney => write!(f, "@@"),
            PlyType::FailedTranceJourney => write!(f, "@@@"),
            PlyType::PawnSacrifice => write!(f, "::"),
        };
    }
}


#[derive(Debug, Clone)]
pub struct Ply {
    pub ply_type: PlyType,
    pub piece: PT,
    pub steps: Vec<F>,
}


impl fmt::Display for Ply {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.ply_type, self.piece) ?;

        for step in self.steps.iter() {
            write!(f, "{}", step) ?;
        }

        return fmt::Result::Ok(());
    }
}
