// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::piece_type::PieceType as PT;
use crate::field::Field as F;

// use crate::field as f;


#[derive(Debug, Copy, Clone)]
pub enum PlyType {
    Chained,
    Teleportation,
    FailedTeleportation,
    TranceJourney,
    DoubleTranceJourney,
    FailedTranceJourney,
    PawnSacrifice,
}


#[derive(Debug, Clone)]
pub struct Ply {
    pub ply_type: PlyType,
    pub piece: PT,
    pub steps: Vec<F>,
}


impl fmt::Display for Ply {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ply_type {
            PlyType::Chained => write!(f, "~") ?,
            PlyType::Teleportation => write!(f, "|") ?,
            PlyType::FailedTeleportation => write!(f, "||") ?,
            PlyType::TranceJourney => write!(f, "@") ?,
            PlyType::DoubleTranceJourney => write!(f, "@@") ?,
            PlyType::FailedTranceJourney => write!(f, "@@@") ?,
            PlyType::PawnSacrifice => write!(f, "::") ?,
        };

        write!(f, "{}", self.piece) ?;

        for step in self.steps.iter() {
            write!(f, "{}", step) ?;
        }

        return fmt::Result::Ok(());
    }
}
