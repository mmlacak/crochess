// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::piece_type::PieceType as PT;
use crate::field::Field as F;

// use crate::field as f;


#[derive(Debug, Copy, Clone)]
pub enum LinkType {
    Activation,
    Teleportation,
    FailedTeleportation,
    TranceJourney,
    DoubleTranceJourney,
    FailedTranceJourney,
    PawnSacrifice,
    Last,
}


impl fmt::Display for LinkType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            LinkType::Activation => write!(f, "~"),
            LinkType::Teleportation => write!(f, "|"),
            LinkType::FailedTeleportation => write!(f, "||"),
            LinkType::TranceJourney => write!(f, "@"),
            LinkType::DoubleTranceJourney => write!(f, "@@"),
            LinkType::FailedTranceJourney => write!(f, "@@@"),
            LinkType::PawnSacrifice => write!(f, "::"),
            LinkType::Last => fmt::Result::Ok(()),
        };
    }
}


#[derive(Debug, Clone)]
pub struct Ply {
    pub piece: PT,
    pub steps: Vec<F>,
    pub link: LinkType,
}


impl fmt::Display for Ply {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.link, self.piece) ?;

        for step in self.steps.iter() {
            write!(f, "{}", step) ?;
        }

        return fmt::Result::Ok(());
    }
}
