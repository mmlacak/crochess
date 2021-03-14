// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

use crate::ply::Ply;


#[derive(Debug, Copy, Clone)]
pub enum MoveStatus {
    None,
    Check,
    Checkmate,
}


impl fmt::Display for MoveStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            MoveStatus::None => fmt::Result::Ok(()),
            MoveStatus::Check => write!(f, "+"),
            MoveStatus::Checkmate => write!(f, "#"),
        };
    }
}


#[derive(Debug, Clone)]
pub struct Move {
    plies: Vec<Ply>,
    status: MoveStatus,
}


impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ply in self.plies.iter() {
            write!(f, "{}", ply) ?;
        }

        write!(f, "{}", self.status) ?;

        return fmt::Result::Ok(());
    }
}
