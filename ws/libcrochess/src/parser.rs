// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use crate::board_type::BoardType as BT;
use crate::board::Board as B;
use crate::flags::Flags as F;

use crate::ply::LinkType as LT;
use crate::ply::Ply;
// use crate::step::Step as S;

use crate::ascii_checks::AsciiPrintable;
use crate::moves as m;
use crate::parse_ply as pp;


pub fn parse(move_str: &str, variant: BT, board: &B, flags: &F) -> Result<m::Move, String> {

    let plies = parse_plies(move_str, variant, board, flags) ?;

    let mut mv = m::Move { origin: move_str.to_owned(),
                           plies: plies, // vec![],
                           status: m::MoveStatus::None };

    return Ok(mv);
}

pub fn parse_plies(move_str: &str, variant: BT, board: &B, flags: &F) -> Result<Vec<Ply>, String> {
    let count = move_str.chars().count();
    let mut depth = 0;
    let mut itr = move_str.chars().peekable();
    let mut ply_string: String = "".to_string();
    let mut plies: Vec<Ply> = vec![];

    for i in 0 .. count {
        let c: char = itr.next().unwrap_or(' ');

        if c == '~' {
            if depth != 0 {
                return Err( format!("Unbalanced '[' and ']' braces, when '~' (activation) is reach, at {}.", i) );
            }

            let lt = LT::Activation;
            let ply = pp::parse_ply(ply_string.as_str(), lt, variant, board, flags) ?;
            plies.push(ply);
            ply_string = "".to_string();
        }
        else if c == '|' {
            if depth != 0 {
                return Err( format!("Unbalanced '[' and ']' braces, when '|' (teleportation) is reach, at {}.", i) );
            }

            let c_peek: char = *itr.peek().unwrap_or(&' ');
            let mut lt = LT::Teleportation;

            if c_peek == '|' {
                lt = LT::FailedTeleportation;

                // Consume 2nd '|', already used up.
                itr.next();
            };

            let ply = pp::parse_ply(ply_string.as_str(), lt, variant, board, flags) ?;
            plies.push(ply);
            ply_string = "".to_string();
        }
        else if c == '@' {
            if depth != 0 {
                return Err( format!("Unbalanced '[' and ']' braces, when '@' (trance-journey) is reach, at {}.", i) );
            }

            let mut c_peek: char = *itr.peek().unwrap_or(&' ');
            let mut lt = LT::TranceJourney;

            if c_peek == '@' {
                lt = LT::DoubleTranceJourney;

                // Consume 2nd '@', already used up.
                itr.next();

                c_peek = *itr.peek().unwrap_or(&' ');

                if c_peek == '@' {
                    lt = LT::FailedTeleportation;

                    // Consume 3rd '@', already used up.
                    itr.next();
                }
            };

            let ply = pp::parse_ply(ply_string.as_str(), lt, variant, board, flags) ?;
            plies.push(ply);
            ply_string = "".to_string();
        }
        else if c == ':' {
            let mut c_peek: char = *itr.peek().unwrap_or(&' ');

            if c_peek == ':' {
                if depth != 0 {
                    return Err( format!("Unbalanced '[' and ']' braces, when '::' (pawn-sacrifice) is reach, at {}.", i) );
                }

                let mut lt = LT::PawnSacrifice;

                // Consume 2nd ':', already used up.
                itr.next();

                let ply = pp::parse_ply(ply_string.as_str(), lt, variant, board, flags) ?;
                plies.push(ply);
                ply_string = "".to_string();
            }
            else {
                ply_string.push(c);
            }
        }
        else if c == '[' {
            depth += 1;
        }
        else if c == ']' {
            depth -= 1;
        }
        else {
            if c.is_ascii_printable() {
                ply_string.push(c);
            }
            else {
                return Err( format!("Non-ASCII char '{}' found, at {}.", c, i) );
            }
        }
    }

    return Ok(plies);
}
