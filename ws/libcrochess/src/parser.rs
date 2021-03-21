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

    if !move_str.is_ascii_printable() {
        return Err( format!("Non-ASCII char(s) found in notation.") );
    }

    let mut mv = m::Move { origin: move_str.to_owned(),
                           plies: plies, // vec![],
                           status: m::MoveStatus::None };

    return Ok(mv);
}

pub fn parse_plies(move_str: &str, variant: BT, board: &B, flags: &F) -> Result<Vec<Ply>, String> {
    let bytes = move_str.as_bytes();
    let count = bytes.len();

    let mut ply_string: String = "".to_string();
    let mut plies: Vec<Ply> = vec![];

    let mut is_ply_started = true;
    let mut depth = 0;
    let mut index = 0;

    let get_char = | offset: usize | if index + offset < count { bytes[ index + offset ] } else { b' ' };

    while index < count {
        let c: u8 = bytes[ index ];

        if c == b'~' {
            if depth != 0 {
                return Err( format!("Unbalanced '[' and ']' braces, when '~' (activation) is reached, at {}.", index+1) );
            }

            let lt = LT::Activation;
            let ply = pp::parse_ply(ply_string.as_str(), lt, variant, board, flags) ?;
            plies.push(ply);

            ply_string = "".to_string();
            is_ply_started = true;
        }
        else if c == b'|' {
            if depth != 0 {
                return Err( format!("Unbalanced '[' and ']' braces, when '|' (teleportation) is reached, at {}.", index+1) );
            }

            let c_peek = get_char( 1 ); // if index+1 < count { bytes[ index+1 ] } else { b' ' };
            let mut lt = LT::Teleportation;

            if c_peek == b'|' {
                lt = LT::FailedTeleportation;

                // Skip 2nd '|', already used up.
                index += 1;
            };

            let ply = pp::parse_ply(ply_string.as_str(), lt, variant, board, flags) ?;
            plies.push(ply);

            ply_string = "".to_string();
            is_ply_started = true;
        }
        else if c == b'@' {
            if depth != 0 {
                return Err( format!("Unbalanced '[' and ']' braces, when '@' (trance-journey) is reached, at {}.", index+1) );
            }

            let mut c_peek = bytes[ index+1 ];
            let mut lt = LT::TranceJourney;

            if c_peek == b'@' {
                lt = LT::DoubleTranceJourney;

                // Skip 2nd '@', already used up.
                index += 1;

                c_peek = bytes[ index+1 ]; // *itr.peek().unwrap_or(&' ');

                if c_peek == b'@' {
                    lt = LT::FailedTeleportation;

                    // Skip 3rd '@', already used up.
                    index += 1;
                }
            };

            let ply = pp::parse_ply(ply_string.as_str(), lt, variant, board, flags) ?;
            plies.push(ply);

            ply_string = "".to_string();
            is_ply_started = true;
        }
        else if c == b':' {
            let mut c_peek = bytes[ index+1 ];

            if c_peek == b':' {
                if depth != 0 {
                    return Err( format!("Unbalanced '[' and ']' braces, when '::' (pawn-sacrifice) is reached, at {}.", index+1) );
                }

                let mut lt = LT::PawnSacrifice;

                // Skip 2nd ':', already used up.
                index += 1;

                let ply = pp::parse_ply(ply_string.as_str(), lt, variant, board, flags) ?;
                plies.push(ply);

                ply_string = "".to_string();
                is_ply_started = true;
            }
            else {
                ply_string.push(c as char);
            }
        }
        else if c == b'[' {
            if depth != 0 {
                return Err( format!("Unbalanced '[' and ']' braces, when '[' (ply gathering) is reached, found at {}.", index+1) );
            }

            if !is_ply_started {
                return Err( format!("Brace '[' should open only at the beginning of a ply, found at {}.", index+1) );
            }

            depth += 1;
        }
        else if c == b']' {
            if depth != 1 {
                return Err( format!("Unbalanced '[' and ']' braces, when ']' (ply gathering) is reached, found at {}.", index+1) );
            }

            let mut c_peek = bytes[ index+1 ];

            if c_peek != b'~' && c_peek != b'|' && c_peek != b'@' && c_peek != b':' {
                return Err( format!("Brace ']' should close only at the end of a ply, found at {}.", index+1) );
            }
            else if c_peek == b':' {
                c_peek = bytes[ index+2 ];
                if c_peek != b':' {
                    return Err( format!("Brace ']' should close only at the end of a ply, found at {}.", index+1) );
                }
            }

            depth -= 1;
        }
        else if c == b'#' || c == b'+' {
            // Ignore for now, these are move statuses, not related to plies.
            break;
        }
        else {
            if c.is_ascii_printable() {
                ply_string.push(c as char);
                is_ply_started = false;
            }
            else {
                return Err( format!("Non-ASCII char '{}' found, at {}.", c, index+1) );
            }
        }

        index += 1;
    }

    if depth != 0 {
        return Err( format!("Unbalanced '[' and ']' braces, at the end of notation.") );
    }

    return Ok(plies);
}
