// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use crate::board_type::BoardType as BT;
use crate::board::Board as B;
use crate::flags::Flags as F;

use crate::ply::LinkType as LT;
use crate::ply::Ply;
use crate::step::Step as S;


pub fn parse_ply(ply_str: &str, link_type: LT, variant: BT, board: &B, flags: &F) -> Result<Ply, &'static str> {

    use crate::piece_type::PieceType as PT;

    let mut ply = Ply { piece: PT::None, steps: vec![], link: link_type };

    return Ok(ply);
}
