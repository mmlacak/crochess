// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


export enum ChipType {
    None = 0,
    CanRush, // Pawns
    CanCastle, // Rooks, Kings
    TagForPromotion, // Pawn
}


export char as_char( ChipType ct ) {
    final switch ( ct ) {
        case ChipType.None: return ' ';
        case ChipType.CanRush: return 'R';
        case ChipType.CanCastle: return 'C';
        case ChipType.TagForPromotion: return 'P';
    }
}
