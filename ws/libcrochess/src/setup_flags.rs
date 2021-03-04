// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use crate::board_type as bt;
use crate::board_type::BoardType as BT;

// use crate::piece_flag as pf;
use crate::piece_flag::PieceFlag as PF;


pub fn new_setup_rules(board_type: bt::BoardType) -> &'static [ &'static [ PF ] ] {

    use crate::piece_flag::PieceFlag::None as n;
    use crate::piece_flag::PieceFlag::CanRush as R;
    use crate::piece_flag::PieceFlag::CanCastle as C;
    use crate::piece_flag::PieceFlag::TagForPromotion as P;

    fn new_cc_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, C, n, n, C ],
            &[ R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, C, n, n, C ],
        ];
    }

    fn new_ct_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, n, C, n, n, n, C ],
            &[ R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, n, C, n, n, n, C ],
        ];
    }

    fn new_ma_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, n, n, C, n, n, n, n, C ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, n, n, C, n, n, n, n, C ],
        ];
    }

    fn new_aoa_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, n, n, n, C, n, n, n, n, n, C ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, n, n, n, C, n, n, n, n, n, C ],
        ];
    }

    fn new_mv_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C ],
        ];
    }

    fn new_n_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, C, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_hd_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, R, n, n, n, R, n, n, n, n, n, n, R, n, n, n, R, n, n ],
            &[ n, n, n, R, n, R, n, n, n, n, n, n, n, n, R, n, R, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, P, n, P, n, n, n, n, n, n, n, n, P, n, P, n, n, n ],
            &[ n, n, P, n, n, n, P, n, n, n, n, n, n, P, n, n, n, P, n, n ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
            &[ n, C, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_tr_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, n, n, R, n, n, n, R, R, n, n, n, R, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, R, n, R, n, n, R, n, R, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, R, n, R, n, n, R, n, R, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, R, n, n, n, R, R, n, n, n, R, n, n, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_cot_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n ],
            &[ n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n ],
            &[ n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_d_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n ],
            &[ n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n ],
            &[ n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, C, n ],
        ];
    }

    fn new_o_setup() -> &'static [ &'static [ PF ] ] {
        return &[
            &[ n, C, n, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, n, C, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n, n ],
            &[ n, n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n ],
            &[ n, n, n, n, n, R, n, R, n, n, R, n, R, R, n, R, n, n, R, n, R, n, n, n, n, n ],
            &[ n, n, n, n, R, n, n, n, R, R, n, n, R, R, n, n, R, R, n, n, n, R, n, n, n, n ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R ],
            &[ n, C, n, n, n, n, n, n, n, n, n, n, n, C, n, n, n, n, n, n, n, n, n, n, C, n ],
        ];
    }

    return match board_type {
        BT::ClassicalChess => new_cc_setup(),
        BT::CroatianTies => new_ct_setup(),
        BT::MayanAscendancy => new_ma_setup(),
        BT::AgeOfAquarius => new_aoa_setup(),
        BT::MirandasVeil => new_mv_setup(),
        BT::Nineteen => new_n_setup(),
        BT::HemerasDawn => new_hd_setup(),
        BT::TamoanchanRevisited => new_tr_setup(),
        BT::ConquestOfTlalocan => new_cot_setup(),
        BT::Discovery => new_d_setup(),
        BT::One => new_o_setup(),
    };
}
