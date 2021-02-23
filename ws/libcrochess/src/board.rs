
use crate::piece_type;
use crate::board_type;

#[derive(Debug, Clone)]
pub struct Board {
    pub variant: board_type::BoardType,
    board: &'static mut [[ piece_type::PieceType ]],
}
