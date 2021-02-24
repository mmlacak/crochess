
use crate::piece_type;
use crate::board_type;

#[derive(Debug, Clone)]
pub struct Board {
    pub variant: board_type::BoardType,
    pub chessboard: Box<[ Box<[ piece_type::PieceType ]> ]>,
}
