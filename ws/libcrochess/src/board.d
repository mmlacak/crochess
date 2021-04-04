// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import pt = piece_type;
import bt = board_type;
import ct = chip_type;


export final class Board {
    private bt.BoardType boardType;
    private uint size;

    private immutable uint capacity = bt.size( bt.BoardType.One ); // 26
    private pt.PieceType[ capacity ][ capacity ] board;
    private ct.ChipType[ capacity ][ capacity ] chips;

    @disable this();

    this( bt.BoardType boardType ) {
        this.init( boardType );
    }

    void init( bt.BoardType boardType ) {
        this.boardType = boardType;
        this.size = bt.size( this.boardType );

        clear();
    }

    void clear() {
        for ( uint i = 0; i < this.capacity; ++i ) {
            for ( uint j = 0; j < this.capacity; ++j ) {
                this.board[ i ][ j ] = pt.PieceType.None;
                this.chips[ i ][ j ] = ct.ChipType.None;
            }
        }
    }

    uint getBoardType() const {
        return this.boardType;
    }

    pt.PieceType[ this.capacity ][ this.capacity ] getBoard() const {
        return this.board;
    }

    ct.ChipType[ this.capacity ][ this.capacity ] getBoard() const {
        return this.chips;
    }

    bool isFieldLight( int i, int j ) const {
        return ( (i + j) % 2 == 0 );
    }

    bool isValid( int i ) const {
        return ( 0 <= i && i < this.size );
    }

    bool isValid( int i, int j ) const {
        return ( this.isValid( i ) && this.isValid( j ) );
    }

    pt.PieceType getPiece( int i, int j ) const {
        if ( this.isValid( i, j ) ) {
            return this.board[ i ][ j ];
        }

        return pt.PieceType.None;
    }

    ct.ChipType getChip( int i, int j ) const {
        if ( this.isValid( i, j ) ) {
            return this.chips[ i ][ j ];
        }

        return ct.ChipType.None;
    }

    void setPiece( int i, int j, pt.PieceType piece, ct.ChipType chip = ct.ChipType.None ) {
        if ( this.isValid( i, j ) ) {
            this.board[ i ][ j ] = piece;
            this.chips[ i ][ j ] = chip;
        }
    }

    bool copyFrom( const pt.PieceType[][] board, const ct.ChipType[][] chips = null ) {
        if ( board.length != this.size ) return false;
        if ( chips !is null && chips.length != this.size ) return false;

        for ( uint i = 0; i < this.size; ++i ) {
            if ( board[ i ].length != this.size ) return false;
            if ( chips !is null && ( chips[ i ].length != this.size ) ) return false;
        }

        for ( uint i = 0; i < this.size; ++i ) {
            for ( uint j = 0; j < this.size; ++j ) {
                this.board[ i ][ j ] = board[ i ][ j ];
                this.chips[ i ][ j ] = ( chips !is null ) ? chips[ i ][ j ] : ct.ChipType.None;
            }
        }

        return true;
    }
}
