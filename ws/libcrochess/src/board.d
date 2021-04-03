// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import pt = piece_type;
import bt = board_type;


export class Board {
    private bt.BoardType boardType;
    private uint size;

    private immutable uint capacity = bt.size( bt.BoardType.One ); // 26
    private pt.PieceType[ capacity ][ capacity ] board;

    // @disable this();

    this( bt.BoardType boardType ) {
        this.init( boardType );
    }

    final init( bt.BoardType boardType ) {
        this.boardType = boardType;
        this.size = bt.size( this.boardType );

        clear();
    }

    final clear( bool whole_board = true ) {
        uint len = ( whole_board ) ? this.capacity : this.size;

        for ( uint i = 0; i < len; ++i ) {
            for ( uint j = 0; j < len; ++j ) {
                this.board[ i ][ j ] = pt.PieceType.None;
            }
        }
    }

    final uint getBoardType() const {
        return this.boardType;
    }

    final pt.PieceType[ this.capacity ][ this.capacity ] getBoard() const {
        return this.board;
    }

    final bool isFieldLight( int i, int j ) const {
        return ( (i + j) % 2 == 0 );
    }

    final bool isValid( int i ) const {
        return ( 0 <= i && i < this.size );
    }

    final bool isValid( int i, int j ) const {
        return ( this.isValid( i ) && this.isValid( j ) );
    }

    final pt.PieceType getPiece( int i, int j ) const {
        if ( this.isValid( i, j ) ) {
            return this.board[ i ][ j ];
        }

        return pt.PieceType.None;
    }

    final setPiece( int i, int j, pt.PieceType piece ) {
        if ( this.isValid( i, j ) ) {
            this.board[ i ][ j ] = piece;
        }
    }

    final bool copyBoard( const pt.PieceType[][] board ) {
        if ( board.length != this.size ) return false;

        for ( uint i = 0; i < board.length; ++i ) {
            if ( board[ i ].length != this.size ) return false;
        }

        for ( uint i = 0; i < this.size; ++i ) {
            for ( uint j = 0; j < this.size; ++j ) {
                this.board[ i ][ j ] = board[ i ][ j ];
            }
        }

        return true;
    }
}
