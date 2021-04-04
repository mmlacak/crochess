// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


import pt = piece_type;
import bt = board_type;
import ct = chip_type;


export final class Board {
    private bt.BoardType boardType;
    private uint size;

    private immutable static uint CAPACITY = bt.size( bt.BoardType.One ); // 26
    private pt.PieceType[ CAPACITY ][ CAPACITY ] board;
    private ct.ChipType[ CAPACITY ][ CAPACITY ] chips;

    @disable this();

    this( bt.BoardType boardType ) {
        this.init( boardType );
    }

    void init( bt.BoardType boardType ) {
        this.boardType = boardType;
        this.size = bt.size( this.boardType );

        this.clear();
    }

    void clear() {
        for ( uint i = 0; i < Board.CAPACITY; ++i ) {
            for ( uint j = 0; j < Board.CAPACITY; ++j ) {
                this.board[ i ][ j ] = pt.PieceType.None;
                this.chips[ i ][ j ] = ct.ChipType.None;
            }
        }
    }

    void setup() {
        this.clear();

        final switch ( this.boardType ) {
            case bt.BoardType.ClassicalChess: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_CLASSICAL_CHESS, cast(ct.ChipType[][])Board.CHIPS_CLASSICAL_CHESS );
                break;
            }
            case bt.BoardType.CroatianTies: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_CROATIAN_TIES_BOARD, cast(ct.ChipType[][])Board.CHIPS_CROTIAN_TIES );
                break;
            }
            case bt.BoardType.MayanAscendancy: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_MAYAN_ASCENDANCY, cast(ct.ChipType[][])Board.CHIPS_MAYAN_ASCENDANCY );
                break;
            }
            case bt.BoardType.AgeOfAquarius: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_AGE_OF_AQUARIUS, cast(ct.ChipType[][])Board.CHIPS_AGE_OF_AQUARIUS );
                break;
            }
            case bt.BoardType.MirandasVeil: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_MIRANDAS_VEIL, cast(ct.ChipType[][])Board.CHIPS_MIRANDAS_VEIL );
                break;
            }
            case bt.BoardType.Nineteen: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_NINETEEN, cast(ct.ChipType[][])Board.CHIPS_NINETEEN );
                break;
            }
            case bt.BoardType.HemerasDawn: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_HEMERAS_DAWN, cast(ct.ChipType[][])Board.CHIPS_HEMERAS_DAWN );
                break;
            }
            case bt.BoardType.TamoanchanRevisited: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_TAMOANCHAN_REVISITED, cast(ct.ChipType[][])Board.CHIPS_TAMOANCHAN_REVISITED );
                break;
            }
            case bt.BoardType.ConquestOfTlalocan: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_CONQUEST_OF_TLALOCAN, cast(ct.ChipType[][])Board.CHIPS_CONQUEST_OF_TLALOCAN );
                break;
            }
            case bt.BoardType.Discovery: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_DISCOVERY, cast(ct.ChipType[][])Board.CHIPS_DISCOVERY );
                break;
            }
            case bt.BoardType.One: {
                this.copyFrom( cast(pt.PieceType[][])Board.SETUP_ONE, cast(ct.ChipType[][])Board.CHIPS_ONE );
                break;
            }
        }
    }

    uint getBoardType() const {
        return this.boardType;
    }

    pt.PieceType[ Board.CAPACITY ][ Board.CAPACITY ] getBoard() const {
        return this.board;
    }

    ct.ChipType[ Board.CAPACITY ][ Board.CAPACITY ] getChips() const {
        return this.chips;
    }

    static bool isFieldLight( int i, int j ) {
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


    private immutable static SIZE_CLASSICAL_CHESS = bt.size( bt.BoardType.ClassicalChess );
    private immutable static SIZE_CROTIAN_TIES = bt.size( bt.BoardType.CroatianTies );
    private immutable static SIZE_MAYAN_ASCENDANCY = bt.size( bt.BoardType.MayanAscendancy );
    private immutable static SIZE_AGE_OF_AQUARIUS = bt.size( bt.BoardType.AgeOfAquarius );
    private immutable static SIZE_MIRANDAS_VEIL = bt.size( bt.BoardType.MirandasVeil );
    private immutable static SIZE_NINETEEN = bt.size( bt.BoardType.Nineteen );
    private immutable static SIZE_HEMERAS_DAWN = bt.size( bt.BoardType.HemerasDawn );
    private immutable static SIZE_TAMOANCHAN_REVISITED = bt.size( bt.BoardType.TamoanchanRevisited );
    private immutable static SIZE_CONQUEST_OF_TLALOCAN = bt.size( bt.BoardType.ConquestOfTlalocan );
    private immutable static SIZE_DISCOVERY = bt.size( bt.BoardType.Discovery );
    private immutable static SIZE_ONE = bt.size( bt.BoardType.One );


    private immutable static t = pt.PieceType.DimStar;

    private immutable static i = pt.PieceType.DarkStarchild;
    private immutable static h = pt.PieceType.DarkShaman;
    private immutable static s = pt.PieceType.DarkSerpent;
    private immutable static c = pt.PieceType.DarkCentaur;
    private immutable static w = pt.PieceType.DarkWave;
    private immutable static u = pt.PieceType.DarkUnicorn;
    private immutable static a = pt.PieceType.DarkPyramid;
    private immutable static g = pt.PieceType.DarkPegasus;
    private immutable static k = pt.PieceType.DarkKing;
    private immutable static q = pt.PieceType.DarkQueen;
    private immutable static r = pt.PieceType.DarkRook;
    private immutable static b = pt.PieceType.DarkBishop;
    private immutable static n = pt.PieceType.DarkKnight;
    private immutable static p = pt.PieceType.DarkPawn;

    private immutable static x = pt.PieceType.None;

    private immutable static P = pt.PieceType.LightPawn;
    private immutable static N = pt.PieceType.LightKnight;
    private immutable static B = pt.PieceType.LightBishop;
    private immutable static R = pt.PieceType.LightRook;
    private immutable static Q = pt.PieceType.LightQueen;
    private immutable static K = pt.PieceType.LightKing;
    private immutable static G = pt.PieceType.LightPegasus;
    private immutable static A = pt.PieceType.LightPyramid;
    private immutable static U = pt.PieceType.LightUnicorn;
    private immutable static W = pt.PieceType.LightWave;
    private immutable static C = pt.PieceType.LightCentaur;
    private immutable static S = pt.PieceType.LightSerpent;
    private immutable static H = pt.PieceType.LightShaman;
    private immutable static I = pt.PieceType.LightStarchild;

    private immutable static T = pt.PieceType.BrightStar;

    private immutable static M = pt.PieceType.Monolith;


    private immutable static pt.PieceType[ SIZE_CLASSICAL_CHESS ][ SIZE_CLASSICAL_CHESS ] SETUP_CLASSICAL_CHESS = [
        [ r, n, b, q, k, b, n, r ],
        [ p, p, p, p, p, p, p, p ],
        [ x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x ],
        [ P, P, P, P, P, P, P, P ],
        [ R, N, B, Q, K, B, N, R ],
    ];

    private immutable static pt.PieceType[ SIZE_CROTIAN_TIES ][ SIZE_CROTIAN_TIES ] SETUP_CROATIAN_TIES_BOARD = [
        [ r, g, n, b, q, k, b, n, g, r ],
        [ p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P ],
        [ R, G, N, B, Q, K, B, N, G, R ],
    ];

    private immutable static pt.PieceType[ SIZE_MAYAN_ASCENDANCY ][ SIZE_MAYAN_ASCENDANCY ] SETUP_MAYAN_ASCENDANCY = [
        [ r, g, a, n, b, q, k, b, n, a, g, r ],
        [ p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P ],
        [ R, G, A, N, B, Q, K, B, N, A, G, R ],
    ];

    private immutable static pt.PieceType[ SIZE_AGE_OF_AQUARIUS ][ SIZE_AGE_OF_AQUARIUS ] SETUP_AGE_OF_AQUARIUS = [
        [ r, g, a, u, n, b, q, k, b, n, u, a, g, r ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ R, G, A, U, N, B, Q, K, B, N, U, A, G, R ],
    ];

    private immutable static pt.PieceType[ SIZE_MIRANDAS_VEIL ][ SIZE_MIRANDAS_VEIL ] SETUP_MIRANDAS_VEIL = [
        [ r, g, a, u, w, n, b, q, k, b, n, w, u, a, g, r ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ R, G, A, U, W, N, B, Q, K, B, N, W, U, A, G, R ],
    ];

    private immutable static pt.PieceType[ SIZE_NINETEEN ][ SIZE_NINETEEN ] SETUP_NINETEEN = [
        [ t, r, n, b, w, g, u, a, q, k, a, u, g, w, b, n, r, T ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ T, R, N, B, W, G, U, A, Q, K, A, U, G, W, B, N, R, t ],
    ];

    private immutable static pt.PieceType[ SIZE_HEMERAS_DAWN ][ SIZE_HEMERAS_DAWN ] SETUP_HEMERAS_DAWN = [
        [ t, r, n, b, c, w, g, u, a, q, k, a, u, g, w, c, b, n, r, T ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, p, x, x, x, p, x, x, x, x, x, x, p, x, x, x, p, x, x ],
        [ x, x, x, p, x, p, x, x, x, x, x, x, x, x, p, x, p, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, P, x, P, x, x, x, x, x, x, x, x, P, x, P, x, x, x ],
        [ x, x, P, x, x, x, P, x, x, x, x, x, x, P, x, x, x, P, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ T, R, N, B, C, W, G, U, A, Q, K, A, U, G, W, C, B, N, R, t ],
    ];

    private immutable static pt.PieceType[ SIZE_TAMOANCHAN_REVISITED ][ SIZE_TAMOANCHAN_REVISITED ] SETUP_TAMOANCHAN_REVISITED = [
        [ t, r, n, b, s, w, u, g, c, a, q, k, a, c, g, u, w, s, b, n, r, T ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, x, x, x, p, x, x, x, p, p, x, x, x, p, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, p, x, p, x, x, p, x, p, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, P, x, P, x, x, P, x, P, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, P, x, x, x, P, P, x, x, x, P, x, x, x, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ T, R, N, B, S, W, U, G, C, A, Q, K, A, C, G, U, W, S, B, N, R, t ],
    ];

    private immutable static pt.PieceType[ SIZE_CONQUEST_OF_TLALOCAN ][ SIZE_CONQUEST_OF_TLALOCAN ] SETUP_CONQUEST_OF_TLALOCAN = [
        [ t, r, n, b, s, c, u, w, g, a, h, q, k, h, a, g, w, u, c, s, b, n, r, T ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x ],
        [ x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x ],
        [ x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ T, R, N, B, S, C, U, W, G, A, H, Q, K, H, A, G, W, U, C, S, B, N, R, t ],
    ];

    private immutable static pt.PieceType[ SIZE_DISCOVERY ][ SIZE_DISCOVERY ] SETUP_DISCOVERY = [
        [ t, r, n, b, s, c, u, w, g, a, h, q, k, h, a, g, w, u, c, s, b, n, r, T ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x ],
        [ x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, M, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, M, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x ],
        [ x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ T, R, N, B, S, C, U, W, G, A, H, Q, K, H, A, G, W, U, C, S, B, N, R, t ],
    ];

    private immutable static pt.PieceType[ SIZE_ONE ][ SIZE_ONE ] SETUP_ONE = [
        [ t, r, n, b, s, i, c, u, g, w, a, h, q, k, h, a, w, g, u, c, i, s, b, n, r, T ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p ],
        [ x, x, x, x, p, x, x, x, p, p, x, x, p, p, x, x, p, p, x, x, x, p, x, x, x, x ],
        [ x, x, x, x, x, p, x, p, x, x, p, x, p, p, x, p, x, x, p, x, p, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, M, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, M, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x ],
        [ x, x, x, x, x, P, x, P, x, x, P, x, P, P, x, P, x, x, P, x, P, x, x, x, x, x ],
        [ x, x, x, x, P, x, x, x, P, P, x, x, P, P, x, x, P, P, x, x, x, P, x, x, x, x ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P ],
        [ T, R, N, B, S, I, C, U, G, W, A, H, Q, K, H, A, W, G, U, C, I, S, B, N, R, t ],
    ];


    private immutable static z = ct.ChipType.None;
    private immutable static L = ct.ChipType.CanRush; // Pawns
    private immutable static V = ct.ChipType.CanCastle; // Rooks, Kings
    private immutable static O = ct.ChipType.TagForPromotion; // Pawn


    private immutable static ct.ChipType[ SIZE_CLASSICAL_CHESS ][ SIZE_CLASSICAL_CHESS ] CHIPS_CLASSICAL_CHESS = [
        [ V, z, z, z, V, z, z, V ],
        [ L, L, L, L, L, L, L, L ],
        [ z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z ],
        [ L, L, L, L, L, L, L, L ],
        [ V, z, z, z, V, z, z, V ],
    ];

    private immutable static ct.ChipType[ SIZE_CROTIAN_TIES ][ SIZE_CROTIAN_TIES ] CHIPS_CROTIAN_TIES = [
        [ V, z, z, z, z, V, z, z, z, V ],
        [ L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L ],
        [ V, z, z, z, z, V, z, z, z, V ],
    ];

    private immutable static ct.ChipType[ SIZE_MAYAN_ASCENDANCY ][ SIZE_MAYAN_ASCENDANCY ] CHIPS_MAYAN_ASCENDANCY = [
        [ V, z, z, z, z, z, V, z, z, z, z, V ],
        [ L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L ],
        [ V, z, z, z, z, z, V, z, z, z, z, V ],
    ];

    private immutable static ct.ChipType[ SIZE_AGE_OF_AQUARIUS ][ SIZE_AGE_OF_AQUARIUS ] CHIPS_AGE_OF_AQUARIUS = [
        [ V, z, z, z, z, z, z, V, z, z, z, z, z, V ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ V, z, z, z, z, z, z, V, z, z, z, z, z, V ],
    ];

    private immutable static ct.ChipType[ SIZE_MIRANDAS_VEIL ][ SIZE_MIRANDAS_VEIL ] CHIPS_MIRANDAS_VEIL = [
        [ V, z, z, z, z, z, z, z, V, z, z, z, z, z, z, V ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ V, z, z, z, z, z, z, z, V, z, z, z, z, z, z, V ],
    ];

    private immutable static ct.ChipType[ SIZE_NINETEEN ][ SIZE_NINETEEN ] CHIPS_NINETEEN = [
        [ z, V, z, z, z, z, z, z, z, V, z, z, z, z, z, z, V, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, V, z, z, z, z, z, z, z, V, z, z, z, z, z, z, V, z ],
    ];

    private immutable static ct.ChipType[ SIZE_HEMERAS_DAWN ][ SIZE_HEMERAS_DAWN ] CHIPS_HEMERAS_DAWN = [
        [ z, V, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, V, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, L, z, z, z, L, z, z, z, z, z, z, L, z, z, z, L, z, z ],
        [ z, z, z, L, z, L, z, z, z, z, z, z, z, z, L, z, L, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, L, z, L, z, z, z, z, z, z, z, z, L, z, L, z, z, z ],
        [ z, z, L, z, z, z, L, z, z, z, z, z, z, L, z, z, z, L, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, V, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, V, z ],
    ];

    private immutable static ct.ChipType[ SIZE_TAMOANCHAN_REVISITED ][ SIZE_TAMOANCHAN_REVISITED ] CHIPS_TAMOANCHAN_REVISITED = [
        [ z, V, z, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, z, V, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, z, z, z, L, z, z, z, L, L, z, z, z, L, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, L, z, L, z, z, L, z, L, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, L, z, L, z, z, L, z, L, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, L, z, z, z, L, L, z, z, z, L, z, z, z, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, V, z, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, z, V, z ],
    ];

    private immutable static ct.ChipType[ SIZE_CONQUEST_OF_TLALOCAN ][ SIZE_CONQUEST_OF_TLALOCAN ] CHIPS_CONQUEST_OF_TLALOCAN = [
        [ z, V, z, z, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, z, z, V, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, L, z, z, z, L, L, z, z, L, L, z, z, L, L, z, z, z, L, z, z, z ],
        [ z, z, z, z, L, z, L, z, z, L, z, L, L, z, L, z, z, L, z, L, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, L, z, L, z, z, L, z, L, L, z, L, z, z, L, z, L, z, z, z, z ],
        [ z, z, z, L, z, z, z, L, L, z, z, L, L, z, z, L, L, z, z, z, L, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, V, z, z, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, z, z, V, z ],
    ];

    private immutable static ct.ChipType[ SIZE_DISCOVERY ][ SIZE_DISCOVERY ] CHIPS_DISCOVERY = [
        [ z, V, z, z, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, z, z, V, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, L, z, z, z, L, L, z, z, L, L, z, z, L, L, z, z, z, L, z, z, z ],
        [ z, z, z, z, L, z, L, z, z, L, z, L, L, z, L, z, z, L, z, L, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, L, z, L, z, z, L, z, L, L, z, L, z, z, L, z, L, z, z, z, z ],
        [ z, z, z, L, z, z, z, L, L, z, z, L, L, z, z, L, L, z, z, z, L, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, V, z, z, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, z, z, V, z ],
    ];

    private immutable static ct.ChipType[ SIZE_ONE ][ SIZE_ONE ] CHIPS_ONE = [
        [ z, V, z, z, z, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, z, z, z, V, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, z, z, z, L, z, z, z, L, L, z, z, L, L, z, z, L, L, z, z, z, L, z, z, z, z ],
        [ z, z, z, z, z, L, z, L, z, z, L, z, L, L, z, L, z, z, L, z, L, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z, z ],
        [ z, z, z, z, z, L, z, L, z, z, L, z, L, L, z, L, z, z, L, z, L, z, z, z, z, z ],
        [ z, z, z, z, L, z, z, z, L, L, z, z, L, L, z, z, L, L, z, z, z, L, z, z, z, z ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L, L ],
        [ z, V, z, z, z, z, z, z, z, z, z, z, z, V, z, z, z, z, z, z, z, z, z, z, V, z ],
    ];
}
