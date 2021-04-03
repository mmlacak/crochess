// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

import str = std.string;
import uni = std.uni;


export enum BoardType {
    ClassicalChess,
    CroatianTies,
    MayanAscendancy,
    AgeOfAquarius,
    MirandasVeil,
    Nineteen,
    HemerasDawn,
    TamoanchanRevisited,
    ConquestOfTlalocan,
    Discovery,
    One,
}


export BoardType from_str( string code ) {
    string lc = uni.toLower( str.strip( code ) );

    switch ( lc ) {
        case "cc": return BoardType.ClassicalChess;
        case "ct": return BoardType.CroatianTies;
        case "ma": return BoardType.MayanAscendancy;
        case "aoa": return BoardType.AgeOfAquarius;
        case "mv": return BoardType.MirandasVeil;
        case "n": return BoardType.Nineteen;
        case "hd": return BoardType.HemerasDawn;
        case "tr": return BoardType.TamoanchanRevisited;
        case "cot": return BoardType.ConquestOfTlalocan;
        case "d": return BoardType.Discovery;
        case "o": return BoardType.One;

        default: return BoardType.One;
    }
}

export string label( BoardType bt ) {
    final switch ( bt ) {
        case BoardType.ClassicalChess: return "Classical Chess";
        case BoardType.CroatianTies: return "Croatian Ties";
        case BoardType.MayanAscendancy: return "Mayan Ascendancy";
        case BoardType.AgeOfAquarius: return "Age of Aquarius";
        case BoardType.MirandasVeil: return "Miranda’s Veil";
        case BoardType.Nineteen: return "Nineteen";
        case BoardType.HemerasDawn: return "Hemera’s Dawn";
        case BoardType.TamoanchanRevisited: return "Tamoanchan Revisited";
        case BoardType.ConquestOfTlalocan: return "Conquest of Tlalocan";
        case BoardType.Discovery: return "Discovery";
        case BoardType.One: return "One";
    }
}

export uint size( BoardType bt ) {
    final switch ( bt ) {
        case BoardType.ClassicalChess: return 8;
        case BoardType.CroatianTies: return 10;
        case BoardType.MayanAscendancy: return 12;
        case BoardType.AgeOfAquarius: return 14;
        case BoardType.MirandasVeil: return 16;
        case BoardType.Nineteen: return 18;
        case BoardType.HemerasDawn: return 20;
        case BoardType.TamoanchanRevisited: return 22;
        case BoardType.ConquestOfTlalocan: return 24;
        case BoardType.Discovery: return 24;
        case BoardType.One: return 26;
    }
}
