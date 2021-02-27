// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum BoardType {
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

impl BoardType {
    pub fn from_str(code: &str) -> Option<BoardType> {
        let lc = code.trim().to_lowercase();
        let lcs = lc.as_str();
        return match lcs {
            "cc" => Some(BoardType::ClassicalChess),
            "ct" => Some(BoardType::CroatianTies),
            "ma" => Some(BoardType::MayanAscendancy),
            "aoa" => Some(BoardType::AgeOfAquarius),
            "mv" => Some(BoardType::MirandasVeil),
            "n" => Some(BoardType::Nineteen),
            "hd" => Some(BoardType::HemerasDawn),
            "tr" => Some(BoardType::TamoanchanRevisited),
            "cot" => Some(BoardType::ConquestOfTlalocan),
            "d" => Some(BoardType::Discovery),
            "o" => Some(BoardType::One),

            _ => None, // BoardType::One,
        };
    }

    pub fn label(&self) -> &'static str {
        return match self {
            BoardType::ClassicalChess => "Classical Chess",
            BoardType::CroatianTies => "Croatian Ties",
            BoardType::MayanAscendancy => "Mayan Ascendancy",
            BoardType::AgeOfAquarius => "Age of Aquarius",
            BoardType::MirandasVeil => "Miranda’s Veil",
            BoardType::Nineteen => "Nineteen",
            BoardType::HemerasDawn => "Hemera’s Dawn",
            BoardType::TamoanchanRevisited => "Tamoanchan Revisited",
            BoardType::ConquestOfTlalocan => "Conquest of Tlalocan",
            BoardType::Discovery => "Discovery",
            BoardType::One => "One",
        };
    }

    pub fn size(&self) -> usize {
        return match self {
            BoardType::ClassicalChess => 8,
            BoardType::CroatianTies => 10,
            BoardType::MayanAscendancy => 12,
            BoardType::AgeOfAquarius => 14,
            BoardType::MirandasVeil => 16,
            BoardType::Nineteen => 18,
            BoardType::HemerasDawn => 20,
            BoardType::TamoanchanRevisited => 22,
            BoardType::ConquestOfTlalocan => 24,
            BoardType::Discovery => 24,
            BoardType::One => 26,
        };
    }
}

impl fmt::Display for BoardType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", self.label());
    }
}
