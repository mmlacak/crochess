
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
