
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

pub fn label(e: BoardType) ->&'static str {
    return match e {
        BoardType::ClassicalChess => "Classical chess",
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
