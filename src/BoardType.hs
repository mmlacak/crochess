
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module BoardType
where

import qualified Dict as D


data BoardType  = None
                | OddClassical
                | Classical
                | OddCroatianTies
                | CroatianTies
                | OddMayanAscendancy
                | MayanAscendancy
                | OddAgeOfAquarius
                | AgeOfAquarius
                | OddMirandasVeil
                | MirandasVeil
                | OddNineteen
                | Nineteen
                | OddHemerasDawn
                | HemerasDawn
                | OddTamoanchanRevisited
                | TamoanchanRevisited
                | OddConquestOfTlalocan
                | ConquestOfTlalocan
                | OddDiscovery
                | Discovery
                | OddOne
                | One
    deriving (Show, Read, Eq, Bounded, Ord, Enum)

isBoardOdd :: BoardType -> Bool
isBoardOdd bt = bt `elem` [ OddClassical,
                            OddCroatianTies,
                            OddMayanAscendancy,
                            OddAgeOfAquarius,
                            OddMirandasVeil,
                            OddNineteen,
                            OddHemerasDawn,
                            OddTamoanchanRevisited,
                            OddConquestOfTlalocan,
                            OddDiscovery,
                            OddOne ]

isBoardEven :: BoardType -> Bool
isBoardEven bt = bt `elem` [ Classical,
                             CroatianTies,
                             MayanAscendancy,
                             AgeOfAquarius,
                             MirandasVeil,
                             Nineteen,
                             HemerasDawn,
                             TamoanchanRevisited,
                             ConquestOfTlalocan,
                             Discovery,
                             One ]

isBoardNone :: BoardType -> Bool
isBoardNone bt = bt == None

isBoardWithMonolith :: BoardType -> Bool
isBoardWithMonolith bt = bt `elem` [OddDiscovery, Discovery, OddOne, One]

boardName :: BoardType -> String
boardName bt = D.findByKey bt dict_
    where dict_ = [ ( None, "" ),
                    ( OddClassical, "Odd Classical" ),
                    ( Classical, "Classical" ),
                    ( OddCroatianTies, "Odd Croatian Ties" ),
                    ( CroatianTies, "Croatian Ties" ),
                    ( OddMayanAscendancy, "Odd Mayan Ascendancy" ),
                    ( MayanAscendancy, "Mayan Ascendancy" ),
                    ( OddAgeOfAquarius, "Odd Age Of Aquarius" ),
                    ( AgeOfAquarius, "Age Of Aquarius" ),
                    ( OddMirandasVeil, "Odd Miranda's Veil" ),
                    ( MirandasVeil, "Miranda's Veil" ),
                    ( OddNineteen, "Odd Nineteen" ),
                    ( Nineteen, "Nineteen" ),
                    ( OddHemerasDawn, "Odd Hemera's Dawn" ),
                    ( HemerasDawn, "Hemera's Dawn" ),
                    ( OddTamoanchanRevisited, "Odd Tamoanchan Revisited" ),
                    ( TamoanchanRevisited, "Tamoanchan Revisited" ),
                    ( OddConquestOfTlalocan, "Odd Conquest Of Tlalocan" ),
                    ( ConquestOfTlalocan, "Conquest Of Tlalocan" ),
                    ( OddDiscovery, "Odd Discovery" ),
                    ( Discovery, "Discovery" ),
                    ( OddOne, "Odd One" ),
                    ( One, "One" )
                  ]

boardSize :: BoardType -> Int
boardSize bt  = D.findByKey bt dict_
    where dict_ = [ ( None, 0 ),
                    ( OddClassical, 7 ),
                    ( Classical, 8 ),
                    ( OddCroatianTies, 9 ),
                    ( CroatianTies, 10 ),
                    ( OddMayanAscendancy, 11 ),
                    ( MayanAscendancy, 12 ),
                    ( OddAgeOfAquarius, 13 ),
                    ( AgeOfAquarius, 14 ),
                    ( OddMirandasVeil, 15 ),
                    ( MirandasVeil, 16 ),
                    ( OddNineteen, 17 ),
                    ( Nineteen, 18 ),
                    ( OddHemerasDawn, 19 ),
                    ( HemerasDawn, 20 ),
                    ( OddTamoanchanRevisited, 21 ),
                    ( TamoanchanRevisited, 22 ),
                    ( OddConquestOfTlalocan, 23 ),
                    ( ConquestOfTlalocan, 24 ),
                    ( OddDiscovery, 23 ),
                    ( Discovery, 24 ),
                    ( OddOne, 25 ),
                    ( One, 26 )
                  ]

minBoardSize :: Int
minBoardSize = 7

maxBoardSize :: Int
maxBoardSize = 26

isBoardSizeValid :: Int -> Bool
isBoardSizeValid size = (minBoardSize <= size) && (size <= maxBoardSize)

