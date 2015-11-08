
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- All rights reserved. See accompanying LICENSE.txt for details.

module ParseBoard
where

import qualified Data.Char as DC

import qualified Dict as D
import qualified BoardType as BT


boardType :: String -> BT.BoardType
boardType key | D.isKeyInDict key_ dict_ = D.findByKey key_ dict_
              | otherwise                = BT.None
    where key_ = map DC.toLower key
          dict_ = [ -- ( "0", BT.None ),
                    ( "7",      BT.OddClassical ),
                    ( "8",      BT.Classical ),
                    ( "9",      BT.OddCroatianTies ),
                    ( "10",     BT.CroatianTies ),
                    ( "11",     BT.OddMayanAscendancy ),
                    ( "12",     BT.MayanAscendancy ),
                    ( "13",     BT.OddAgeOfAquarius ),
                    ( "14",     BT.AgeOfAquarius ),
                    ( "15",     BT.OddMirandasVeil ),
                    ( "16",     BT.MirandasVeil ),
                    ( "17",     BT.OddNineteen ),
                    ( "18",     BT.Nineteen ),
                    ( "19",     BT.OddHemerasDawn ),
                    ( "20",     BT.HemerasDawn ),
                    ( "21",     BT.OddTamoanchanRevisited ),
                    ( "22",     BT.TamoanchanRevisited ),
                    ( "23",     BT.OddConquestOfTlalocan ),
                    ( "24",     BT.ConquestOfTlalocan ),
                    ( "23b",    BT.OddDiscovery ),
                    ( "24b",    BT.Discovery ),
                    ( "25",     BT.OddOne ),
                    ( "26",     BT.One )
                  ]

boardTypeByName :: String -> BT.BoardType
boardTypeByName name | D.isKeyInDict name dict_ = D.findByKey name dict_
                     | otherwise                = BT.None
    where dict_ = [ -- ( "", None ),
                    ( "Odd Classical",              BT.OddClassical ),
                    ( "Classical",                  BT.Classical ),
                    ( "Odd Croatian Ties",          BT.OddCroatianTies ),
                    ( "Croatian Ties",              BT.CroatianTies ),
                    ( "Odd Mayan Ascendancy",       BT.OddMayanAscendancy ),
                    ( "Mayan Ascendancy",           BT.MayanAscendancy ),
                    ( "Odd Age Of Aquarius",        BT.OddAgeOfAquarius ),
                    ( "Age Of Aquarius",            BT.AgeOfAquarius ),
                    ( "Odd Miranda's Veil",         BT.OddMirandasVeil ),
                    ( "Miranda's Veil",             BT.MirandasVeil ),
                    ( "Odd Nineteen",               BT.OddNineteen ),
                    ( "Nineteen",                   BT.Nineteen ),
                    ( "Odd Hemera's Dawn",          BT.OddHemerasDawn ),
                    ( "Hemera's Dawn",              BT.HemerasDawn ),
                    ( "Odd Tamoanchan Revisited",   BT.OddTamoanchanRevisited ),
                    ( "Tamoanchan Revisited",       BT.TamoanchanRevisited ),
                    ( "Odd Conquest Of Tlalocan",   BT.OddConquestOfTlalocan ),
                    ( "Conquest Of Tlalocan",       BT.ConquestOfTlalocan ),
                    ( "Odd Discovery",              BT.OddDiscovery ),
                    ( "Discovery",                  BT.Discovery ),
                    ( "Odd One",                    BT.OddOne ),
                    ( "One",                        BT.One )
                  ]

