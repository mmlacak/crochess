
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- All rights reserved. See accompanying LICENSE.txt for details.

module ParsePosition
where

import qualified Data.Char as DC

import qualified Dict as D
import qualified BoardType as BT
import qualified Position as Pos


type ColumnIndexChar = Char
type RowIndexChar = Char

dummyColumnIndexChar :: ColumnIndexChar
dummyColumnIndexChar = ' '

startColumnIndexChar :: ColumnIndexChar
startColumnIndexChar = 'a'

endColumnIndexChar :: BT.BoardType -> ColumnIndexChar
endColumnIndexChar bt = D.findByKey bt dict_
    where dict_ = [ ( BT.None, dummyColumnIndexChar ),
                    ( BT.OddClassical, 'g' ),
                    ( BT.Classical, 'h' ),
                    ( BT.OddCroatianTies, 'i' ),
                    ( BT.CroatianTies, 'j' ),
                    ( BT.OddMayanAscendancy, 'k' ),
                    ( BT.MayanAscendancy, 'l' ),
                    ( BT.OddAgeOfAquarius, 'm' ),
                    ( BT.AgeOfAquarius, 'n' ),
                    ( BT.OddMirandasVeil, 'o' ),
                    ( BT.MirandasVeil, 'p' ),
                    ( BT.OddNineteen, 'q' ),
                    ( BT.Nineteen, 'r' ),
                    ( BT.OddHemerasDawn, 's' ),
                    ( BT.HemerasDawn, 't' ),
                    ( BT.OddTamoanchanRevisited, 'u' ),
                    ( BT.TamoanchanRevisited, 'v' ),
                    ( BT.OddConquestOfTlalocan, 'w' ),
                    ( BT.ConquestOfTlalocan, 'x' ),
                    ( BT.OddDiscovery, 'w' ),
                    ( BT.Discovery, 'x' ),
                    ( BT.OddOne, 'y' ),
                    ( BT.One, 'z' )
                  ]

dummyRowIndexChar :: RowIndexChar
dummyRowIndexChar = ' '

type RowIndexString = [ RowIndexChar ]

dummyRowIndexString :: RowIndexString
dummyRowIndexString = " "


isColumnIndexChar :: Char -> BT.BoardType -> Bool
isColumnIndexChar c bt = c `elem` [startColumnIndexChar .. (endColumnIndexChar bt)]

isIntColumnIndex :: Int -> BT.BoardType -> Bool
isIntColumnIndex i bt = ((0 <= i) && (i <= (BT.boardSize bt)))

columnIndexInt :: Char -> BT.BoardType -> Pos.ColumnIndex
columnIndexInt c bt | isColumnIndexChar c bt = (DC.ord c) - (DC.ord startColumnIndexChar)
                    | otherwise = Pos.dummyColumnIndex

consoleColumnIndex :: Int -> BT.BoardType -> ColumnIndexChar
consoleColumnIndex i bt | isIntColumnIndex i bt = DC.chr (i + DC.ord startColumnIndexChar)
                        | otherwise = dummyColumnIndexChar

