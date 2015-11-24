
-- Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com
-- Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

module Main
where


import qualified System.IO as SIO
import qualified Data.List as DL

import qualified CommonString as CS
import qualified ConsoleInput as CI
import qualified ConsoleOutput as CO
import qualified ParseBoard as PB

import qualified BoardInitializeLight as BIL
import qualified BoardInitializeDark as BID

import qualified PieceType as PT
import qualified BoardType as BT
import qualified Board as B
import qualified Move as M
import qualified Rules as R
import qualified Game as G
import qualified ParseMove as PM


loop :: G.Game -> IO ()
loop game = do
    putStr "> "
    SIO.hFlush SIO.stdout
    line_ <- getLine

    let (cmd_, params_) = CI.separateCommandAndParameters line_

    if cmd_ `elem` ["q", "quit"] then return ()
    else if cmd_ `elem` ["h", "help"] then do
        putStrLn  " Croatian chess - console application \n\
                  \ Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com. \n\
                  \ Licensed under 3-clause (modified) BSD license. Use a(bout) command for details. \n\
                  \ \n\
                  \ Based on book 'Croatian chess and other variants' \n\
                  \ by Mario Mlačak, ISBN 978-953-55929-2-1 \n\
                  \ \n\
                  \ Commands: \n\
                  \ h, help       - prints this screen \n\
                  \ a, about      - prints about info \n\
                  \ v, version    - prints version(s) info \n\
                  \ q, quit       - quits program \n\
                  \ d, display    - display current positions \n\
                  \ * i, info     - display list of all moves played, time \n\
                  \ * t, time     - (re)sets time counter(s) \n\
                  \ n, new        - starts new game \n\
                  \                 takes optional integer in range [7.. 26] \n\
                  \                 with '23b' and '24b' supported, e.g. n 26 \n\
                  \ * p, players  - sets up players \n\
                  \                 takes two parameters, both are one of 'ai', 'human', 'pipe' \n\
                  \                 'pipe' redirects stdin and stdout to/from another program \n\
                  \ * m, move     - moves piece(s) \n\
                  \                 takes long notation as parameter, e.g. m Sa4-c3 \n\
                  \ * s, save     - saves current game into PGN file \n\
                  \                 takes <path> as parameter, e.g. s my_new_game.pgn \n\
                  \ * l, load     - loads game/positions from PGN file \n\
                  \                 takes <path> as parameter, e.g. l my_new_game.pgn \n\
                  \ \n\
                  \ Commands marked with * are not currently implemented. \n\
                  \ \n\
                  \ Supported variants (to be used as parameter to 'new' command): \n\
                  \ 7   - Odd Classical \n\
                  \ 8   - Classical \n\
                  \ 9   - Odd Croatian Ties \n\
                  \ 10  - Croatian Ties \n\
                  \ 11  - Odd Mayan Ascendancy \n\
                  \ 12  - Mayan Ascendancy \n\
                  \ 13  - Odd Age Of Aquarius \n\
                  \ 14  - Age Of Aquarius \n\
                  \ 15  - Odd Miranda's Veil \n\
                  \ 16  - Miranda's Veil \n\
                  \ 17  - Odd Nineteen \n\
                  \ 18  - Nineteen \n\
                  \ 19  - Odd Hemera's Dawn \n\
                  \ 20  - Hemera's Dawn \n\
                  \ 21  - Odd Tamoanchan Revisited \n\
                  \ 22  - Tamoanchan Revisited \n\
                  \ 23  - Odd Conquest Of Tlalocan \n\
                  \ 24  - Conquest Of Tlalocan \n\
                  \ 23b - Odd Discovery \n\
                  \ 24b - Discovery \n\
                  \ 25  - Odd One \n\
                  \ 26  - One \n"
        loop game
    else if cmd_ `elem` ["a", "about"] then do
        putStrLn  " Croatian chess - console application \n\
                  \ Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com. \n\
                  \ All rights reserved. \n\
                  \ \n\
                  \ Redistribution and use in source and binary forms, with or without \n\
                  \ modification, are permitted provided that the following conditions \n\
                  \ are met: \n\
                  \ \n\
                  \ 1. Redistributions of source code must retain the above copyright \n\
                  \    notice, this list of conditions and the following disclaimer.\n\
                  \ \n\
                  \ 2. Redistributions in binary form must reproduce the above copyright \n\
                  \    notice, this list of conditions and the following disclaimer in the \n\
                  \    documentation and/or other materials provided with the distribution. \n\
                  \ \n\
                  \ 3. Neither the name of the copyright holder nor the names of its \n\
                  \    contributors may be used to endorse or promote products derived \n\
                  \    from this software without specific prior written permission. \n\
                  \ \n\
                  \ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \n\
                  \ \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT \n\
                  \ LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR \n\
                  \ A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT \n\
                  \ HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, \n\
                  \ SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT \n\
                  \ LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, \n\
                  \ DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY \n\
                  \ THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT \n\
                  \ (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE \n\
                  \ OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. \n"
        loop game
    else if cmd_ `elem` ["v", "version"] then do
        putStrLn  " Croatian chess - console application \n\
                  \ Copyright (c) 2014, 2015 Mario Mlačak, mmlacak@gmail.com. \n\
                  \ Licensed under 3-clause (modified) BSD license. Use a(bout) command for details. \n\
                  \ \n\
                  \ 2015-11-21: ver. 0.0.0.0 \n\
                  \ Initial public hosting, more for backup than for public useage. \n"
        loop game
    else if cmd_ `elem` ["d", "display"] then do
        putStrLn $ CO.gameString game
        loop game
    else if cmd_ `elem` ["n", "new"] then do
        let bt_ = PB.boardType $ CS.stripSpace params_
        let bt2_ | BT.isBoardNone bt_ = B.boardType $ R.board $ G.rules game
                 | otherwise          = bt_
        let game_ = G.initializeGame bt2_
        putStrLn $ CO.gameString game_
        loop game_
    else if cmd_ `elem` ["x", "xxx"] then do
--         let ml_ = [ (PT.LightKnight, (1, 3), (3, 4))
--                     (PT.DarkSerpent, (3, 4), (7, 8)),
--                     (PT.Monolith, (5, 6), (2, 7)),
--                     (PT.DarkBishop, (2, 7), (3, 8)) ]

        let ml_ = M.Move { M.plies=[ (M.Ply {M.piece=PT.LightKnight, M.start=(1, 3), M.end=(3, 4), M.sideEffect=M.dummySideEffect}),
                                     (M.Ply {M.piece=PT.DarkSerpent, M.start=(3, 4), M.end=(7, 8), M.sideEffect=M.dummySideEffect}),
                                     (M.Ply {M.piece=PT.Monolith, M.start=(5, 6), M.end=(2, 7), M.sideEffect=M.dummySideEffect}),
                                     (M.Ply {M.piece=PT.DarkBishop, M.start=(2, 7), M.end=(3, 8), M.sideEffect=M.dummySideEffect}) ],
                           M.condition=M.NoCondition }
        print ""
        print ml_
        print ""
        print $ reverse $ M.plies ml_
        print ""
        print $ M.fetchReversedPiecePositions $ reverse $ M.plies ml_

        let pposs_ = M.fetchAllPiecePositions ml_
        print ""
        print pposs_
        print ""

        let r_ = G.rules game
        let b_ = B.setPiecesOnBoard (R.board r_) pposs_
        let game_ = game { G.rules=(r_ { R.board=b_ }) }
        putStrLn $ CO.gameString game_
        loop game_

--         loop game
    else if cmd_ `elem` ["y", "yyy"] then do
        let _move = CS.filterAllSeparators params_ -- "[Nb3-d2]~We7~Qn3-c3~Md7"
        print ""
        print _move
        print ""

-- TODO :: handle errors (!?)

-- main    = do{ result <- parseFromFile numbers "digits.txt"
--             ; case result of
--                 Left err  -> print err
--                 Right xs  -> print (sum xs)
--             }

--         let _pr = PM.parseMove _move
--         let ml_ = case _pr of Right m -> m
--                               Left err -> M.dummyMove 

--         do { if ml_ == M.dummyMove then do { print $ show _pr;
--                                              loop game }
--                                    else do { return () } }

        let r_ = G.rules game
        let ml_ = PM.parseMove _move r_
        print $ ml_

        print ""
        print $ reverse $ M.plies ml_
        print ""
        print $ M.fetchReversedPiecePositions $ reverse $ M.plies ml_

        let pposs_ = M.fetchAllPiecePositions ml_
        print ""
        print pposs_
        print ""

        let b_ = B.setPiecesOnBoard (R.board r_) pposs_
        let game_ = game { G.rules=(r_ { R.board=b_ }) }
        putStrLn $ CO.gameString game_
        loop game_

--       loop game
    else if cmd_ `elem` ["z", "zzz"] then do
        let l = BIL.lightCastlingFiguresInitialPositions BT.One
        print ""
        print $ l
        print ""

        let d = BID.darkCastlingFiguresInitialPositions BT.One
        print ""
        print $ d
        print ""

        loop game
    else if cmd_ `elem` ["i", "info"] then do
        putStrLn "Not implemented yet."
        loop game
    else if cmd_ `elem` ["t", "time"] then do
        putStrLn "Not implemented yet."
        loop game
    else if cmd_ `elem` ["p", "players"] then do
        putStrLn "Not implemented yet."
        loop game
    else if cmd_ `elem` ["m", "move"] then do
        putStrLn "Not implemented yet."
        loop game
    else if cmd_ `elem` ["s", "save"] then do
        putStrLn "Not implemented yet."
        loop game
    else if cmd_ `elem` ["l", "load"] then do
        putStrLn "Not implemented yet."
        loop game
    else if null cmd_ then loop game
    else do
        putStrLn "Unknown command."
        loop game


main :: IO ()
main = do
    SIO.hSetEncoding SIO.stdout SIO.utf8
    putStrLn "Tip: enter 'h' or 'help' for help info."
    putStrLn "     enter 'a' or 'about' for about info, licensing."
    let game_ = G.initializeGame BT.One -- BT.Classical  -- BT.One -- CroatianTies
    putStrLn $ CO.gameString game_
    loop game_

