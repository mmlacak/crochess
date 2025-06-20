#! /usr/bin/env bash

# Copyright (c) 2010 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

cd book

rm -rfv *.aux
rm -rfv *.fdb_latexmk
rm -rfv *.fls
rm -rfv *.lof
rm -rfv *.log
rm -rfv *.lot
rm -rfv *.out
rm -rfv *.pdf
rm -rfv *.toc
echo

echo
echo "0 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---"
echo
echo

pdflatex -halt-on-error -draftmode -jobname crochess crochess.tex
echo
echo

echo "1 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---"
echo
echo

pdflatex -halt-on-error -draftmode -jobname crochess crochess.tex
echo
echo

# echo "2 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---"
# echo
# echo

# pdflatex -halt-on-error -draftmode -jobname crochess crochess.tex
# echo
# echo

echo "z --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---"
echo
echo

pdflatex -halt-on-error -jobname crochess crochess.tex
echo

cd ..
echo
ls -Fal book/*.pdf
