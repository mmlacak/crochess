#! /usr/bin/env bash

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

cd book

rm -rfv *.aux
rm -rfv *.lof
rm -rfv *.log
rm -rfv *.lot
rm -rfv *.out
rm -rfv *.pdf
rm -rfv *.toc
echo

pdflatex -draftmode crochess.tex
echo
echo

pdflatex -draftmode crochess.tex
echo
echo

pdflatex crochess.tex
echo

cd ..
echo
ls -Fal book/*.pdf
