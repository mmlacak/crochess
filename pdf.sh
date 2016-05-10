#! /usr/bin/env bash

# Copyright (c) 2010 - 2016 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.

cd book/tex

rm -rfv *.aux
rm -rfv *.lof
rm -rfv *.log
rm -rfv *.lot
rm -rfv *.out
rm -rfv *.toc

# pdflatex ignores -output-directory option
pdflatex crochess.tex

# pdflatex is unstable/unreliable compiler
pdflatex crochess.tex

cd ../..
echo
ls -Fal book/tex/*.pdf
