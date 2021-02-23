#! /usr/bin/env bash


source ./cc-opts.sh

echo
${COMPILER} --version
echo


echo
echo "${COMPILER} ${OPTIONS} -c src/utils.c -Iinc -o obj/utils.o"
${COMPILER} ${OPTIONS} -c src/utils.c -Iinc -o obj/utils.o
echo
echo "${COMPILER} ${OPTIONS} -c src/lst_node.c -Iinc -o obj/lst_node.o"
${COMPILER} ${OPTIONS} -c src/lst_node.c -Iinc -o obj/lst_node.o
echo
echo "${COMPILER} ${OPTIONS} -c src/map_node.c -Iinc -o obj/map_node.o"
${COMPILER} ${OPTIONS} -c src/map_node.c -Iinc -o obj/map_node.o
echo
"ar src lib/libcommon.a obj/utils.o obj/lst_node.o obj/map_node.o"
ar src lib/libcommon.a obj/utils.o obj/lst_node.o obj/map_node.o
echo


ls -Fal --color=auto bin
echo

ls -Fal --color=auto lib
echo
