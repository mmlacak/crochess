#! /usr/bin/env bash


source ./cc-opts.sh

echo
${COMPILER} --version
echo


echo
echo "${COMPILER} ${OPTIONS} -fPIC -c src/utils.c -Iinc -o obj/utils.so.o"
${COMPILER} ${OPTIONS} -fPIC -c src/utils.c -Iinc -o obj/utils.so.o
echo
echo "${COMPILER} ${OPTIONS} -fPIC -c src/lst_node.c -Iinc -o obj/lst_node.so.o"
${COMPILER} ${OPTIONS} -fPIC -c src/lst_node.c -Iinc -o obj/lst_node.so.o
echo
echo ${COMPILER} ${OPTIONS} -fPIC -c src/map_node.c -Iinc -o obj/map_node.so.o
${COMPILER} ${OPTIONS} -fPIC -c src/map_node.c -Iinc -o obj/map_node.so.o
echo
echo "${COMPILER} ${OPTIONS} -shared -fPIC obj/utils.so.o obj/lst_node.so.o obj/map_node.so.o -o lib/libcommon.so"
${COMPILER} ${OPTIONS} -shared -fPIC obj/utils.so.o obj/lst_node.so.o obj/map_node.so.o -o lib/libcommon.so
echo


ls -Fal --color=auto bin
echo

ls -Fal --color=auto lib
echo
