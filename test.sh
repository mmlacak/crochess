#! /usr/bin/env bash


export COMPILER=clang 
# export COMPILER=gcc

export OPTIONS="-Wall -pedantic -O3"

./clean.sh


echo
${COMPILER} --version
echo

#echo
#${COMPILER} ${OPTIONS} -fPIC -c src/utils.c -Iinc -o obj/utils.so.o
#echo
#${COMPILER} ${OPTIONS} -fPIC -c src/queue_node.c -Iinc -o obj/queue_node.so.o
#echo
#${COMPILER} ${OPTIONS} -shared -fPIC obj/utils.so.o obj/queue_node.so.o -o lib/libcommon.so
#echo

echo
${COMPILER} ${OPTIONS} -c src/utils.c -Iinc -o obj/utils.o
echo
${COMPILER} ${OPTIONS} -c src/lst_node.c -Iinc -o obj/lst_node.o
echo
${COMPILER} ${OPTIONS} -c src/map_node.c -Iinc -o obj/map_node.o
echo
ar src lib/libcommon.a obj/utils.o obj/lst_node.o obj/map_node.o
# ar src lib/libcommon.a obj/utils.o
echo

echo
${COMPILER} ${OPTIONS} -c test/test_lst_node.c -Iinc -o obj/test_lst_node.o
echo
# # ${COMPILER} ${OPTIONS} obj/utils.o obj/queue_node.o obj/test_lst_node.o -o bin/test_lst_node
# ${COMPILER} ${OPTIONS} obj/test_lst_node.o -o bin/test_lst_node -Llib -lcommon
${COMPILER} -static ${OPTIONS} obj/test_lst_node.o -o bin/test_lst_node -Llib -lcommon
echo

# echo
# ${COMPILER} ${OPTIONS} -c test/test_node.c -Iinc -o test/test_node.o
# echo
# ${COMPILER} ${OPTIONS} -c test/test_node_macro.c -Iinc -o test/test_node_macro.o
# echo
# # # ${COMPILER} ${OPTIONS} obj/utils.o obj/queue_node.o test/test_node.o test/test_node_macro.o -o bin/test_node_macro
# # ${COMPILER} ${OPTIONS} test/test_node.o test/test_node_macro.o -o bin/test_node_macro -Llib -lcommon
# ${COMPILER} -static ${OPTIONS} test/test_node.o test/test_node_macro.o -o bin/test_node_macro -Llib -lcommon
# echo

echo
# ${COMPILER} ${OPTIONS} -c test/test_utils.c -Iinc -o obj/test_utils.o
${COMPILER} ${OPTIONS} -c test/test_utils.c -Iinc -o obj/test_utils.o
echo
# # # ${COMPILER} ${OPTIONS} obj/utils.o obj/test_utils.o -o bin/test_utils
# # ${COMPILER} ${OPTIONS} obj/test_utils.o -o bin/test_utils -Llib -lcommon
# ${COMPILER} -static ${OPTIONS} obj/test_utils.o -o bin/test_utils -Llib -lcommon
${COMPILER} ${OPTIONS} -O3 obj/test_utils.o -o bin/test_utils -Llib -lcommon
echo

ls -Fal --color=auto bin
echo

ls -Fal --color=auto lib
echo

