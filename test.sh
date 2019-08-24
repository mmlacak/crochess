#! /usr/bin/env bash


export COMPILER=clang # gcc

./clean.sh


echo
$COMPILER --version
echo

#echo
#$COMPILER -Wall -pedantic -O3 -fPIC -c src/utils.c -Iinc -o obj/utils.so.o
#echo
#$COMPILER -Wall -pedantic -O3 -fPIC -c src/queue_node.c -Iinc -o obj/queue_node.so.o
#echo
#$COMPILER -Wall -pedantic -O3 -shared -fPIC obj/utils.so.o obj/queue_node.so.o -o lib/libcommon.so
#echo

echo
$COMPILER -Wall -pedantic -O3 -c src/utils.c -Iinc -o obj/utils.o
echo
$COMPILER -Wall -pedantic -O3 -c src/lst_node.c -Iinc -o obj/lst_node.o
echo
$COMPILER -Wall -pedantic -O3 -c src/map_node.c -Iinc -o obj/map_node.o
echo
ar src lib/libcommon.a obj/utils.o obj/lst_node.o obj/map_node.o
# ar src lib/libcommon.a obj/utils.o
echo

echo
$COMPILER -Wall -pedantic -O3 -c test/test_lst_node.c -Iinc -o obj/test_lst_node.o
echo
# # $COMPILER -Wall -pedantic -O3 obj/utils.o obj/queue_node.o obj/test_lst_node.o -o bin/test_lst_node
# $COMPILER -Wall -pedantic -O3 obj/test_lst_node.o -o bin/test_lst_node -Llib -lcommon
$COMPILER -static -Wall -pedantic -O3 obj/test_lst_node.o -o bin/test_lst_node -Llib -lcommon
echo

# echo
# $COMPILER -Wall -pedantic -O3 -c test/test_node.c -Iinc -o test/test_node.o
# echo
# $COMPILER -Wall -pedantic -O3 -c test/test_node_macro.c -Iinc -o test/test_node_macro.o
# echo
# # # $COMPILER -Wall -pedantic -O3 obj/utils.o obj/queue_node.o test/test_node.o test/test_node_macro.o -o bin/test_node_macro
# # $COMPILER -Wall -pedantic -O3 test/test_node.o test/test_node_macro.o -o bin/test_node_macro -Llib -lcommon
# $COMPILER -static -Wall -pedantic -O3 test/test_node.o test/test_node_macro.o -o bin/test_node_macro -Llib -lcommon
# echo

echo
$COMPILER -Wall -pedantic -O3 -c test/test_utils.c -Iinc -o obj/test_utils.o
echo
# # $COMPILER -Wall -pedantic -O3 obj/utils.o obj/test_utils.o -o bin/test_utils
# $COMPILER -Wall -pedantic -O3 obj/test_utils.o -o bin/test_utils -Llib -lcommon
$COMPILER -static -Wall -pedantic -O3 obj/test_utils.o -o bin/test_utils -Llib -lcommon
echo

ls -Fal --color=auto bin
echo

ls -Fal --color=auto lib
echo

