#! /usr/bin/env bash


source ./cc-opts.sh

echo
${COMPILER} --version
echo


echo
echo "${COMPILER} ${OPTIONS} -c test/test_lst_node.c -Iinc -o obj/test_lst_node.o"
${COMPILER} ${OPTIONS} -c test/test_lst_node.c -Iinc -o obj/test_lst_node.o
echo
# # ${COMPILER} ${OPTIONS} obj/utils.o obj/queue_node.o obj/test_lst_node.o -o bin/test_lst_node
# ${COMPILER} ${OPTIONS} obj/test_lst_node.o -o bin/test_lst_node -Llib -lcommon
echo "${COMPILER} -static ${OPTIONS} obj/test_lst_node.o -o bin/test_lst_node -Llib -lcommon"
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
echo "${COMPILER} ${OPTIONS} -c test/test_utils.c -Iinc -o obj/test_utils.o"
${COMPILER} ${OPTIONS} -c test/test_utils.c -Iinc -o obj/test_utils.o
echo
# # # ${COMPILER} ${OPTIONS} obj/utils.o obj/test_utils.o -o bin/test_utils
# # ${COMPILER} ${OPTIONS} obj/test_utils.o -o bin/test_utils -Llib -lcommon
# ${COMPILER} -static ${OPTIONS} obj/test_utils.o -o bin/test_utils -Llib -lcommon
echo "${COMPILER} ${OPTIONS} -O3 obj/test_utils.o -o bin/test_utils -Llib -lcommon"
${COMPILER} ${OPTIONS} -O3 obj/test_utils.o -o bin/test_utils -Llib -lcommon
echo


ls -Fal --color=auto bin
echo

ls -Fal --color=auto lib
echo
