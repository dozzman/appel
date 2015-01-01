#! /bin/bash

for file in `ls tiger/testcases/`
do
    echo "testing file $file"
    ocaml tiger.cma parse_me.ml < tiger/testcases/$file
done
