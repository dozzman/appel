#! /bin/bash

for file in `ls tiger/testcases/`
do
    echo "testing file $file"
    ./main.native tiger/testcases/$file
done
