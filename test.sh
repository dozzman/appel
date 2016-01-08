#! /bin/bash

for file in `ls tiger/testcases/`
do
    echo "testing file $file"
    ./parse_me.native < tiger/testcases/$file
done
