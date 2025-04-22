#/bin/bash

# compile latest
csc -O3 -o fun fun.scm

# run a sequence 1 to 30 inclusive
for i in $(seq 1 30);
do
    echo "running program " $i
    ./fun $i &    
done

