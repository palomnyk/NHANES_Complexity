#!/bin/bash

for j in `seq 13238316 13238338` ; do
    scancel $j
    echo $j
done