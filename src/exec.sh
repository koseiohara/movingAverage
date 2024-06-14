#!/bin/bash

#PBS -q tqueue
#PBS -N movAve_PRES
#PBS -j oe
#PBS -l nodes=1:ppn=1

JOBNAME=PRES
NOW=$(date "+%Y%m%d_%H%M%S")
RESULT_FILE="../output/result_${JOBNAME}_${NOW}.txt"

ulimit -s unlimited

cd /mnt/hail8/kosei/mim/energetics/movingAverage/src

./EXE >& ${RESULT_FILE}

