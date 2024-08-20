#!/bin/bash

#PBS -q tqueue
#PBS -N movave
#PBS -j oe
#PBS -l nodes=1:ppn=1

VAR="mim_st"
NOW=$(date "+%Y%m%d_%H%M%S")
RESULT_FILE="../output/result_${VAR}_${NOW}.txt"

NAMELIST="../nml/${VAR}_1975_2023_all.nml"

ulimit -s unlimited

cd /mnt/jet11/kosei/mim/energetics/movingAverage/src

./EXE < ${NAMELIST} >> ${RESULT_FILE} 2>&1
#./EXE >> ${RESULT_FILE} 2>&1

