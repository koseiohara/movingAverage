#!/bin/bash

#PBS -q tqueue
#PBS -N movave
#PBS -j oe
#PBS -l nodes=1:ppn=1

DATASET="JRA3Q"
INI=1980
FIN=2023
VAR="VDFHR"
#NOW=$(date "+%Y%m%d_%H%M%S")
RESULT="../output/result_${DATASET}_${INI}_${FIN}_${VAR}.txt"

NAMELIST="../nml/${DATASET}_${INI}_${FIN}_${VAR}.nml"

ulimit -s unlimited

cd /mnt/jet11/kosei/mim/energetics/movingAverage/src

./EXE < ${NAMELIST} >> ${RESULT} 2>&1

