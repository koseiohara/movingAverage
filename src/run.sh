#!/bin/bash

#PBS -q tqueue
#PBS -N movave
#PBS -j oe
#PBS -l nodes=1:ppn=1

DATASET="JRA55"
INI=1990
FIN=2020
VAR="VVEL"
#NOW=$(date "+%Y%m%d_%H%M%S")
RESULT="../output/result_${DATASET}_${INI}_${FIN}_${VAR}.txt"

NAMELIST="../nml/${DATASET}_${INI}_${FIN}_${VAR}.nml"

ulimit -s unlimited

cd /mnt/jet11/kosei/mim/energetics/movingAverage/src

./EXE < ${NAMELIST} > ${RESULT} 2>&1

