#!/bin/bash

mkdir -p ./logs/queso_logs

numBenchmarks=`cat ./$1 | wc -l`
totalTasks=1
tasksCompleted=0

# Run QUESO normal
java --enable-preview -cp SymbolicOptimizer-1.0-SNAPSHOT-jar-with-dependencies.jar Applier -c $1 -g nam -r rules_q3_s6_nam.txt -sr rules_q3_s3_nam_symb.txt -t $2 -j "nam"

#< ./$1 &> ./logs/queso_logs/queso_normal_logs_"${1/.txt/}"_${2}_nam.txt

# grep -i "Final gate count" ./logs/queso_logs/queso*logs*.txt > ./logs/queso_gate_counts.txt
# grep -i "time to best final" ./logs/queso_logs/queso*logs*ibm.txt > ./logs/queso_ibm_time_to_best.txt

# mkdir -p ./optimized_benchmarks/optimized_queso_ibm
# mkdir -p ./optimized_benchmarks/optimized_queso_rigetti
# mkdir -p ./optimized_benchmarks/optimized_queso_ion

echo "Done running QUESO!"
