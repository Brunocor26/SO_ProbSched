#!/bin/bash

ALGOS="fcfs sjf priority_np priority_preemp rr rm edf"
REPS=10

echo "algoritmo,run,total_processes_completed,avg_waiting_time,avg_turnaround_time,cpu_utilization,throughput,deadline_misses"

for algo in $ALGOS; do
  for i in $(seq 1 $REPS); do
    # Para RR, usa quantum 2; para RM/EDF, usa max 50
    if [ "$algo" = "rr" ]; then
      OUT=$(./_build/default/bin/prob_sched.exe --algo $algo --gen 10 --quantum 2)
    elif [ "$algo" = "rm" ] || [ "$algo" = "edf" ]; then
      OUT=$(./_build/default/bin/prob_sched.exe --algo $algo --gen 10 --max 50)
    else
      OUT=$(./_build/default/bin/prob_sched.exe --algo $algo --gen 10)
    fi

    # Extrai estatísticas do JSON
    total=$(echo "$OUT" | grep -o '"total_processes_completed":[0-9]*' | cut -d: -f2)
    wait=$(echo "$OUT" | grep -o '"avg_waiting_time":[0-9.]*' | cut -d: -f2)
    tat=$(echo "$OUT" | grep -o '"avg_turnaround_time":[0-9.]*' | cut -d: -f2)
    cpu=$(echo "$OUT" | grep -o '"cpu_utilization":[0-9.]*' | cut -d: -f2)
    thr=$(echo "$OUT" | grep -o '"throughput":[0-9.]*' | cut -d: -f2)
    miss=$(echo "$OUT" | grep -o '"deadline_misses":[0-9]*' | cut -d: -f2)

    echo "$algo,$i,$total,$wait,$tat,$cpu,$thr,$miss"
  done
done