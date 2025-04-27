#!/bin/bash

REPS=10

echo "algoritmo,run,total_processes_completed,avg_waiting_time,avg_turnaround_time,cpu_utilization,throughput,deadline_misses"

for i in $(seq 1 $REPS); do
  OUT=$(./_build/default/bin/prob_sched.exe --algo rr --gen 10 --quantum 4)

  total=$(echo "$OUT" | grep -o '"total_processes_completed":[0-9]*' | cut -d: -f2)
  wait=$(echo "$OUT" | grep -o '"avg_waiting_time":[0-9.]*' | cut -d: -f2)
  tat=$(echo "$OUT" | grep -o '"avg_turnaround_time":[0-9.]*' | cut -d: -f2)
  cpu=$(echo "$OUT" | grep -o '"cpu_utilization":[0-9.]*' | cut -d: -f2)
  thr=$(echo "$OUT" | grep -o '"throughput":[0-9.]*' | cut -d: -f2)
  miss=$(echo "$OUT" | grep -o '"deadline_misses":[0-9]*' | cut -d: -f2)

  echo "rr,$i,$total,$wait,$tat,$cpu,$thr,$miss"
done