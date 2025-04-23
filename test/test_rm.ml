(* Test file for Rate Monotonic scheduling *)
open Prob_sched_lib
open OUnit2

(* Helper functions *)
let print_process_list processes =
  Printf.printf "Process List:\n";
  List.iter (fun p ->
    Printf.printf "Process %d: arrival=%d, burst=%d, priority=%d, period=%s, deadline=%s\n"
      p.Process.id
      p.Process.arrival_time
      p.Process.burst_time
      p.Process.priority
      (match p.Process.period with Some p -> string_of_int p | None -> "None")
      (match p.Process.deadline with Some d -> string_of_int d | None -> "None")
  ) processes;
  Printf.printf "\n"

let print_timeline timeline =
  Printf.printf "Timeline:\n";
  List.iter (fun event ->
    Printf.printf "Time %d: Process %d -> %s\n"
      event.Scheduler.time
      event.Scheduler.process_id
      (Process.string_of_state event.Scheduler.new_state)
  ) timeline;
  Printf.printf "\n"

(* Test case for process instantiation *)
let test_process_instantiation _ =
  (* Create same processes as in processos_0md.csv *)
  let p1 = Process.create ~id:1 ~arrival_time:0 ~burst_time:1 ~priority:1 
                         ?period:(Some 5) ?deadline:(Some 5) () in
  let p2 = Process.create ~id:2 ~arrival_time:0 ~burst_time:2 ~priority:2
                         ?period:(Some 8) ?deadline:(Some 8) () in
  let p3 = Process.create ~id:3 ~arrival_time:0 ~burst_time:2 ~priority:3
                         ?period:(Some 14) ?deadline:(Some 14) () in
  
  (* Verify processes are created correctly *)
  assert_equal 1 p1.Process.id;
  assert_equal 0 p1.Process.arrival_time;
  assert_equal 1 p1.Process.burst_time;
  assert_equal (Some 5) p1.Process.period;
  
  assert_equal 2 p2.Process.id;
  assert_equal 0 p2.Process.arrival_time;
  assert_equal 2 p2.Process.burst_time;
  assert_equal (Some 8) p2.Process.period;
  
  assert_equal 3 p3.Process.id;
  assert_equal 0 p3.Process.arrival_time;
  assert_equal 2 p3.Process.burst_time;
  assert_equal (Some 14) p3.Process.period;
  
  print_process_list [p1; p2; p3]

(* Test case for priority assignment based on periods *)
let test_priority_assignment _ =
  (* Create a priority queue using the RM criteria *)
  let pq = Priority_queue.create (fun p ->
    match p.Process.period with
    | Some period -> period  (* smaller period = higher priority *)
    | None -> failwith "Missing period for RM scheduling"
  ) in
  
  (* Add processes with different periods *)
  let p1 = Process.create ~id:1 ~arrival_time:0 ~burst_time:1 ~priority:1 
                         ?period:(Some 5) ?deadline:(Some 5) () in
  let p2 = Process.create ~id:2 ~arrival_time:0 ~burst_time:2 ~priority:2
                         ?period:(Some 8) ?deadline:(Some 8) () in
  let p3 = Process.create ~id:3 ~arrival_time:0 ~burst_time:2 ~priority:3
                         ?period:(Some 14) ?deadline:(Some 14) () in
  
  Priority_queue.add pq p3; (* Add in reverse order to test sorting *)
  Priority_queue.add pq p2;
  Priority_queue.add pq p1;
  
  (* Verify the extraction order follows RM priority (shortest period first) *)
  let first = Priority_queue.take pq in
  let second = Priority_queue.take pq in
  let third = Priority_queue.take pq in
  
  assert_equal 1 first.Process.id "Process with shortest period (5) should be extracted first";
  assert_equal 2 second.Process.id "Process with medium period (8) should be extracted second";
  assert_equal 3 third.Process.id "Process with longest period (14) should be extracted last";
  
  Printf.printf "RM Priority Extraction Order: P%d -> P%d -> P%d\n" 
    first.Process.id second.Process.id third.Process.id

(* Test case for periodic instance generation *)
let test_periodic_instances _ =
  (* Create same processes as in processos_0md.csv *)
  let p1 = Process.create ~id:1 ~arrival_time:0 ~burst_time:1 ~priority:1 
                         ?period:(Some 5) ?deadline:(Some 5) () in
  let p2 = Process.create ~id:2 ~arrival_time:0 ~burst_time:2 ~priority:2
                         ?period:(Some 8) ?deadline:(Some 8) () in
  let p3 = Process.create ~id:3 ~arrival_time:0 ~burst_time:2 ~priority:3
                         ?period:(Some 14) ?deadline:(Some 14) () in
  
  let processes = [p1; p2; p3] in
  let max_time = 20 in (* Use smaller time for testing *)
  
  (* Generate periodic instances *)
  let instances = Scheduler.gerar_instancias_periodicas processes max_time in
  
  Printf.printf "Generated %d periodic instances for max_time=%d\n" (List.length instances) max_time;
  print_process_list instances;
  
  (* Count instances by original process ID (stored in priority field) *)
  let p1_instances = List.filter (fun p -> p.Process.priority = 1) instances in
  let p2_instances = List.filter (fun p -> p.Process.priority = 2) instances in
  let p3_instances = List.filter (fun p -> p.Process.priority = 3) instances in
  
  (* Expected instance counts based on periods:
     - P1 (period 5): instances at t=0,5,10,15 -> 4 instances
     - P2 (period 8): instances at t=0,8,16 -> 3 instances
     - P3 (period 14): instances at t=0,14 -> 2 instances
  *)
  Printf.printf "Process 1 (period 5) instances: %d\n" (List.length p1_instances);
  Printf.printf "Process 2 (period 8) instances: %d\n" (List.length p2_instances);
  Printf.printf "Process 3 (period 14) instances: %d\n" (List.length p3_instances);
  
  assert_equal 4 (List.length p1_instances) "Process 1 should have 4 instances";
  assert_equal 3 (List.length p2_instances) "Process 2 should have 3 instances";
  assert_equal 2 (List.length p3_instances) "Process 3 should have 2 instances";
  
  (* Verify instance arrival times match periods *)
  let p1_arrivals = List.map (fun p -> p.Process.arrival_time) p1_instances |> List.sort compare in
  let p2_arrivals = List.map (fun p -> p.Process.arrival_time) p2_instances |> List.sort compare in
  let p3_arrivals = List.map (fun p -> p.Process.arrival_time) p3_instances |> List.sort compare in
  
  assert_equal [0; 5; 10; 15] p1_arrivals "P1 instances should arrive at t=0,5,10,15";
  assert_equal [0; 8; 16] p2_arrivals "P2 instances should arrive at t=0,8,16";
  assert_equal [0; 14] p3_arrivals "P3 instances should arrive at t=0,14"

(* Test the actual rate monotonic scheduling algorithm *)
let test_rm_scheduling _ =
  (* Create same processes as in processos_0md.csv *)
  let p1 = Process.create ~id:1 ~arrival_time:0 ~burst_time:1 ~priority:1 
                         ?period:(Some 5) ?deadline:(Some 5) () in
  let p2 = Process.create ~id:2 ~arrival_time:0 ~burst_time:2 ~priority:2
                         ?period:(Some 8) ?deadline:(Some 8) () in
  let p3 = Process.create ~id:3 ~arrival_time:0 ~burst_time:2 ~priority:3
                         ?period:(Some 14) ?deadline:(Some 14) () in
  
  let processes = [p1; p2; p3] in
  let max_time = 20 in (* Use smaller time for testing *)
  
  (* Run RM scheduling *)
  let final_time, timeline = Scheduler.rate_monotonic processes ~tempo_max:max_time in
  
  Printf.printf "RM Scheduling completed at time %d\n" final_time;
  print_timeline timeline;
  
  (* Count executions by process ID - this is a key verification *)
  let running_events = List.filter (fun e -> e.Scheduler.new_state = Process.Running) timeline in
  let p1_runs = List.filter (fun e -> e.Scheduler.process_id = 1) running_events in
  let p2_runs = List.filter (fun e -> e.Scheduler.process_id = 2) running_events in
  let p3_runs = List.filter (fun e -> e.Scheduler.process_id = 3) running_events in
  
  Printf.printf "Process 1 runs: %d\n" (List.length p1_runs);
  Printf.printf "Process 2 runs: %d\n" (List.length p2_runs);
  Printf.printf "Process 3 runs: %d\n" (List.length p3_runs);
  
  (* We expect P1 (highest priority, period=5) to have the most executions *)
  assert_bool "Process 1 should have more runs than P2 and P3" 
    ((List.length p1_runs) > (List.length p2_runs) && 
     (List.length p1_runs) > (List.length p3_runs));
  
  (* Verify the beginning of execution follows RM priority *)
  match timeline with
  | first::second::_ when 
      first.Scheduler.new_state = Process.Running && 
      second.Scheduler.new_state = Process.Running ->
      (* The first process to run should be P1 (highest priority) *)
      assert_equal 1 first.Scheduler.process_id "First running process should be P1 (highest priority)";
  | _ -> assert_failure "Timeline doesn't start with expected running processes"

(* Test case for debugging the problem with Process 1 *)
let test_missing_p1_debug _ =
  (* Directly simulate the RM scheduler with exact CSV file processes *)
  let p1 = Process.create ~id:1 ~arrival_time:0 ~burst_time:1 ~priority:1 
                         ?period:(Some 5) ?deadline:(Some 5) () in
  let p2 = Process.create ~id:2 ~arrival_time:0 ~burst_time:2 ~priority:2
                         ?period:(Some 8) ?deadline:(Some 8) () in
  let p3 = Process.create ~id:3 ~arrival_time:0 ~burst_time:2 ~priority:3
                         ?period:(Some 14) ?deadline:(Some 14) () in
  
  let processes = [p1; p2; p3] in
  let max_time = 30 in (* Use smaller time for detailed analysis *)
  
  (* Run RM scheduling and trace the execution *)
  let final_time, timeline = Scheduler.rate_monotonic processes ~tempo_max:max_time in
  
  (* Add visibility into the process instances being generated *)
  let instances = Scheduler.get_all_instances () in
  let completed = Scheduler.get_completed_instances () in
  
  Printf.printf "Total instances generated: %d\n" (List.length instances);
  Printf.printf "Total instances completed: %d\n" (List.length completed);
  
  (* Check for P1 instances specifically (using priority field which stores original process ID) *)
  let p1_instances = List.filter (fun p -> p.Process.priority = 1) instances in
  let p1_completed = List.filter (fun p -> p.Process.priority = 1) completed in
  
  Printf.printf "P1 instances generated: %d\n" (List.length p1_instances);
  Printf.printf "P1 instances completed: %d\n" (List.length p1_completed);
  
  (* Check the timeline for P1 executions *)
  let p1_events = List.filter (fun e -> 
    e.Scheduler.process_id = 1 || 
    (e.Scheduler.process_id >= 1000 && e.Scheduler.process_id < 2000)
  ) timeline in
  
  Printf.printf "P1 events in timeline: %d\n" (List.length p1_events);
  List.iter (fun e ->
    Printf.printf "Time %d: P1 -> %s\n" 
      e.Scheduler.time (Process.string_of_state e.Scheduler.new_state)
  ) p1_events;
  
  (* Assert that there are P1 executions in the timeline *)
  assert_bool "P1 should have executions in the timeline" ((List.length p1_events) > 0)

(* Suite definition *)
let suite = 
  "Rate Monotonic Tests" >::: [
    "test_process_instantiation" >:: test_process_instantiation;
    "test_priority_assignment" >:: test_priority_assignment;
    "test_periodic_instances" >:: test_periodic_instances;
    "test_rm_scheduling" >:: test_rm_scheduling;
    "test_missing_p1_debug" >:: test_missing_p1_debug;
  ]

(* Run the tests *)
let () =
  run_test_tt_main suite

