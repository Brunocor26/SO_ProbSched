type stats = {
  waiting_time : float;
  turnaround_time : float;
}

type result = {
  schedule : (float * int) list;  (* tempo, id do processo *)
  stats_list : (int * stats) list;
}

let sort_by f lst = List.sort (fun a b -> compare (f a) (f b)) lst

let fcfs (processes : process list) : result =
  let time = ref 0. in
  let schedule = ref [] in
  let stats_list = ref [] in
  let sorted = sort_by (fun p -> p.arrival_time) processes in
  List.iter (fun p ->
    if !time < p.arrival_time then time := p.arrival_time;
    schedule := !schedule @ [(!time, p.id)];
    let waiting_time = !time -. p.arrival_time in
    let turnaround_time = waiting_time +. p.burst_time in
    stats_list := (p.id, { waiting_time; turnaround_time }) :: !stats_list;
    time := !time +. p.burst_time
  ) sorted;
  { schedule = !schedule; stats_list = List.rev !stats_list }

let sjf (processes : process list) : result =
  let ready = ref [] in
  let time = ref 0. in
  let schedule = ref [] in
  let stats_list = ref [] in
  let not_arrived = ref (sort_by (fun p -> p.arrival_time) processes) in
  while !ready <> [] || !not_arrived <> [] do
    while !not_arrived <> [] && (List.hd !not_arrived).arrival_time <= !time do
      let p = List.hd !not_arrived in
      ready := p :: !ready;
      not_arrived := List.tl !not_arrived
    done;
    if !ready = [] then (
      time := (List.hd !not_arrived).arrival_time
    ) else (
      ready := sort_by (fun p -> p.burst_time) !ready;
      let p = List.hd !ready in
      ready := List.tl !ready;
      schedule := !schedule @ [(!time, p.id)];
      let waiting_time = !time -. p.arrival_time in
      let turnaround_time = waiting_time +. p.burst_time in
      stats_list := (p.id, { waiting_time; turnaround_time }) :: !stats_list;
      time := !time +. p.burst_time
    )
  done;
  { schedule = !schedule; stats_list = List.rev !stats_list }

let priority_non_preemptive (processes : process list) : result =
  let ready = ref [] in
  let time = ref 0. in
  let schedule = ref [] in
  let stats_list = ref [] in
  let not_arrived = ref (sort_by (fun p -> p.arrival_time) processes) in
  while !ready <> [] || !not_arrived <> [] do
    while !not_arrived <> [] && (List.hd !not_arrived).arrival_time <= !time do
      let p = List.hd !not_arrived in
      ready := p :: !ready;
      not_arrived := List.tl !not_arrived
    done;
    if !ready = [] then (
      time := (List.hd !not_arrived).arrival_time
    ) else (
      ready := sort_by (fun p -> p.priority) !ready;
      let p = List.hd !ready in
      ready := List.tl !ready;
      schedule := !schedule @ [(!time, p.id)];
      let waiting_time = !time -. p.arrival_time in
      let turnaround_time = waiting_time +. p.burst_time in
      stats_list := (p.id, { waiting_time; turnaround_time }) :: !stats_list;
      time := !time +. p.burst_time
    )
  done;
  { schedule = !schedule; stats_list = List.rev !stats_list }

let priority_preemptive (processes : process list) : result =
  let all = ref (sort_by (fun p -> p.arrival_time) processes) in
  let time = ref 0. in
  let schedule = ref [] in
  let stats_tbl = Hashtbl.create 16 in
  let ready = ref [] in
  while !all <> [] || !ready <> [] do
    while !all <> [] && (List.hd !all).arrival_time <= !time do
      let p = List.hd !all in
      ready := p :: !ready;
      all := List.tl !all
    done;
    if !ready = [] then (
      time := (List.hd !all).arrival_time
    ) else (
      ready := sort_by (fun p -> p.priority) !ready;
      let p = List.hd !ready in
      schedule := !schedule @ [(!time, p.id)];
      p.remaining_time <- p.remaining_time -. 1.;
      time := !time +. 1.;
      if p.remaining_time <= 0. then (
        ready := List.tl !ready;
        let waiting_time = !time -. p.arrival_time -. p.burst_time in
        let turnaround_time = !time -. p.arrival_time in
        Hashtbl.add stats_tbl p.id { waiting_time; turnaround_time }
      )
    )
  done;
  let stats_list = Hashtbl.fold (fun k v acc -> (k,v)::acc) stats_tbl [] in
  { schedule = !schedule; stats_list }

let round_robin (processes : process list) ~(quantum:float) : result =
  let queue = Queue.create () in
  let not_arrived = ref (sort_by (fun p -> p.arrival_time) processes) in
  let time = ref 0. in
  let schedule = ref [] in
  let stats_tbl = Hashtbl.create 16 in
  while Queue.length queue > 0 || !not_arrived <> [] do
    while !not_arrived <> [] && (List.hd !not_arrived).arrival_time <= !time do
      let p = List.hd !not_arrived in
      Queue.add p queue;
      not_arrived := List.tl !not_arrived
    done;
    if Queue.is_empty queue then
      time := (List.hd !not_arrived).arrival_time
    else (
      let p = Queue.pop queue in
      schedule := !schedule @ [(!time, p.id)];
      let run_time = min quantum p.remaining_time in
      p.remaining_time <- p.remaining_time -. run_time;
      time := !time +. run_time;
      while !not_arrived <> [] && (List.hd !not_arrived).arrival_time <= !time do
        let p = List.hd !not_arrived in
        Queue.add p queue;
        not_arrived := List.tl !not_arrived
      done;
      if p.remaining_time > 0. then Queue.add p queue
      else (
        let waiting_time = !time -. p.arrival_time -. p.burst_time in
        let turnaround_time = !time -. p.arrival_time in
        Hashtbl.add stats_tbl p.id { waiting_time; turnaround_time }
      )
    )
  done;
  let stats_list = Hashtbl.fold (fun k v acc -> (k,v)::acc) stats_tbl [] in
  { schedule = !schedule; stats_list }

let rate_monotonic (processes : process list) : result =
  let periodic = List.filter (fun p -> match p.deadline with Some _ -> true | _ -> false) processes in
  let periodic = sort_by (fun p -> match p.deadline with Some d -> d | _ -> infinity) periodic in
  priority_preemptive periodic

let edf (processes : process list) : result =
  let ready = ref [] in
  let all = ref (sort_by (fun p -> p.arrival_time) processes) in
  let time = ref 0. in
  let schedule = ref [] in
  let stats_tbl = Hashtbl.create 16 in
  while !ready <> [] || !all <> [] do
    while !all <> [] && (List.hd !all).arrival_time <= !time do
      ready := (List.hd !all) :: !ready;
      all := List.tl !all
    done;
    if !ready = [] then time := (List.hd !all).arrival_time
    else (
      ready := sort_by (fun p -> match p.deadline with Some d -> d | None -> infinity) !ready;
      let p = List.hd !ready in
      schedule := !schedule @ [(!time, p.id)];
      p.remaining_time <- p.remaining_time -. 1.;
      time := !time +. 1.;
      if p.remaining_time <= 0. then (
        ready := List.tl !ready;
        let waiting_time = !time -. p.arrival_time -. p.burst_time in
        let turnaround_time = !time -. p.arrival_time in
        Hashtbl.add stats_tbl p.id { waiting_time; turnaround_time }
      )
    )
  done;
  let stats_list = Hashtbl.fold (fun k v acc -> (k,v)::acc) stats_tbl [] in
  { schedule = !schedule; stats_list }
