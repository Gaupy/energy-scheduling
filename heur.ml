open Const
open Usr
open Doc
open Schedule
open Format

let vCritH = Array.make 5 (VH1.empty)

let sol_heur0 config param eDag vDag h deadlineMAX = 
    let break = ref (-1) in
      while not (!break = sumBool config.tabSlowed.(h)) do
        break:= sumBool config.tabSlowed.(h) ;
        while (!break=sumBool config.tabSlowed.(h))&&(not (VH1.is_empty vCritH.(h))) do
          let maxEvC = max_elt_heuristic config vDag vCritH.(h) in
          let available_time = available_time_fun config eDag h deadlineMAX maxEvC in
          let f1 = computef1 param available_time in 
          let f2 = computef2 param available_time in 
            (try
               if not config.tabSlowed.(h).(maxEvC) then (slowTwoExec config param eDag vDag h maxEvC f1 f2 available_time) ;
             with
               | Not_Enough_Time -> ()
               | Not_Enough_Reliability -> () );
	  (*here is the difference with the next heuristic ->*)
              ( if config.tabSlowed.(h).(maxEvC) then 
                (slowOneExec config param eDag vDag h maxEvC available_time ; 
                 initCompTime config param eDag h)
              );
            let vCritRef = vCritH.(h) in
               vCritH.(h) <- (VH1.remove maxEvC vCritRef) ; 
        done;
      done;
    let list_greedy = List.sort (fun x y -> (weight_compare config) y x) (V.elements vDag) in
    List.iter (slowGlouton config param eDag h deadlineMAX) list_greedy ;
    List.iter (slowGlouton2 config param eDag h deadlineMAX) list_greedy 
(*be careful, in slowGlouton is a part attributed to this heuristic. We need to modify the id directly in slowGlouton if we change the id of this heuristic. *)


let sol_heur1 config param eDag vDag h deadlineMAX =
    let break = ref (-1) in
      while not (!break = sumBool config.tabSlowed.(h)) do
        break:= sumBool config.tabSlowed.(h);
        while (!break=sumBool config.tabSlowed.(h))&&(not (VH1.is_empty vCritH.(h))) do
          let maxEvC = (max_elt_heuristic config vDag vCritH.(h)) in
          let available_time = available_time_fun config eDag h deadlineMAX maxEvC in
          let f1 = computef1 param available_time in
          let f2 = computef2 param available_time in 
            (try
               if not config.tabSlowed.(h).(maxEvC) then (slowTwoExec config param eDag vDag h maxEvC f1 f2 available_time) 
             with
               | Not_Enough_Time -> ()
               | Not_Enough_Reliability -> ()  );
	    if h=0 then (
            if config.tabSlowed.(h).(maxEvC) then (slowOneExec config param eDag vDag h maxEvC available_time)) ;
            let vCritRef = vCritH.(h) in
               vCritH.(h) <- (VH1.remove maxEvC vCritRef) ; 
        done;
      done;
    let list_greedy = List.sort (fun x y -> (weight_compare config) y x) (V.elements vDag) in
    List.iter (slowGlouton config param eDag h deadlineMAX) list_greedy ;
    List.iter (slowGlouton2 config param eDag h deadlineMAX) list_greedy ;
    (*here is the difference with the previous heuristic ->*)
    (let available_time id = available_time_fun config eDag h deadlineMAX id in
     let slowList1 id = slowList1Ex config param h id (available_time id) in
     List.iter slowList1 list_greedy )


let sol_heur2 config param eDag vDag h deadlineMAX =
(* the difference with the previous heuristic is here, no first step *)
    let list_greedy = List.sort (fun x y -> (weight_compare config) y x) (V.elements vDag) in
    List.iter (slowGlouton config param eDag h deadlineMAX) list_greedy ;
    List.iter (slowGlouton2 config param eDag h deadlineMAX) list_greedy ;
    let available_time = available_time_fun config eDag h deadlineMAX in
    let slowList1 id = slowList1Ex config param h id (available_time id) in
    List.iter slowList1 list_greedy

let sol_heur3 config param eDag vDag h deadlineMAX =
  let f = max param.fr (1./. param.ratio_deadlineMAX) in
  for i=0 to (param.n-1) do
  let _,f2 = config.tabSlowedFreq.(h).(i) in 
    config.tabSlowedFreq.(h).(i) <- (f,f2)
  done ;


    for i= 0 to (param.n-1) do
      let (time, l) = config.tabFinishTime.(h).(i) in
      let time' = time *. param.fmax /. f in
      config.tabFinishTime.(h).(i) <- (time', l);
    done ; initCompTime config param eDag h ; 

    let list_greedy = List.sort (fun x y -> (weight_compare config) y x) (V.elements vDag) in
    List.iter (slowGlouton config param eDag h deadlineMAX) list_greedy ;
    List.iter (slowGlouton2 config param eDag h deadlineMAX) list_greedy 

(*let greedy_compare config id1 id2 =
  if ((config.tabTask.(0).(id1)).work < (config.tabTask.(0).(id2)).work) then -1
  else if ((config.tabTask.(0).(id1)).work > (config.tabTask.(0).(id2)).work) then 1
  else if id1 < id2 then -1
  else if id1 = id2 then 0
  else 1*)

let sol_heur4 config param eDag vDag h deadlineMAX =
  let f = max param.fr (1./.param.ratio_deadlineMAX) in

  for i=0 to (param.n-1) do
  let _,f2 = config.tabSlowedFreq.(h).(i) in 
    config.tabSlowedFreq.(h).(i) <- (f,f2)
  done ; 

    for i= 0 to (param.n-1) do
      let (time, l) = config.tabFinishTime.(h).(i) in
      let time' = time *. param.fmax /. f in
      config.tabFinishTime.(h).(i) <- (time', l);
    done ; initCompTime config param eDag h ; 

    let break = ref (-1) in
      while not (!break = sumBool config.tabSlowed.(h)) do
        break:= sumBool config.tabSlowed.(h) ;
        while (!break=sumBool config.tabSlowed.(h))&&(not (VH1.is_empty vCritH.(h))) do
          let maxEvC = max_elt_heuristic config vDag vCritH.(h) in
          let available_time = available_time_fun config eDag h deadlineMAX maxEvC in
          let f1 = computef1 param available_time in
          let f2 = computef2 param available_time in 
            (try
               if not config.tabSlowed.(h).(maxEvC) then (slowTwoExec config param eDag vDag h maxEvC f1 f2 available_time) 
             with
               | Not_Enough_Time -> ()
               | Not_Enough_Reliability -> ()  );
	    if h=0 then (
            if config.tabSlowed.(h).(maxEvC) then (slowOneExec config param eDag vDag h maxEvC available_time)) ;
            let vCritRef = vCritH.(h) in
               vCritH.(h) <- (VH1.remove maxEvC vCritRef) ; 
        done;
      done;
    let list_greedy = List.sort (fun x y -> (weight_compare config) y x) (V.elements vDag) in
    List.iter (slowGlouton config param eDag h deadlineMAX) list_greedy ;
    List.iter (slowGlouton2 config param eDag h deadlineMAX) list_greedy 

