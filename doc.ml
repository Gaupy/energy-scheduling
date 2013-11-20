open Const
open Usr
open Format
    
(* FUNCTIONS USED TO FIND THE STATE IN TE DIFFERNET ARRAYS *)

let array_find p arr =
  let l = Array.length arr in
  let i = ref 0 in
  let break = ref true in
    while !break && !i < l do
      if p arr.(!i) then break := false
      else i:=!i+1
    done;
    if not !break then !i
    else raise Not_found
      
      
let findId arr id =
  let p (_, l) = V.mem id l in
  array_find p arr

let findIBegin config heur = findId config.tabStartTime.(heur)

let findIFinish config heur = findId config.tabFinishTime.(heur)


(*Sorting modules depending on the heuristics*)
let propComp config heur id id' = 
    let i1 = findIBegin config heur id in
    let i' = findIBegin config heur id' in
    let j1 = findIFinish config heur id in
    let j' = findIFinish config heur id' in
      ((i1 <= i') && (j'<=j1))


let weight_compare config id1 id2 =
    if ((config.tabTask.(0).(id1)).work < (config.tabTask.(0).(id2)).work) then -1
    else if ((config.tabTask.(0).(id1)).work > (config.tabTask.(0).(id2)).work) then 1
    else if id1 < id2 then -1
    else if id1 = id2 then 0
    else 1



let compare_heuristic1 config vDag id1 id2 =
  if (id1 = id2) then 0
  else (
    let s1 = (V.filter (propComp config 0 id1) vDag) in 
    let s2 = (V.filter (propComp config 0 id2) vDag) in
      let w1 = V.fold (heurFun1 config) s1 0. in
      let w2 = V.fold (heurFun1 config) s2 0. in
      if (w1 > w2) then 1 (*t1>> t2*)  (*here we compare according to W1 ?>? W2 (super-weight) *)
      else if (w1 < w2) then -1 (*t1 << t2*)
      else if (id1 < id2) then -1
      else 1)

let max_elt_heuristic config vDag v =
  let e = VH1.choose v in
  let v = VH1.remove e v in
  let fold elt accu =
    if compare_heuristic1 config vDag accu elt < 0 then elt else accu
  in
  VH1.fold fold v e

(* module VH1 = Set.Make(Heur1) *)
  
(* CRITICAL PATHS TO FILL VCRITH.(H) *)
let execTime config heur id =
  let f1,f2 = config.tabSlowedFreq.(heur).(id) in 
  if config.tabSlowed.(heur).(id) then ((config.tabTask.(heur).(id)).work *. (1./.f1 +. 1./.f2))
  else (config.tabTask.(heur).(id)).work/.f1
      

let array_update_add arr j time id = (*works on both arrays tabBeginTime and tabFinishTime ?*)
  let l= Array.length arr in
  let i = ref j in
  let found = ref false in
    while not !found && !i < l  do
      let (time', list) = arr.(!i) in
      if V.is_empty list then (arr.(!i) <- time, V.singleton id; found := true)
      else
	    if time = time' then (arr.(!i) <- (time, V.add id list) ; found := true)
	    else if time' > time then 
	      (
		 for k = l - 2 downto !i do
			arr.(k + 1) <- arr.(k)
		 done;
		 arr.(!i) <- time, V.singleton id; found := true ;
		)
	    else i:=!i+1
    done


let array_update_remove arr j id = (*works on both arrays tabBeginTime and tabFinishTime ?*)
  let l = Array.length arr in
  let (f, s) = arr.(j) in
  let s = V.remove id s in
  if V.is_empty s then
	  begin
	      ( for i = j to l-2 do
		  arr.(i) <- arr.(i+1)
		done;
		arr.(l-1)<- (0., V.empty)
	      )
	  end
   else
     arr.(j) <- (f, s)

let initCompTime config param eDag heur = 
  let break = ref false in
  let cptr = ref 0 in
    while (not !break) && (!cptr < param.n+3) do
      break := true ;
      let f e =
	let j1 = findIFinish config heur (e.id1) in 
	let i2 = findIBegin config heur (e.id2) in
	let (d1, _) = config.tabFinishTime.(heur).(j1) in
	let (r2, _) = config.tabStartTime.(heur).(i2) in
	  if d1 <= r2 then ()(* printf "Break = %B a la %i non-modif \n" !break !cptr *)
	  else (break := false ;
		let j2 = findIFinish config heur (e.id2) in
		  array_update_remove config.tabStartTime.(heur) i2 (e.id2);
		  array_update_remove config.tabFinishTime.(heur) j2 (e.id2);
		  array_update_add config.tabStartTime.(heur) i2 d1 e.id2;
		  array_update_add config.tabFinishTime.(heur) j2 (d1 +. execTime config heur e.id2) e.id2;
	       )
      in
	E.iter f eDag;
        cptr := !cptr +1 ;
    done;
if !cptr = param.n+3 then failwith "this is not a DAG"     


  
	   
let rec critTasks0 config eDag heur vCrit m = (* sort every critical tasks with super-weight function *)
  if m < 0 then vCrit
  else 
    if V.is_empty (snd (config.tabFinishTime.(heur).(m))) then critTasks0 config eDag heur vCrit (m-1)
    else (* otherwise, tabFinishTime.(heur).(m) belong to a critical path since they are the last one to finish computation.*)
      let (d, set) = config.tabFinishTime.(heur).(m) in
      let critTasksR_fold id vCr =
	     if VH1.mem id vCr then vCr (* has a task been assigned to one of the critical paths? if it has, then we ignore it. *)
	     else (* else, which tasks make this one critical? its predecessor with the greater finishtime. *)
	       ( let vCr'= (VH1.add id vCr) in (* So we do not count this task twice, we add it in the set of the critical tasks. *)
		 let setEfilt = E.filter (fun t -> (t.id2 = id)) eDag in 
                 let listSet_fold edge (maxdeadline, accu) =
                   let i1 = findIFinish config heur edge.id1 in
		   let deadline = fst config.tabFinishTime.(heur).(i1) in
                   if maxdeadline <= deadline then (deadline, Some i1)
                   else (maxdeadline, accu)
	         in
                 match E.fold listSet_fold setEfilt (0., None) with
		 | (_, None) -> vCr'
		 | (_, Some i_idKept) ->
			  critTasks0 config eDag heur vCr' i_idKept 
	       )
      in
	V.fold critTasksR_fold set  vCrit





let critTasks config param eDag heur vCrit = (*modifies vCritH (array) so that it contains all the tasks in critical pathes of our initial DAG. m is a shortcut to make sure there are tasks that finishes in m position.*)
  match heur with
  | 0 -> critTasks0 config eDag 0 vCrit (param.n-1)
  | 1 -> critTasks0 config eDag 1 vCrit (param.n-1)
  | 2 -> critTasks0 config eDag 2 vCrit (param.n-1)
  | 3 -> critTasks0 config eDag 3 vCrit (param.n-1)
  | 4 -> critTasks0 config eDag 4 vCrit (param.n-1)
  | _ -> failwith "This heuristic does not exist yet." (* when adding new heuristics, add "critTaskh heur vCrit (n-1)" *)







let rec slowList2Ex config heur f1 f2 list_of_id =
  match list_of_id with
    |[]-> ()
    |id::list_of_id'-> (
	if (energy id config heur > energyTest config f1 f2 id) then 
	  (* remember to do the reliability test BEFORE slowing every task. *)
	  (config.tabSlowed.(heur).(id)<- true ;
	   config.tabSlowedFreq.(heur).(id)<-f1,f2;		  
	   let i = findIBegin config heur (id) in 
	   let j = findIFinish config heur id in
	   let (r, _) = config.tabStartTime.(heur).(i) in
	     array_update_remove config.tabFinishTime.(heur) j (id);
	     array_update_add (config.tabFinishTime.(heur)) 0 (r +. (execTime config heur id)) id;
	  );
	slowList2Ex config heur f1 f2 list_of_id'
      )
       

let slowList1Ex config param heur id available_time =
  if config.tabSlowed.(heur).(id) then ()
  else let f1,f2 = config.tabSlowedFreq.(heur).(id) in
    let f = config.tabTask.(heur).(id).work /. available_time in
    (match compare param.fr f with
      | -1 (*fr<f*)-> if f1 > f then config.tabSlowedFreq.(heur).(id)<-(f,f2)
      | 0 -> if f1 > param.fr then config.tabSlowedFreq.(heur).(id)<-(param.fr,f2)
      | _ (*fr>f*)-> if f1 > param.fr then config.tabSlowedFreq.(heur).(id)<-(param.fr,f2) );
    let i = findIBegin config heur id in 
    let j = findIFinish config heur id in
    let (r, _) = config.tabStartTime.(heur).(i) in
      array_update_remove config.tabFinishTime.(heur) j (id);
      array_update_add (config.tabFinishTime.(heur)) 0 (r +. (execTime config heur id)) id
	  


exception Not_Enough_Time;;
exception Not_Enough_Reliability ;;


let rec printintlist l= 
  match l with
    |[]-> printf "[] \n" 
    |id::l'-> (printf "%i :: " id ; printintlist l')
       
       

let slowTwoExec config param eDag vDag heur id f1 f2 available_time =
  let work = config.tabTask.(heur).(id).work in
  if work *. (1./.f1 +. 1./.f2) > available_time then raise Not_Enough_Time
  else ( 
    if matchRel config param f1 f2 id then 
      begin
	let s1 = V.filter (propComp config heur id) vDag in
	let elements_s1 = V.elements s1 in 
	  slowList2Ex config heur f1 f2 elements_s1 ;
	  (*	printf "taches modifiees :" ; printintlist elements_s1 ;*)
	  initCompTime config param eDag heur;
      end
    else raise Not_Enough_Reliability
  )

let slowOneExec config param eDag vDag heur id available_time =
  begin
    let s1 = (V.filter (propComp config heur id) vDag) in
    let elements_s1 = V.elements s1 in 
    let slowList t = slowList1Ex config param heur t available_time in
      List.iter slowList elements_s1 ;
    initCompTime config param eDag heur;
   end

let sumBool arr_of_bool =
  let l = Array.length arr_of_bool in
  let sum = ref 0 in
    for i=0 to l-1 do
      if arr_of_bool.(i) then (sum := !sum+1) 
    done;
    !sum
      
let maxTime config param h =
  let sol = ref 0. in
  let i= ref (param.n-1) in
  let break = ref false in
    while ((not !break) && !i> -1) do
	let (endtime, l) = config.tabFinishTime.(h).(!i) in
      if V.is_empty l then 
        i:=!i-1
 	else (break := true ; sol := endtime )
    done;
    !sol

exception Over_Deadline of float

let rec available_time_fun config eDag heur deadlineMAX id visited =
  
  if IntMap.mem id visited then
    (IntMap.find id visited, visited)
  else
    let j_id = findIFinish config heur id in
    let d_id = fst config.tabFinishTime.(heur).(j_id) in
    let base_time = deadlineMAX -.d_id in
    let setEfilt = E.filter (fun t -> (t.id1 = id)) eDag in
    let fold elt (time, visited) =
      let i_id2 = findIBegin config heur elt.id2 in
      let r_id2 = fst config.tabStartTime.(heur).(i_id2) in
      let (av_time_id2, visited) = available_time_fun config eDag heur deadlineMAX elt.id2 visited in
      let new_time = (r_id2 -.d_id) +. av_time_id2 in
      (min time new_time, visited)
    in
    let (time, visited) = E.fold fold setEfilt (base_time, visited) in
    (time, IntMap.add id time visited)

let available_time_fun config eDag heur deadlineMAX id = 

    let (time, _) = available_time_fun config eDag heur deadlineMAX id IntMap.empty in
    if time < 0. then raise (Over_Deadline time) (*failwith "Over_Deadline"*)
	else time

(* GLOUTON MEANS GREEDY *)

let slow2ExGlouton config param heur id available_time =
  if config.tabSlowed.(heur).(id) then
    (let f1,f2 = config.tabSlowedFreq.(heur).(id) in
     let work_id = (config.tabTask.(heur).(id)).work in
     let f = max (sqrt(param.lambda0 *. work_id *. param.fmax)) (2.*.work_id/.available_time) in
      if f <= f1 then config.tabSlowedFreq.(heur).(id)<-f,f ;
      let i = findIBegin config heur (id) in 
      let j = findIFinish config heur id in
      let (r, _) = config.tabStartTime.(heur).(i) in
        array_update_remove config.tabFinishTime.(heur) j (id);
        array_update_add (config.tabFinishTime.(heur)) 0 (r +. (execTime config heur id)) id
     )
      
let slowGlouton config param eDag heur deadlineMAX id = (* List.rev (V.elements !vDag) *)
    if config.tabSlowed.(heur).(id) then ()
    else
	    begin
              let available_time = available_time_fun config eDag heur deadlineMAX id in
	      let f1 = computef1 param available_time in
 	      let f2 = computef2 param available_time in 
                (try
                   if (((config.tabTask.(heur).(id)).work) *. (1./.f1 +. 1./.f2) > available_time) 
                   then raise Not_Enough_Time
                   else ( 
                     if matchRel config param f1 f2 id then 
                       begin 
                         slowList2Ex config heur f1 f2 [id] ;
                         initCompTime config param eDag heur ; 
                       end
                     else raise Not_Enough_Reliability
                   )
                  with
	    	   | Not_Enough_Time -> ()
                   | Not_Enough_Reliability -> () )
                ;
                if heur=0 then ( (*replace here with the id of the heuristic concerned*)
                  if not config.tabSlowed.(heur).(id) then 
                    (slowList1Ex config param heur id available_time ;
                     initCompTime config param eDag heur) ;
                  )
	    end


let slowGlouton2 config param eDag heur deadlineMAX id =
  if config.tabSlowed.(heur).(id) then
    begin
      let available_time = available_time_fun config eDag heur deadlineMAX id in
      slow2ExGlouton config param heur id available_time ;
      initCompTime config param eDag heur
    end
