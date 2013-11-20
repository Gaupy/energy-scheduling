open Const
open Usr
open Doc
open Format




let max_elt_heur vD =
	V.max_elt vD 
(* to modify accordingly with the list heuristic....*)

let max_elt_weight config v =
  let e = V.choose v in
  let v_new = V.remove e v in
  let fold elt accu =
    if weight_compare config accu elt < 0 then elt else accu
  in
  V.fold fold v_new e



let list_schedule config param vD eDag = 
	let rec priority_queue_rev queue set_of_v_with_pred set_of_e =
		if V.is_empty set_of_v_with_pred then queue
 		else begin
			let filter id =
			  E.for_all (fun e ->not (e.id2 = id)) set_of_e  
			in
			let no_pred = V.filter filter set_of_v_with_pred in
			let max_elt = max_elt_weight config no_pred in
			let set_of_v' = (V.remove max_elt set_of_v_with_pred) in
			let set_of_e' = E.filter (fun e -> not (e.id1 = max_elt)) set_of_e in
			priority_queue_rev (max_elt :: queue) set_of_v' set_of_e'
		     end
	in 
		let priority_queue = List.rev (priority_queue_rev [] vD eDag) in
		let array_proc = Array.make param.p (0.,-1) in  (*the first element (float) is the weight already executed on the processor, the second (integer) is the id of the last task executed.*)
		let assign eD elt =
			let j = ref 0 in
				for i = 1 to param.p-1 do
				let a,_ = array_proc.(i) in
				let a',_ = array_proc.(!j) in
				if a<a' then j := i
				else ()
			done ;
			let a,b = array_proc.(!j) in 
				begin
				  array_proc.(!j) <- (a +. config.tabTask.(0).(elt).work, elt) ;
				  (*printf "task %i is executed on proc %i\n"  elt !j ;*)
				  if b=(-1) then eD
				  else E.add {id1 = b; id2 = elt} eD
				end
		in List.fold_left assign eDag priority_queue



let priority_CP config param vD eDag =
	let tab_priority = Array.make param.n 0. in
	for i= 0 to param.n-1 do
		tab_priority.(i) <- ((config.tabTask.(0).(i)).work)
	done;
	let modif = ref true in
	while !modif do
		modif := false ;
		let f e = 
			let new_val = max (tab_priority.(e.id1)) (tab_priority.(e.id2) +. config.tabTask.(0).(e.id1).work) in
			if new_val > tab_priority.(e.id1) then modif := true ;
			tab_priority.(e.id1) <- new_val
		in
		E.iter f eDag
	done;
	tab_priority


let list_sched_crit_path config param vD eDag = 
	let eNew = ref E.empty in
	let tab_priority = priority_CP config param vD eDag in	
	let array_proc = Array.make param.p (0.,true,-1) in  (* (time of the end of the execution, bool working ?, last executed task) *)


	let vDone = ref V.empty in
	let vWorking = ref V.empty in
	let vAvailable = ref V.empty in
	let vTodo = ref vD in
	let time = ref 0. in

	let first_proc av_proc = (* assigns the first available proc *)
		let new_time = ref 0. in 
		let past_task = ref (-1) in 
		let proc = ref (-1) in
		for i= 0 to param.p -1 do
			let (t,b,task) = array_proc.(i) in 
			if not b then ()
			else
				if !proc < 0 then (new_time := t ; past_task := task ; proc := i)
				else (if t < !new_time then (new_time := t ; past_task := task ; proc := i))
		done;
		if !past_task = -1 then ()
		else (  vDone := V.add !past_task !vDone;
			vWorking := V.remove !past_task !vWorking;
		     ) ;
		array_proc.(!proc) <- (!new_time,false,!past_task);
		if !time > !new_time then failwith "erreur dans list scheduling" else time := !new_time;		
		(!proc::av_proc)
		     
	in

	let update_available eD = (* Modifies the available tasks with todo tasks, when we added a new task to the completed tasks (vDone) *)
		let filter v = E.for_all (fun e -> if e.id2 = v then V.mem e.id1 !vDone else true) eD in
		let v1,v2 = V.partition filter !vTodo in
			vAvailable := V.union !vAvailable v1 ;
			vTodo := v2
	in

	let rec actu_proc v av_proc = (* assigns available tasks on the available procs *)
		if V.is_empty v then av_proc
		else 
			match av_proc with
				| [] -> av_proc
				| p :: av_proc' ->
					let fold elt accu =
						if tab_priority.(elt) > tab_priority.(accu) then elt else accu
					in
					let task = V.choose v in
					let new_elt = V.fold fold v task in
					let (_,_,id) = array_proc.(p) in
					let new_id = (config.tabTask.(0).(new_elt)).id in
						((if id < 0 then () else eNew := E.add {id1=id; id2 =new_id} !eNew );
						 (array_proc.(p) <- (!time +. (config.tabTask.(0).(new_elt)).work ,true,new_id) ;
						 vWorking := V.add new_id !vWorking ;
						 vAvailable := V.remove new_id !vAvailable ;
						 (*printf "la tache %i est sur le proc %i\n %!"  new_id p ;*)
						 actu_proc (V.remove new_elt v) av_proc') )
	in

	let available_proc = ref [] in
	while not (V.cardinal !vTodo + V.cardinal !vAvailable = 0) do
		available_proc := first_proc !available_proc ;          (* the first task to be completed is added to vDone *)
		update_available eDag ;                                 (* makes consequences on available tasks, hence an update. *)
		available_proc := actu_proc !vAvailable !available_proc; (* as long as we can, do assign! *)
	done;
	let eFinal = E.union eDag !eNew in
	eFinal


