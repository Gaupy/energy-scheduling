open Const
open Usr
open Doc
open Schedule
open Heur
open Format

let () = Random.self_init ()

let parse_graph file =
  let chan = open_in file in
  let ans = Graph_parser.parse Graph_lexer.token (Lexing.from_channel chan) in
  let () = close_in chan in
  ans

let parse_config file =
  let chan = open_in file in
  let lambda0 = float_of_string (input_line chan) in
  let fr = float_of_string (input_line chan) in 
  let n = int_of_string (input_line chan) in
  let ratio = float_of_string (input_line chan) in
  let p = int_of_string (input_line chan) in
  let () = close_in chan in
  {
  lambda0 = lambda0;
  fmax = 1.;
  fr = fr;
  n = n;
  ratio_deadlineMAX = ratio;
  bound = 10.;
  nbrHeur = 5;
  p = p;
  }


let gen_config param =
  let nbrHeur = param.nbrHeur in
  let n = param.n in
  let fmax = param.fmax in
  let fr = param.fr in
  let bound = param.bound in

  let config = {
    tabTask = (* array containing the reference of every tasks sorted by id. *)
      Array.make_matrix nbrHeur n {id = 0; work = 1.; fiab = fr};
    tabStartTime = Array.make_matrix nbrHeur n (0., V.empty);
    tabFinishTime = Array.make_matrix nbrHeur n (0., V.empty);
    tabSlowed = Array.make_matrix nbrHeur n false;
    tabSlowedFreq = Array.make_matrix nbrHeur n (fmax,fmax);
  } in
  for i = 0 to (pred n) do
    config.tabTask.(0).(i) <- { id = i; work = Random.float bound; fiab = fr }
  done;
  (*init tabTasks for every heuristics*)
  for h=1 to nbrHeur-1 do
    for i=0 to n-1 do
      config.tabTask.(h).(i)<- config.tabTask.(0).(i)
    done
  done;
  config

let rec gen_list n accu = if n < 0 then accu else gen_list (pred n) (V.add n accu)

let init_config config param eDag =
  let nbrHeur = param.nbrHeur in
  let n = param.n in
  let fmax = param.fmax in

  config.tabStartTime.(0).(0) <- (0., gen_list (pred n) V.empty);

  for i=0 to (n-1) do
    array_update_add config.tabFinishTime.(0) 0 (config.tabTask.(0).(i).work /. fmax) i
  done;
  initCompTime config param eDag 0;

  (* init time arrays for other heuristics *)
  for h=1 to nbrHeur-1 do
    for i=0 to n-1 do
      config.tabFinishTime.(h).(i) <- config.tabFinishTime.(0).(i);
      config.tabStartTime.(h).(i) <- config.tabStartTime.(0).(i);
    done
  done;
config

let script dagfile paramfile = 

  let param = parse_config paramfile in
  let n = param.n in

  let rec gen n accu =
    if n < 0 then accu
    else gen (pred n) (V.add n accu)
  in

  let vDag = gen (pred n) V.empty in
  let eDag_sans_proc = parse_graph dagfile in
(* scheduling without speed scaling *)
  let config_work = gen_config param in
  let eDag = list_sched_crit_path config_work param vDag eDag_sans_proc in
  let config = init_config config_work param eDag in

  (* init vCritH *)
  for h=0 to param.nbrHeur-1 do
  let vCritRef = vCritH.(h) in
    vCritH.(h) <- (critTasks config param eDag h vCritRef)
  done;

  (* define deadline with ratio *)
  let deadlineMAX = param.ratio_deadlineMAX *. (maxTime config param 0) in

  let rec eg_pire eg_cour i=
	if i=n then eg_cour
	else eg_pire (eg_cour +. (config.tabTask.(0).(i).work)*.param.fmax ** 2.) (i+1)
      in
	let eg_norm = (eg_pire 0. 0) in
(*      printf "energie normale fmax : %f \n%!" eg_norm ; *) printf "%f \t%!" eg_norm ;
        let f = max param.fr (1./.param.ratio_deadlineMAX) in
        let eg_ral = eg_norm *. f**2. /. (param.fmax**2.) in
(*      printf "energie ralentie (max(fr,WC/D)) : %f \n%!" eg_ral;*) printf "%f \t%!" eg_ral ;

  for h=0 to param.nbrHeur-1 do
   try
    (match h with
      | 0 -> sol_heur0 config param eDag vDag 0 deadlineMAX 
      | 1 -> sol_heur1 config param eDag vDag 1 deadlineMAX
      | 2 -> sol_heur2 config param eDag vDag 2 deadlineMAX
      | 3 -> sol_heur3 config param eDag vDag 3 deadlineMAX
      | 4 -> sol_heur4 config param eDag vDag 4 deadlineMAX
      | _ -> failwith "not a heuristic" );
    let eg = ref 0. in
      for i=0 to (n-1) do	
        eg := !eg +. (energy i config h);
      done;
(*      printf "energie totale de l'heuristique %i : %f \n%!" (h+1) !eg; flush stdout ; *) printf "%f \t%!" !eg ;
    with
      | Over_Deadline _ -> printf "Over_Deadline \t%!"
  done;
  printf "\n"

let () = script Sys.argv.(1) Sys.argv.(2)
