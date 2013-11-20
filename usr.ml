open Const
open Format

(* DEFINITION TYPE TASKS *)

type task ={id:int;
	    work:float;
	    fiab:float;
	   }
    
    
(* DEFINITION TYPE EDGES *)
type edge ={id1:int;
	    id2:int;
	   }
    
module IntOrd =
struct
  type t = int
  let compare = Pervasives.compare
end

module V = Set.Make(IntOrd)
module VH1 = Set.Make(IntOrd)
module IntMap = Map.Make(IntOrd)	


type configuration = {
  tabTask : task array array;
  tabStartTime : (float * V.t) array array;
  tabFinishTime : (float * V.t) array array;
  tabSlowed : bool array array;
  tabSlowedFreq : (float * float) array array;
}    
    
(*GLOBAL ARRAYS, DEFINITIONS*)
    
    
(*WE DEFINE THE SET OF EDGES, SORTED WITH LEX ORDER. *)
    
module EdgeIdOrd =
struct
  type t= edge
  let compare = fun e1 e2 ->
    if ((e1.id1) < (e2.id1)) then -1
    else if ((e1.id1) > (e2.id1)) then 1
    else if ((e1.id2) < (e2.id2)) then -1
    else if ((e1.id2) > (e2.id2)) then 1
    else 0
end
  
module E = Set.Make(EdgeIdOrd)

let eDag = ref E.empty (*eDAG is the set of edges *)
  
(* OBJECTIVE FUNCTIONS *)
  
let matchRel config param f1 f2 id =
  (f1*.f2 >= param.lambda0 *. ((config.tabTask.(0).(id)).work) *. param.fr)
    
    
let energy id config h =
  let f1,f2 = config.tabSlowedFreq.(h).(id) in
  if config.tabSlowed.(h).(id) then ((config.tabTask.(0).(id)).work) *. (f1**2. +. f2**2.)
  else ((config.tabTask.(0).(id)).work) *. (f1**2.)
    (* energy functions depending on the model *)

let energyTest config f1 f2 id =
  ((config.tabTask.(0).(id)).work) *. (f1**2. +. f2**2.)
    (* energy functions depending on the model  *)    

let computef1 param t = (*execution speed when two executions *)
(*  0.2838 *. param.fr*) 0.7 *. param.fr
    
let computef2 param t =(*re-execution speed when two executions *)
(*  0.2838 *. param.fr*) 0.7 *. param.fr
(* FONCTION DE COMPARAISON D'HEURISTIQUES*)

let heurFun1 config t w = w +. (config.tabTask.(0).(t).work)
