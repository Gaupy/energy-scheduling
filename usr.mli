type task = { id : int; work : float; fiab : float; }

type edge = { id1 : int; id2 : int; }

module V : Set.S with type elt = int
module VH1 : Set.S with type elt = int
module IntMap : Map.S with type key = int

type configuration = {
  tabTask : task array array;
  tabStartTime : (float * V.t) array array;
  tabFinishTime : (float * V.t) array array;
  tabSlowed : bool array array;
  tabSlowedFreq : (float * float) array array;
}

module E : Set.S with type elt = edge

val matchRel : configuration -> Const.parameter -> float -> float -> int -> bool
val energy : int -> configuration -> int -> float
val energyTest : configuration -> float -> float -> int -> float
val computef1 : Const.parameter -> 'a -> float
val computef2 : Const.parameter -> 'a -> float
val heurFun1 : configuration -> int -> float -> float
