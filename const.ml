open Arg

type parameter = {
  lambda0 : float;
  fmax : float;
  fr : float;
  n : int;
  ratio_deadlineMAX : float;
  bound : float;
  nbrHeur : int;
  p : int
 }

let default = {
  lambda0 = 0.0001;
  fmax = 1.;
  fr = (2. /. 3.) *. 1.;
  n = 100;
  ratio_deadlineMAX = 1.9;
  bound = 10.;
  nbrHeur = 5;
  p = 2
}
