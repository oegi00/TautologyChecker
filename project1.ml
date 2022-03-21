
    type proposition = 
      False | 
      True | 
      Var of string | 
      And of proposition * proposition | 
      Or of proposition * proposition | 
      Not of proposition | 
      Imply of proposition * proposition | 
      Equiv of proposition * proposition | 
      If of proposition * proposition * proposition ;;




let rec ifify p =

match p
with False -> p |
True ->  p |
Var a -> p |
And(a, b) -> If(ifify a, ifify b, False) |
Or(a, b) -> If(ifify a, True, ifify b) |
Not a -> If(ifify a, False, True) |
Imply(a,b) -> If(ifify a, ifify b, True) |
Equiv(a,b) -> If(ifify a, ifify b, (If(ifify b, False, True))) |
If(a,b,a1) -> p ;;


let rec normalize c =

  let rec normalizing pi a2 b2 =
      match pi with
      If(x, y, z) -> normalizing x (If( y, a2, b2)) (If( z, a2, b2)) |
      _ -> If(pi, normalize a2, normalize b2)	
  	in match c with 
  	If(pi, a, b) -> normalizing pi a b |
  	_ -> c;;
  	
  	
  
let rec substitute c v b =
	match c with 
	Var v -> b |
	_ -> c ;;
	
  
let rec simplify c =
	match c with
	If(True, a, b) -> a |
	If(False, a, b) -> b |
	If(pi, True, False) -> pi |
	If(pi, a, b) -> 
	if a = b
	then a
	else If(pi, substitute a pi True, substitute b pi False) |
	_ -> c;;	
	
  
  
	
	

(*return c {v -> b}*)
(*return a new if expression that is like c,
but everytime you see v, replace with b
so match c with v -> b, match v with b 
if c is equal to v then return b
if not do nothing*)




let tautology p = 
   simplify(normalize(ifify p)) = True;;
  
let test = ifify(Imply(Not(And((Var "p"),(Var "q"))), Or(Not(Var "p"), Not(Var "q")))) ;;
let test1 = normalize(ifify(Imply(Not(And((Var "p"),(Var "q"))), Or(Not(Var "p"), Not(Var "q"))))) ;;
let test2 = simplify(normalize(ifify(Imply(Not(And((Var "p"),(Var "q"))), Or(Not(Var "p"), Not(Var "q")))))) ;;






(*
type proposition =
    False
  | True
  | Var of string
  | And of proposition * proposition
  | Or of proposition * proposition
  | Not of proposition
  | Imply of proposition * proposition
  | Equiv of proposition * proposition
  | If of proposition * proposition * proposition
val ifify : proposition -> proposition = <fun>
val normalize : proposition -> proposition = <fun>
val substitute :
  proposition -> 'a -> proposition -> proposition =
  <fun>
val simplify : proposition -> proposition = <fun>
val tautology : proposition -> bool = <fun>
val test : proposition =
  If (If (If (Var "p", Var "q", False), False, True),
   If (If (Var "p", False, True), True,
    If (Var "q", False, True)),
   True)
val test1 : proposition =
  If (Var "p",
   If (Var "q",
    If (False,
     If (Var "p",
      If (False, True, If (Var "q", False, True)),
      If (True, True, If (Var "q", False, True))),
     True),
    If (True,
     If (Var "p",
      If (False, True, If (Var "q", False, True)),
      If (True, True, If (Var "q", False, True))),
     True)),
   If (False,
    If (False,
     If (Var "p",
      If (False, True, If (Var "q", False, True)),
      If (True, True, If (Var "q", False, True))),
     True),
    If (True,
     If (Var "p",
      If (False, True, If (Var "q", False, True)),
      If (True, True, If (Var "q", False, True))),
     True)))
val test2 : proposition =
  If (Var "p",
   If (Var "q",
    If (False,
     If (Var "p",
      If (False, True, If (Var "q", False, True)),
      If (True, True, If (Var "q", False, True))),
     True),
    If (True,
     If (Var "p",
      If (False, True, If (Var "q", False, True)),
      If (True, True, If (Var "q", False, True))),
     True)),
   If (False,
    If (False,
     If (Var "p",
      If (False, True, If (Var "q", False, True)),
      If (True, True, If (Var "q", False, True))),
     True),
    If (True,
     If (Var "p",
      If (False, True, If (Var "q", False, True)),
      If (True, True, If (Var "q", False, True))),
     True)))
     *)

