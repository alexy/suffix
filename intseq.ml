(* Generalization of the Suffix Tree alphabet and sequence
  
   Copyright (c) 2009, Alexy Khrabrov, Cicero Institute
   Author: Alexy Khrabrov <deliverable@gmail.com>  
   License: LGPL  
 *)


(*  NB for cmo linking, I had to add with type t = int -- why? *)
module Intseq : Suffix.ALPHABET with type t = int = 
struct
  (* this cannot be left out even when the above with type t = int specializer is given: *)
  type t = int
    
  let dummy = -1 (* NB need largest int value *)
  
  let equal a b = a = b   (* NB can omit?  directly equal = ( = ) ? *)
  
  let compare a b = Pervasives.compare a b
  
  (* let print = whuh? *)
  
  let of_int x = x (* pervasive identity function? *)
  
  type s = int array
  
  let empty = [||]
 
  let length = Array.length
  let is_empty a = length a = 0
  
  let get s i = s.(i) (* Array.get s i *) 
  
  let sub = Array.sub
  
  let append a e = Array.append a (Array.make 1 e)
  
  let concat a1 a2 = Array.append a1 a2
  
  let of_array = Array.map of_int
  
  let to_string a = 
    let prefix =
      Array.fold_left (fun acc num -> (if acc <> "" then acc ^ ";" else "[|") ^ (string_of_int num)) 
      "" a in
    if String.length prefix > 0 then
      prefix ^ "|]"
    else
      prefix
end