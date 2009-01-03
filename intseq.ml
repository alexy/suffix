(* Generalization of the Suffix Tree alphabet and sequence
  
   Copyright (c) 2009, Alexy Khrabrov, Cicero Institute
   Author: Alexy Khrabrov <deliverable@gmail.com>  
   License: LGPL  
 *)


module type ALPHABET = sig

  (* characters *)

  type t 

  (* [dummy] is a character that is assumed not to appear in any string *)
  val dummy : t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  (* val print :  Format.formatter -> t -> unit *)

  val of_int : int -> t
  
  
  (* strings over this alphabet *)

  type s
  
  val empty : s

  val length : s -> int

  val get : s -> int -> t
  
  val sub: s -> int -> int -> s
  
  val append: s -> t -> s

  val concat: s -> s -> s
  
  val of_array : int array -> s
  val to_string: s -> string
end


module Intseq : ALPHABET = 
struct
  type t = int
    
  let dummy = -1 (* NB need largest int value *)
  
  let equal a b = a = b   (* NB can omit?  directly equal = ( = ) ? *)
  
  let compare a b = Pervasives.compare a b
  
  (* let print = whuh? *)
  
  let of_int x = x (* pervasive identity function? *)
  
  type s = int array
  
  let empty = [||]
  
  let length = Array.length
  
  let get s i = s.(i) (* Array.get s i *) 
  
  let sub = Array.sub
  
  let append a e = Array.append a (Array.make 1 e)
  
  let concat a1 a2 = Array.append a1 a2
  
  let of_array = Array.map of_int
  
  let to_string a = 
    (Array.fold_left (fun acc num -> (if acc <> "" then acc ^ ";" else "[|")
      ^ (string_of_int num)) "" a)^"|]";;
end