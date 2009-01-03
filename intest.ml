(* Testing Generalized Suffix Trees
  
   for generalized alphabet sequences 
   -- for integer sequences from toplevel
  
   Copyright (c) 2009, Alexy Khrabrov, Cicero Institute
   Author: Alexy Khrabrov <deliverable@gmail.com>  
   License: LGPL
 *)


#load "cis.cmo"
#load "lSet.cmo"
#load "suffix.cmo"
#use  "intseq.ml"

module Visible = struct 
  module A=Intseq 
  let get_visible _ = (0,0) 
end

module M = Suffix.Tree (Intseq) (Visible)

let st = M.create ()

M.add st (Intseq.of_array [|1;2;3|])

M.tree st

M.add st (Intseq.of_array [|1;2;4|])

M.tree st
