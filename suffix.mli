(* From JC Filliatre, +empty, append, concat, sub *)

module type ALPHABET = sig

  (* characters *)

  type t 

  (* [dummy] is a character that is assumed not to appear in any string *)
  val dummy : t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  (* val print :  Format.formatter -> t -> unit *)

  (* strings over this alphabet *)

  type s
  
  val empty : s
  val is_empty : s -> bool

  val length : s -> int

  val get : s -> int -> t  
  val sub: s -> int -> int -> s
  
  val append: s -> t -> s
  val concat: s -> s -> s
  
  val of_array : t array -> s
  val to_string: s -> string
end


(* --------------------------------------------------------------------------------
   Operations on substrings of sequences
   -------------------------------------------------------------------------------- *)

module type SUBSEQUENCE = 
  sig
    module A: ALPHABET
    
  	type t = A.s * int * int (* if not given, will be hidden! *)
	  
  	val empty : t
    
  	val is_empty : t -> bool

  	val get : t -> int -> A.t

  	val length : t -> int

  	val sub : t -> int -> int -> t

  	val extend : t -> t
  end
  
 
(** [get_visible s] returns the sizes of the prefix and suffix of [s]
   that can be removed from [s] without damage to its meaning. *)
 
module type VISIBLE = sig
  module A: ALPHABET
  val get_visible : A.s -> int * int
  end


module type TREE = sig
  
  module A : ALPHABET

    type strid = int
	  (** Type of string ids. Functions using such ids are unspecified if the id is not valid. *)
    type t
	  (** Type of suffix trees. This is not a pure functional data-structure. *)

(** {1 Suffix trees as string sets. } *)

    val create : unit -> t
	(** [create ()] returns a fresh and empty suffix tree. *)
    val size : t -> int
	(** [size st] returns the number of strings registered in the suffix tree [st]. *)
    val add : t -> A.s -> strid
	(** [add st s] adds the string [s], and all its suffixes in the suffix tree [st], unless [s] has already been added.
	   It also returns the string id as an handle on this string. *)
    val remove : t -> strid -> unit
	(** [remove st id] removes the string identified by [id], and all its suffixes, from the suffix tree [st]. *)
    val get : t -> strid -> A.s
	(** [get st id] returns the string associated to [id]. *)
    val find : t -> A.s -> strid
	(** [find st s] returns the id associated to the string [s], if the strings exists in the suffix tree [st].
	   Otherwise raise Not_found. *)
    val fold : (strid -> A.s -> 'a -> 'a) -> t -> 'a -> 'a
	(** [fold f st e] is a classic folding on all strings in the suffix tree [st]. *)

(** {1 Low-level interface on suffix trees. } *)

    type node
	  (** Type of the nodes of suffix trees.
	     Nodes are either leaves or internal nodes. *)

    val root : t -> node
	(** [root st] returns the root node of the suffix tree [st]. *)
    val is_leaf : t -> node -> bool
	(** [is_leaf st n] returns whether the node [n] is a leaf. *)
    val label : t -> node -> A.s
	(** [label st n] returns the string labelling the node [n]. *)
    val length : t -> node -> int
	(** [length st n] returns the length of the string labelling the node [n]. *)
    val path : t -> node -> A.s
	(** [path st n] returns the full path from the root to the node [n]. *)
    val height : t -> node -> int
	(** [height st n] returns the height of node [n], i.e. the length of the path from root to [n]. *)
    val ext : t -> node -> strid LSet.t
	(** [ext st n] returns an ordered list of string ids that match the path of the node [n]. *)
    val children : t -> node -> node LSet.t
	(** [children st n] returns the list of children nodes of [n]. *)
    val parent : t -> node -> node option
	(** [parent st n] returns the parent node of [n], unless [n] is the root node. *)
    val succ : t -> node -> node option
	(** [succ st n] returns the successor node through the suffix link of [n], unless there is no suffix link. *)
    val preds : t -> node -> node LSet.t
	(** [preds st n] returns the list of all nodes having [n] as successor node. *)
    val suffix : t -> node -> strid * int
	(** [suffix st n] returns the suffix represented by the leaf node [n] as a couple [(string id, position in the string)].
	   Raise Not_found if [n] is not a leaf. *)
    val find_node : t -> A.s -> node
	(** [find_node st s] returns the node whose path is equal to the string [s], if it exists.
	   Raise Not_found otherwise. *)
    val fold_tree : t -> ('h -> node -> bool) -> ('h -> node -> 'h) -> ('s list -> 'h -> node -> 's) -> 'h -> 's
	(** [fold_tree st filter herit synth h0] returns the result of an attribute evaluation on the suffix tree [st].
	   - [filter] is used to filter which children of a node should be explored given the heritance value of the parent node,
	   - [herit] defines the heritance value of a node, given the heritance value of its parent,
	   - [synth] defines the synthesized value of a node given its heritance value, and the list of synthesized values of its filtered children,
	   - [h0] is the heritance value given to the root.
	 *)

(** {1 Exploring the suffix tree through the substring relation. } *)
		
    val path_restrictions : t -> node -> node list
	(** [path_restrictions st n] returns the list of nodes whose path is a direct restriction of the path of [n]. *)
    val path_extensions : t -> node -> node list
	(** [path_extensions st n] returns the list of nodes whose path is a direct extension of the path of [n]. *)
    val is_maximal : t -> node -> bool
	(** [is_maximal st n] returns whether a node is maximal.
	   A node is maximal is each of its extensions has a strictly smaller extent, or the node represents a full string. *)
    val set_visible : t -> node -> int * int -> unit
	(** [set_visible st node (left_pos, right_pos)] sets which part of a node path should be visible when maximal. *)
    val max_restrictions : t -> node -> node list
	(** [max_restrictions st n] returns the list of maximal nodes whose path is a restriction of the path of [n]. *)
    val max_extensions : t -> node option -> node list * strid list
	(** [max_extensions st n_opt] returns the list of maximal nodes and leaves whose path is an extension of the path of [n], when given.
	   If a start node is not given, then the maximal nodes with shortest path are returned. *)
(*
    val string_extensions : t -> node option -> strid list
	(** [string_extensions st n_opt] completes the result of [max_extensions st n_opt] with full strings through their ids. *)
*)
    val string_restrictions : t -> strid -> node list
	(** [string_restrictions st strid] returns the list of maximal nodes having [strid] as a string extension. *)

(** {1 Searching in a suffix tree} *)

    type factor = node * A.s * node
	  (** [(parent,s,child)] locates a factor string on the edge from node [parent] to node [child], where [s] is a prefix of the label of [child].
	     If [s] is the empty string, then [parent] and [child] are a same node.
	     The path of a factor is the concatenation of [path st parent] and [s]. *)

    val find_factor : t -> A.s -> factor
	(** [find_factor st s] returns the factor locating [s] in the suffix tree [st].
	   This means the path of the result factor is equal to [s].
	   Raise [Not_found] if the string [s] does not appear in any string of [st]. *)
    val suffixes : t -> factor -> (strid * int) list
	(** [suffixes st f] returns the list of all suffixes [(strid,pos)] that have the path of [f] as a prefix: this path occurs in string [strid] at position [pos]. *)
    val strings : t -> factor -> strid LSet.t
	(** [strings st f] returns the ids of all string containing the path of [f]. *)

(** {1 Simpler representation of a suffix tree (for debugging purpose at top-level)} *)

    type tree = Node of string * int * int list * tree list | Leaf of string * (strid * int)

    val tree : t -> tree
  end


module Tree (A: ALPHABET) (Visible: VISIBLE with module A=A) 
 : TREE with module A=A
