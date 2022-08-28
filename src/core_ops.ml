
module type ADD = sig
  type t
  val ( + ) : t -> t -> t
end

let ( + ) { M : ADD} x y = M.( + ) x y

module type SUB = sig
  type t
  val ( - ) : t -> t -> t
end

let ( - ) { M : SUB} x y = M.( - ) x y

module type MUL = sig
  type t
  val ( * ) : t -> t -> t
end

let ( * ) { M : MUL} x y = M.( * ) x y

module type DIV = sig
  type t
  val ( / ) : t -> t -> t
end

let ( / ) { M : DIV} x y = M.( / ) x y

module type ZERO = sig
  type t
  val zero : unit -> t
end

let zero { M : ZERO} () = M.zero ()

module type NEG = sig
  type t
  val neg : t -> t
end

let neg { M : NEG} = M.neg

implicit module Ops_int = struct
  type t = int
  let (+) x y = Pervasives.(+) x y
  let (-) x y = Pervasives.(-) x y
  let ( * ) x y = Pervasives.( * ) x y
  let ( / ) x y = Pervasives.( / ) x y
  let zero () = 0
  let neg n = -n
end

implicit module Ops_float = struct
  type t = float
  let (+) x y = x +. y
  let (-) x y = x -. y
  let ( * ) x y = x *. y
  let ( / ) x y = x /. y
  let zero () = 0.
  let neg n = -. n
end

implicit module Ops_complex = struct
  type t = Complex.t
  let (+) x y = Complex.add x y
  let (-) x y = Complex.sub x y
  let ( * ) x y = Complex.mul x y
  let ( / ) x y = Complex.div x y
  let zero () = Complex.zero
  let neg n = Complex.neg n
end

implicit module Ops_string = struct
  type t = String.t
  let (+) x y =  x^y
  let zero () = ""
end

module type BASIC_GEN_ITERATOR = sig
  type 'a item_t
  type 'a t
  val next : 'a t -> 'a item_t option
end

module type GEN_ITERATOR = sig
  type 'a item_t
  type 'a t
  include BASIC_GEN_ITERATOR with type 'a t := 'a t and type 'a item_t := 'a item_t
  val size_hint : 'a t -> (int * int option)
  val count : 'a t -> int
  val last : 'a t -> 'a item_t option
  val advance_by : 'a t -> int -> (unit, int) Result.result
end

module type BASIC_ITERATOR = sig
  type item_t
  type t
  val next : t -> item_t option
end

module type ITERATOR = sig
  type item_t
  type t
  include BASIC_ITERATOR with type t := t and type item_t := item_t
  val size_hint : t -> (int * int option)
  val count : t -> int
  val last : t -> item_t option
  val advance_by : t -> int -> (unit, int) Result.result
end

module FullGenIterator(I : BASIC_GEN_ITERATOR) : (GEN_ITERATOR with type 'a t = 'a I.t and type 'a item_t = 'a I.item_t) =
  struct
    open Result
    include I
    let size_hint _ = (0, None)
    let count ii =
      let rec crec n =
        match I.next ii with
          None -> n
        | Some _ -> crec (n+1)
      in crec 0
    let last ii =
      let rec lrec cur =
        match I.next ii with
          None -> cur
        | Some v -> lrec (Some v)
      in lrec None
    let advance_by ii n  =
      let rec arec cnt =
        if cnt = n then Ok ()
        else match I.next ii with
               None -> Error cnt
             | Some _ -> arec (cnt+1)
      in arec 0
  end

module FullIterator(I : BASIC_ITERATOR) : (ITERATOR with type t = I.t and type item_t = I.item_t) =
  struct
    open Result
    include I
    let size_hint _ = (0, None)
    let count ii =
      let rec crec n =
        match I.next ii with
          None -> n
        | Some _ -> crec (n+1)
      in crec 0
    let last ii =
      let rec lrec cur =
        match I.next ii with
          None -> cur
        | Some v -> lrec (Some v)
      in lrec None
    let advance_by ii n  =
      let rec arec cnt =
        if cnt = n then Ok ()
        else match I.next ii with
               None -> Error cnt
             | Some _ -> arec (cnt+1)
      in arec 0
  end

type 'a vector_iterator_t = { it : 'a Vector.t ; mutable next : int }

module Basic_gen_iterator_vector : (BASIC_GEN_ITERATOR with type 'a item_t = 'a and
                                                            type 'a t = 'a vector_iterator_t) = struct
  type 'a item_t = 'a
  type 'a t = 'a vector_iterator_t
  let next ii =
    if ii.next == Vector.length ii.it then
      None
    else
        let v = Vector.get ii.it ii.next in
        ii.next <- 1 + ii.next ;
        Some v
end

module Gen_iterator_vector : (GEN_ITERATOR with type 'a item_t = 'a and
                                                         type 'a t = 'a vector_iterator_t) = struct
  include Basic_gen_iterator_vector

  let size_hint ii =
    let siz = (Vector.length ii.it) - ii.next in
    (siz, Some siz)
  let count ii =
    let siz = (Vector.length ii.it) - ii.next in
    ii.next <- Vector.length ii.it ;
    siz
  let last ii =
    let siz = Vector.length ii.it in
    if ii.next == siz then None
    else let v = Vector.get ii.it (siz-1) in
 ii.next <- siz ;
      Some v
  open Result
  let advance_by ii n =
    let siz = Vector.length ii.it in
    let remaining = ii.next + n in
    if remaining > siz then begin
        ii.next <- siz ;
        Error remaining
      end
    else begin
        ii.next <- n + ii.next ;
        Ok ()
      end

end

module type SUBSCRIPTABLE = sig
  type item_t
  type t
  val sub : t -> int -> item_t
  val make : item_t -> item_t list -> t
end

module type ITERABLE = sig
  type item_t
  type t
  module Iter : (ITERATOR with type item_t = item_t)
  val iter : t -> Iter.t
end

let iter { I : ITERABLE } it = I.iter it

module type ZERO_SUBSCRIPTABLE = sig
  type t
  type item_t
  type iterator_t
  include SUBSCRIPTABLE with type item_t := item_t and type t := t
  include ZERO with type t := item_t
  include ITERABLE with type item_t := item_t and type t := t
end

let sub {M : SUBSCRIPTABLE} = M.sub
let make_default {M : SUBSCRIPTABLE} def l = M.make def l
let make {M : ZERO_SUBSCRIPTABLE} l = M.make (M.zero()) l

module GenVec = struct
  type 'a t = 'a Vector.t
  let sub v n =
    Vector.get v n
  let make dummy l = Vector.of_list dummy l
end

module Vec (C : ZERO) : (ZERO_SUBSCRIPTABLE with type item_t = C.t and type t = C.t GenVec.t) = struct
  type item_t = C.t
  type t = item_t GenVec.t
  let zero () = C.zero ()
  let sub = GenVec.sub
  let make = GenVec.make
  module Iter = struct
    type item_t = C.t
    type t = C.t Gen_iterator_vector.t
    let next (ii : t) : C.t option = Gen_iterator_vector.next ii
    let advance_by = Gen_iterator_vector.advance_by
    let last = Gen_iterator_vector.last
    let count = Gen_iterator_vector.count
    let size_hint = Gen_iterator_vector.size_hint
  end
  type iterator_t = Iter.t
  let iter v =
    { it = v ; next = 0 }
end

implicit module IntVector = Vec(Ops_int)
implicit module FloatVector = Vec(Ops_float)
implicit module ComplexVector = Vec(Ops_complex)

implicit module IntVectorIterator = IntVector.Iter
implicit module FloatVectorIterator = FloatVector.Iter
implicit module ComplexVectorIterator = ComplexVector.Iter

type string_iterator_t = { it : string ; mutable cur : int }

module Basic_string_iterator : (BASIC_ITERATOR with type item_t = Char.t and type t = string_iterator_t) = struct
  type item_t = Char.t
  type t = string_iterator_t
  let next ii =
    if ii.cur = String.length ii.it then None
    else let c = String.get ii.it ii.cur in
         ii.cur <- 1 + ii.cur ;
         Some c
end

implicit module Str : (ZERO_SUBSCRIPTABLE with type item_t = Char.t and type t = String.t) = struct
  type item_t = Char.t
  type t = String.t
  let zero () = '\000'
  let sub s n = String.get s n
  let make dummy l =
    let slen = List.length l in
    let s = String.make slen dummy in
    List.iteri (fun i c -> String.set s i c)  l ;
    s
  module Iter = FullIterator(Basic_string_iterator)
  type iterator_t = Iter.t
  let iter s =
    { it = s ; cur = 0 }
end

module type TYPE = sig
  type t
end

implicit module TYPEint = struct type t = int end ;;
implicit module TYPEfloat = struct type t = float end ;;
implicit module TYPEcomplex = struct type t = Complex.t end ;;

module type MAP_ITERATOR = sig
  type dom_t
  type rng_t
  module I : (BASIC_ITERATOR with type item_t = dom_t)
  include (BASIC_ITERATOR with type item_t = rng_t)
  val map : I.t -> (dom_t -> rng_t) -> t
end

module MapIterator (DOM : TYPE)(RNG : TYPE)(I : (BASIC_ITERATOR with type item_t = DOM.t))
       : (MAP_ITERATOR with type dom_t = DOM.t and
                            type rng_t = RNG.t and
                            module I = I) =
  struct
    type dom_t = DOM.t
    type rng_t = RNG.t
    implicit module I = I
    module Basic = struct
    type t = {
        it : I.t ;
        f : dom_t -> rng_t
      }
    type item_t = rng_t
    let next it =
      match I.next it.it with
        None -> None
      | Some arg -> Some(it.f arg)
    end
(*
    module Full = FullIterator(Basic)
    include Full
 *)
    include Basic
    let map (ii : I.t) f = Basic.{ it = ii ; f = f }
  end

let map {M : MAP_ITERATOR} (ii : M.I.t) (f : M.dom_t -> M.rng_t) =
  M.map ii f

module type ADD_ZERO = sig
  include ADD
  include (ZERO with type t := t)
end

module type ITERATOR_SUM = sig
  module I : BASIC_ITERATOR
  module A : (ADD_ZERO with type t = I.item_t)
  val sum : I.t -> A.t
end

module IteratorSum (I : BASIC_ITERATOR) (A : (ADD_ZERO with type t = I.item_t))
     : (ITERATOR_SUM with module I = I and module A = A)
  = struct
  module I = I
  module A = A
  let sum (ii : I.t) =
    let rec srec acc = match I.next ii with
        None -> acc
      | Some v -> srec (A.( + ) acc v)
    in srec (A.zero())
end

let sum {S : ITERATOR_SUM} ii =
  S.sum ii

module type FILTER_ITERATOR = sig
  module I : BASIC_ITERATOR
  include (BASIC_ITERATOR with type item_t = I.item_t)
  val filter : I.t -> (I.item_t -> bool) -> t
end

module FilterIterator(I : BASIC_ITERATOR)
       : (FILTER_ITERATOR with module I = I) =
  struct
    implicit module I = I
    module Basic = struct
    type t = {
        it : I.t ;
        pred : I.item_t -> bool
      }
    type item_t = I.item_t
    let rec next it =
      match I.next it.it with
        None -> None
      | Some arg ->
         if it.pred arg then
           Some arg
         else next it
    end
(*
    module Full = FullIterator(Basic)
    include Full
 *)
    include Basic
    let filter (ii : I.t) pred = Basic.{ it = ii ; pred = pred }
  end

let filter {F : FILTER_ITERATOR} (ii : F.I.t) (pred : F.item_t -> bool) =
  F.filter ii pred

implicit module Range = struct
  type t = { _start : int ;
             _end : int ;
             _end_inclusive : bool }
  type item_t = int

  type range_t = t
  let make ?(end_inclusive=false) st en =
    { _start = st ; _end = en ; _end_inclusive = end_inclusive }

  module BasicIter = struct
    type item_t = int
    type t = { it : range_t ; mutable next : int }
    let next (ii : t) : int option =
      let rng = ii.it in
      if (if rng._end_inclusive then ii.next > rng._end
          else  ii.next >= rng._end) then
        None
      else let rv = ii.next in
           ii.next <- 1 + ii.next ;
           Some rv
  end
  module Iter = FullIterator(BasicIter)
  type iterator_t = Iter.t
  let iter (v : range_t) =
    BasicIter.{ it = v ; next = v._start }
end

implicit module RangeIterator = Range.Iter

module type ITERATOR_COLLECT = sig
  module I : BASIC_ITERATOR
  module Z : (ZERO with type t = I.item_t)
  type 'a coll_t
  val collect : I.t -> Z.t coll_t
end

let collect {S : ITERATOR_COLLECT} ii =
  S.collect ii

module IteratorCollectVector (I : BASIC_ITERATOR) (Z : (ZERO with type t = I.item_t))
     : (ITERATOR_COLLECT with module I = I and module Z = Z and type 'a coll_t = 'a Vector.t)
  = struct
  module I = I
  module Z = Z
  type 'a coll_t = 'a Vector.t
  let collect (ii : I.t) =
    let acc = Vector.create (Z.zero()) in
    let rec srec () = match I.next ii with
        None -> acc
      | Some v ->
         Vector.push acc v ;
         srec ()
    in srec ()
end


module IteratorCollectList (I : BASIC_ITERATOR) (Z : (ZERO with type t = I.item_t))
     : (ITERATOR_COLLECT with module I = I and module Z = Z and type 'a coll_t = 'a list)
  = struct
  module I = I
  module Z = Z
  type 'a coll_t = 'a list
  let collect (ii : I.t) =
    let rec srec revacc = match I.next ii with
        None -> List.rev revacc
      | Some v ->
         srec (v::revacc)
    in srec []
end

module type ITERATOR_FOLD = sig
  type dom_t
  type rng_t
  module I : (BASIC_ITERATOR with type item_t = dom_t)
  type t = rng_t
  val fold : t -> (t -> I.item_t -> t) -> I.t -> t
end

let fold {F : ITERATOR_FOLD} acc f ii =
  F.fold acc f ii

module IteratorFold (DOM : TYPE)(RNG : TYPE) (I : (BASIC_ITERATOR with type item_t = DOM.t))
       : (ITERATOR_FOLD with type dom_t = DOM.t and
                             type rng_t = RNG.t and
                             module I = I)
  = struct
  type dom_t = DOM.t
  type rng_t = RNG.t
  type t = rng_t
  module I = I
  let fold acc f (ii : I.t) =
    let rec frec acc = match I.next ii with
        None -> acc
      | Some v -> frec (f acc v)
    in frec acc
end
