
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

implicit module Add_int : (ADD with type t = int) = struct
  type t = int
  let (+) x y = Pervasives.(+) x y
end

implicit module Sub_int : (SUB with type t = int) = struct
  type t = int
  let (-) x y = Pervasives.(-) x y
end

implicit module Mul_int : (MUL with type t = int) = struct
  type t = int
  let ( * ) x y = Pervasives.( * ) x y
end

implicit module Div_int : (DIV with type t = int) = struct
  type t = int
  let ( / ) x y = Pervasives.( / ) x y
end

implicit module Zero_int : (ZERO with type t = int) = struct
  include Add_int
  let zero () = 0
end

implicit module Neg_int : (NEG with type t = int) = struct
  include Add_int
  let neg n = -n
end

implicit module Add_float : (ADD with type t = float) = struct
  type t = float
  let (+) x y = x +. y
end

implicit module Sub_float : (SUB with type t = float) = struct
  type t = float
  let (-) x y = x -. y
end

implicit module Mul_float : (MUL with type t = float) = struct
  type t = float
  let ( * ) x y = x *. y
end

implicit module Div_float : (DIV with type t = float) = struct
  type t = float
  let ( / ) x y = x /. y
end

implicit module Zero_float : (ZERO with type t = float) = struct
  include Add_float
  let zero () = 0.
end

implicit module Neg_float : (NEG with type t = float) = struct
  include Add_float
  let neg n = -. n
end

implicit module Add_complex : (ADD with type t = Complex.t) = struct
  type t = Complex.t
  let (+) x y = Complex.add x y
end

implicit module Sub_complex : (SUB with type t = Complex.t) = struct
  type t = Complex.t
  let (-) x y = Complex.sub x y
end

implicit module Mul_complex : (MUL with type t = Complex.t) = struct
  type t = Complex.t
  let ( * ) x y = Complex.mul x y
end

implicit module Div_complex : (DIV with type t = Complex.t) = struct
  type t = Complex.t
  let ( / ) x y = Complex.div x y
end

 implicit module Zero_complex : (ZERO with type t = Complex.t) = struct
  include Add_complex
  let zero () = Complex.zero
end

implicit module Neg_complex : (NEG with type t = Complex.t) = struct
  include Add_complex
  let neg n = Complex.neg n
end


implicit module Add_string : (ADD with type t = String.t) = struct
  type t = String.t
  let (+) x y =  x^y
end

 implicit module Zero_string : (ZERO with type t = String.t) = struct
  include Add_string
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

implicit module IntVector = Vec(Zero_int)
implicit module FloatVector = Vec(Zero_float)
implicit module ComplexVector = Vec(Zero_complex)

implicit module IntVectorIterator = IntVector.Iter
implicit module FlaotVectorIterator = FloatVector.Iter

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

module type FUNCTION = sig
  type dom_t
  type rng_t
end

module type MAP_ITERATOR = sig
  type dom_t
  type rng_t
  module I : (BASIC_ITERATOR with type item_t = dom_t)
  include (BASIC_ITERATOR with type item_t = rng_t)
  val make : I.t -> (dom_t -> rng_t) -> t
end

module type TYPE = sig
  type t
end

implicit module TYPEint = struct type t = int end ;;
(*
implicit module TYPEfloat = struct type t = float end ;;
implicit module TYPEcomplex = struct type t = Complex.t end ;;
 *)
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
    let make (ii : I.t) f = Basic.{ it = ii ; f = f }
  end

let map {M : MAP_ITERATOR} (ii : M.I.t) (f : M.dom_t -> M.rng_t) =
  M.make ii f
