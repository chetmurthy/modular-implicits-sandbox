
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

module type GEN_ITERATOR = sig
  type 'a item_t
  type 'a t
  val next : 'a t -> 'a item_t option
  val size_hint : 'a t -> (int * int option)
  val count : 'a t -> int
  val last : 'a t -> 'a item_t option
  val advance_by : 'a t -> int -> (unit, int) Result.result
end

type 'a vector_iterator_t = { it : 'a Vector.t ; mutable next : int }

implicit module Gen_iterator_vector : (GEN_ITERATOR with type 'a item_t = 'a and
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

module type ZERO_SUBSCRIPTABLE = sig
    include SUBSCRIPTABLE
    include ZERO with type t := item_t
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

implicit module Vec { C : ZERO } : (ZERO_SUBSCRIPTABLE with type item_t = C.t and type t = C.t GenVec.t) = struct
  type item_t = C.t
  type t = item_t GenVec.t
  let zero () = C.zero ()
  let sub = GenVec.sub
  let make = GenVec.make
end

implicit module IntItem = struct
  type item_t = int
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
end
