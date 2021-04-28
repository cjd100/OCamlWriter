(* this compilation unit handles state and undo functionality. The save
   state of a file will be represented by a stack, with the most recent
   save on top. This will be returned to by an undo. *)

module type Stack = sig
  (* The type of a stack whose elements are type 'a *)
  type 'a stack

  (* The empty stack *)
  val empty : 'a stack

  (* Whether the stack is empty*)
  val is_empty : 'a stack -> bool

  (* [push x s] is the stack [s] with [x] pushed on the top *)
  val push : 'a -> 'a stack -> 'a stack

  (* [peek s] is the top element of [s]. Raises Failure if [s] is empty. *)
  val peek : 'a stack -> 'a

  (* [pop s] pops and discards the top element of [s]. Raises Failure if
     [s] is empty. *)
  val pop : 'a stack -> 'a stack
end

(* implement stack as variant *)
module VarStack : Stack = struct
  type 'a stack =
    | Empty
    | Entry of 'a * 'a stack

  let empty = Empty

  let is_empty s = s = Empty

  let push x s = Entry (x, s)

  let peek = function Empty -> failwith "Empty" | Entry (x, _) -> x

  let pop = function Empty -> failwith "Empty" | Entry (_, s) -> s
end

open VarStack
