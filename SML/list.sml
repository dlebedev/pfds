signature STACK =
sig
    type 'a Stack

    exception Subscript

    val empty : 'a Stack
    val isEmpty : 'a Stack -> bool

    val cons : 'a * 'a Stack -> 'a Stack
    val head : 'a Stack -> 'a       (* raises EMPTY if stack is empty *)
    val tail : 'a Stack -> 'a Stack (* raises EMPTY if stack is empty *)

    val ++ : 'a Stack * 'a Stack -> 'a Stack
    val update : 'a Stack * int * 'a -> 'a Stack

    val suffixes : 'a Stack -> 'a Stack Stack
end

structure List : STACK =
struct
type 'a Stack = 'a list

exception Subscript

val empty = []
val isEmpty = null

fun cons (x, xs) = x :: xs
val head = hd
val tail = tl

infix 3 ++ fun [] ++ ys        = ys
             | (x :: xs) ++ ys = x :: xs ++ ys

fun update ([], _, _) = raise Subscript
  | update (_ :: xs, 0, y) = y :: xs
  | update (x :: xs, i, y) = x :: update (xs, i-1, y)

(* exercise 2.1 *)
fun suffixes [] = []
  | suffixes xs = xs :: (suffixes o tl) xs
end
