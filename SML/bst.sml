signature SET =
sig
    type Elem
    datatype Tree = E | T of Tree * Elem * Tree
    type Set

    exception ALREADY_IN_SET

    val empty    : Set
    val insert   : Elem * Set -> Set
    val insert2  : Elem * Set -> Set
    val insert3  : Elem * Set -> Set
    val member   : Elem * Set -> bool
    val member2  : Elem * Set -> bool
    val complete : Elem * int -> Tree
end

signature ORDERED =
sig
    type T

    val eq  : T * T -> bool
    val lt  : T * T -> bool
    val leq : T * T -> bool
end

functor UnbalancedSet (Element : ORDERED) : SET =
struct
type Elem = Element.T
datatype Tree = E | T of Tree * Elem * Tree
type Set = Tree

val empty = E

fun member (x, E) = false
  | member (x, T (a, y, b)) =
    if Element.lt(x, y) then member(x, a)
    else if Element.lt(y, x) then member(x, b)
    else true

(* exercise 2.2 *)
fun member2 (x, E) = false
  | member2 (x, T (a, y, b)) =
    let fun aux (E, track) = Element.eq(x, track)
          | aux (T (a, y, b), track) =
            if Element.lt(x, y) then aux(a, track)
            else aux(b, y)
    in
        aux(T (a, y, b), y)
    end

fun insert (x, E) = T (E, x, E)
  | insert (x , s as T (a, y, b)) =
    if Element.lt(x, y) then T (insert (x, a), y, b)
    else if Element.lt(y, x) then T (a, y, insert (x, b))
    else s

(* exercise 2.3 *)
exception ALREADY_IN_SET

fun insert2 (x, E) = T (E, x, E)
  | insert2 (x , s as T (a, y, b)) =
    let fun aux E = T (E, x, E)
          | aux (T (a, y, b)) =
            if Element.lt(x, y) then T (aux a, y, b)
            else if Element.lt(y, x) then T (a, y, aux b)
            else raise ALREADY_IN_SET
    in
        aux s handle ALREADY_IN_SET => s
    end

(* exercise 2.4 *)
fun insert3 (x, E) = T (E, x, E)
  | insert3 (x, s as T (a, y, b)) =
    let fun aux (E, track) =
            if Element.eq(x, track) then raise ALREADY_IN_SET
            else T (E, x, E)
          | aux (T (a, y, b), track) =
            if Element.lt(x, y) then T (aux (a, track), y, b)
            else T (a, y, aux (b, y))
    in
        aux (s, y) handle ALREADY_IN_SET => s
    end

(* exercise 2.5.a *)
fun complete (_, 0) = E
  | complete (x, d) =
    let val child = complete (x, d-1)
    in
        T (child, x, child)
    end

(* exercise 2.5.b *)
fun complete2 (_, 0) = E
  | complete2 (x, 1) = T (E, x, E)
  | complete2 (x, d) =
    let val half = (d-1) div 2;
        val subtree = complete2 (x, half)
    in
        if (d-1) mod 2 = 0 then
            T (subtree, x, subtree)
        else 
            let val subtree2 = complete2 (x, half+1);
            in
                T (subtree, x, subtree2)
            end
    end
end

(* exercise 2.6 *)
signature FINITEMAP =
sig
    type Key
    type 'a Map

    exception NOTFOUND

    val empty : 'a Map
    val bind  : Key * 'a * 'a Map -> 'a Map
    val lookup : Key * 'a Map -> 'a (* raise NOTFOUND if key is not found *)
end

functor UnbalancedMap (Element : ORDERED) : FINITEMAP =
struct
type Key = Element.T
datatype 'a Tree = E | T of 'a Tree * Key * 'a * 'a Tree
type 'a Map = 'a Tree

exception NOTFOUND

val empty = E

fun bind (k, v, E) = T (E, k, v, E)
  | bind (k, v, T (l, k', v', r)) =
    if Element.lt(k, k') then T (bind (k, v, l), k', v', r)
    else if Element.lt(k', k) then T (l, k', v', bind (k, v, r))
    else T (l, k, v, r)

fun lookup (_, E) = raise NOTFOUND
  | lookup (k, m as T (l, key, value, r)) =
    let fun aux (E, (key, value)) = if Element.eq(key, k) then value
                                    else raise NOTFOUND
          | aux (T (l, key, value, r), track) =
            if Element.lt(k, key) then aux (l, track)
            else aux (r, (key, value))
    in
        aux (m, (key, value))
    end
end
