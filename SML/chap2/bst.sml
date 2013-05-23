signature Ordered = sig
    type T

    val eq  : T * T -> bool
    val lt  : T * T -> bool
    val leq : T * T -> bool
end

signature Set = sig
    (* Можно и без структуры (как в тексте книги),
     * но так удобнее делить код для упражнений *)
    structure Elem : Ordered
    datatype Tree = E | T of Tree * Elem.T * Tree
    type Set = Tree

    val empty    : Set
    val insert   : Elem.T * Set -> Set
    val member   : Elem.T * Set -> bool
end

functor UnbalancedSet (Element : Ordered) : Set = struct
        structure Elem = Element
        datatype Tree = E | T of Tree * Elem.T * Tree
        type Set = Tree

        val empty = E

        fun member (x, E) = false
          | member (x, T (a, y, b)) =
            if Elem.lt(x, y) then member(x, a)
            else if Elem.lt(y, x) then member(x, b)
            else true

        fun insert (x, E) = T (E, x, E)
          | insert (x , s as T (a, y, b)) =
            if Elem.lt(x, y) then T (insert (x, a), y, b)
            else if Elem.lt(y, x) then T (a, y, insert (x, b))
            else s
        end

(* exercise 2.2 *)
functor UnbalancedSet_2_2 (Base : Set) : Set = struct
        structure Elem = Base.Elem
        datatype Tree = datatype Base.Tree
        type Set = Tree

        val empty = Base.empty

        fun member (x, E) = false
          | member (x, T (a, y, b)) =
            let fun aux (E, track) = Elem.eq(x, track)
                  | aux (T (a, y, b), track) =
                    if Elem.lt(x, y) then aux(a, track)
                    else aux(b, y)
            in
                aux(T (a, y, b), y)
            end

        val insert = Base.insert
        end

(* exercise 2.3 *)
functor UnbalancedSet_2_3 (Base : Set) : Set = struct
        structure Elem = Base.Elem
        datatype Tree = datatype Base.Tree
        type Set = Tree

        val empty = Base.empty

        val member = Base.member

        exception AlreadyInSet

        fun insert (x, E) = T (E, x, E)
          | insert (x , s as T (a, y, b)) =
            let fun aux E = T (E, x, E)
                  | aux (T (a, y, b)) =
                    if Elem.lt(x, y) then T (aux a, y, b)
                    else if Elem.lt(y, x) then T (a, y, aux b)
                    else raise AlreadyInSet
            in
                aux s handle AlreadyInSet => s
            end
        end

(* exercise 2.4 *)
functor UnbalancedSet_2_4 (Base : Set) : Set = struct
        structure Elem = Base.Elem
        datatype Tree = datatype Base.Tree
        type Set = Tree

        val empty = Base.empty

        val member = Base.member

        exception AlreadyInSet

        fun insert (x, E) = T (E, x, E)
          | insert (x, s as T (a, y, b)) =
            let fun aux (E, track) =
                    if Elem.eq(x, track) then raise AlreadyInSet
                    else T (E, x, E)
                  | aux (T (a, y, b), track) =
                    if Elem.lt(x, y) then T (aux (a, track), y, b)
                    else T (a, y, aux (b, y))
            in
                aux (s, y) handle AlreadyInSet => s
            end
        end

(* exercise 2.5.a *)
signature Set_2_5 = sig
    include Set
    val complete : Elem.T * int -> Tree
end

functor UnbalancedSet_2_5_a (Base : Set) : Set_2_5 = struct
        structure Elem = Base.Elem
        datatype Tree = datatype Base.Tree
        type Set = Tree

        val empty = Base.empty

        val member = Base.member

        exception AlreadyInSet

        val insert = Base.insert

        fun complete (_, 0) = E
          | complete (x, d) =
            let val child = complete (x, d-1)
            in
                T (child, x, child)
            end
        end

(* exercise 2.5.b *)
functor UnbalancedSet_2_5_a (Base : Set) : Set_2_5 = struct
        structure Elem = Base.Elem
        datatype Tree = datatype Base.Tree
        type Set = Tree

        val empty = Base.empty

        val member = Base.member

        exception AlreadyInSet

        val insert = Base.insert

        fun complete (_, 0) = E
          | complete (x, 1) = T (E, x, E)
          | complete (x, d) =
            let val half = (d-1) div 2;
                val subtree = complete (x, half)
            in
                if (d-1) mod 2 = 0 then
                    T (subtree, x, subtree)
                else
                    let val subtree2 = complete (x, half+1);
                    in
                        T (subtree, x, subtree2)
                    end
            end
        end

(* exercise 2.6 *)
signature FiniteMap = sig
    type Key
    type 'a Map

    exception NotFound

    val empty : 'a Map
    val bind  : Key * 'a * 'a Map -> 'a Map
    val lookup : Key * 'a Map -> 'a (* raise NotFound if key is not found *)
end

functor UnbalancedMap (Element : Ordered) : FiniteMap = struct
        type Key = Element.T
        datatype 'a Tree = E | T of 'a Tree * Key * 'a * 'a Tree
        type 'a Map = 'a Tree

        exception NotFound

        val empty = E

        fun bind (k, v, E) = T (E, k, v, E)
          | bind (k, v, T (l, k', v', r)) =
            if Element.lt(k, k') then T (bind (k, v, l), k', v', r)
            else if Element.lt(k', k) then T (l, k', v', bind (k, v, r))
            else T (l, k, v, r)

        fun lookup (_, E) = raise NotFound
          | lookup (k, m as T (l, key, value, r)) =
            let fun aux (E, (key, value)) = if Element.eq(key, k) then value
                                            else raise NotFound
                  | aux (T (l, key, value, r), track) =
                    if Element.lt(k, key) then aux (l, track)
                    else aux (r, (key, value))
            in
                aux (m, (key, value))
            end
        end
