signature Ordered = sig
    type T

    val eq  : T * T -> bool
    val lt  : T * T -> bool
    val leq : T * T -> bool
end

signature Heap = sig
    structure Elem : Ordered

    (* Won't work inheritance for functors without this definition *)
    datatype Heap = E | T of int * Elem.T * Heap * Heap

    val empty     : Heap
    val isEmpty   : Heap -> bool

    val insert    : Elem.T * Heap -> Heap
    val merge     : Heap * Heap -> Heap

    val findMin   : Heap -> Elem.T  (* raises Empty if heap is empty *)
    val deleteMin : Heap -> Heap    (* raises Empty if heap is empty *)
end

functor LeftistHeap (Element : Ordered) : Heap = struct
        structure Elem = Element

        datatype Heap = E | T of int * Elem.T * Heap * Heap

        fun rank E = 0
          | rank (T (r,_,_,_)) = r
        fun makeT (x, a, b) =
            if rank a >= rank b then T (rank b + 1, x, a, b)
            else T (rank a + 1, x, b, a)

        val empty = E
        fun isEmpty E = true | isEmpty  _ = false

        fun merge (h, E) = h
          | merge (E, h) = h
          | merge (h1 as T (_, x, a1, b1), h2 as T (_, y, a2, b2)) =
            if Elem.leq (x, y) then makeT (x, a1, merge (b1, h2))
            else makeT (y, a2, merge (h1, b2))

        fun insert (x, h) = merge (T (1, x, E, E), h)

        fun findMin E = raise Empty
          | findMin (T (_, x, a, b)) = x
        fun deleteMin E = raise Empty
          | deleteMin (T (_, x, a, b)) = merge (a, b)
        end

functor LeftistHeap_3_2 (Base : Heap) : Heap = struct
        structure Elem = Base.Elem
        datatype Heap = datatype Base.Heap

        fun rank E = 0
          | rank (T (r,_,_,_)) = r
        fun makeT (x, a, b) =
            if rank a >= rank b then T (rank b + 1, x, a, b)
            else T (rank a + 1, x, b, a)

        val empty     = Base.empty
        val isEmpty   = Base.isEmpty

        val merge     = Base.merge

        fun insert (x, E) = T (1, x, E, E)
          | insert (x, h as T (r, y, a, b)) =
            if Elem.leq (x, y) then makeT (x, E, h)
            else makeT (y, a, insert (x, b))

        val findMin   = Base.findMin
        val deleteMin = Base.deleteMin
        end

signature Heap_3_3 = sig
    include Heap

    val fromList : Elem.T list -> Heap
end

functor LeftistHeap_3_3 (Base : Heap) : Heap_3_3 = struct
        structure Elem = Base.Elem
        datatype Heap = datatype Base.Heap

        fun rank E = 0
          | rank (T (r,_,_,_)) = r
        fun makeT (x, a, b) =
            if rank a >= rank b then T (rank b + 1, x, a, b)
            else T (rank a + 1, x, b, a)

        val empty     = Base.empty
        val isEmpty   = Base.isEmpty

        val merge     = Base.merge

        fun insert (x, E) = T (1, x, E, E)
          | insert (x, h as T (r, y, a, b)) =
            if Elem.leq (x, y) then makeT (x, E, h)
            else makeT (y, a, insert (x, b))

        val findMin   = Base.findMin
        val deleteMin = Base.deleteMin

        val fromList =
            let fun aux [] = [empty]
                  | aux [x] = [x]
                  | aux [x, y] = [merge (x, y)]
                  | aux (x::y::ys) = aux (merge (x, y) :: aux ys)
            in (hd o aux o map (fn x => T (1, x, E, E)))
            end
        end

functor LeftistHeap_3_4 (Base : Heap) : Heap = struct
        structure Elem = Base.Elem
        datatype Heap = datatype Base.Heap

        fun size E = 0
          | size (T (r,_,_,_)) = r
        fun makeT (x, a, b) =
            let val s_a = size a
                and s_b = size b
            in
                if s_a >= s_b then T (s_a + s_b + 1, x, a, b)
                else T (s_a + s_b + 1, x, b, a)
            end

        val empty     = Base.empty
        val isEmpty   = Base.isEmpty

        val merge     = Base.merge

        val insert    = Base.insert

        val findMin   = Base.findMin
        val deleteMin = Base.deleteMin
        end
