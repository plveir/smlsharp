structure HashTbl =
struct

  type 'v bucket = (word * string * 'v) list

  exception NotFound

  datatype hashtbl = HASH of 
    {hash: string -> word,
      eq: string * string -> bool,
      buckets: (Dynamic.void Dynamic.dyn bucket) Array.array ref,
      size: int ref}

  fun size (HASH {size,...}) : int = !size

  fun new_buckets sz = Array.array (sz, nil)

  (* hash関数(string -> word)と等価性判定関数(string, string) -> boolを受け取ってhashtblを返す *)
  fun create {hash: string -> word, eq: string * string -> bool} : hashtbl =
      HASH {hash=hash,
        eq=eq,
        buckets=ref (new_buckets 32),
        size=ref 0}

  (* hash関数から得られたwをarrszに丸める（衝突の恐れはある） *)
  fun idx arrsz w =
      Word.toInt(Word.andb(Word.fromInt (arrsz-1),w))

  fun maybeResize (t as HASH {buckets,...} : hashtbl) : int =
      let val arrsz = Array.length (!buckets)
      in if size t > arrsz  then
          let val new_arrsz = arrsz+arrsz
            val new_arr = new_buckets new_arrsz
            fun upd (t as (w,_,_)) =
                let val i = idx new_arrsz w
                in Array.update(new_arr,i,t::Array.sub(new_arr,i))
                end
          in Array.app (List.app upd) (!buckets)
            ; buckets := new_arr
            ; new_arrsz
          end
        else arrsz
      end

  (* operations on buckets *)
  fun look eq k0 nil = NONE
    | look eq k0 ((_,k,v)::xs) =
      if eq(k0,k) then SOME v else look eq k0 xs

  fun rem eq k0 acc nil = rev acc
    | rem eq k0 acc ((x as (_,k,_))::xs) =
      if eq(k0,k) then rev acc @ xs
      else rem eq k0 (x::acc) xs


  (* operations for hashtbl *)
  fun add (t as HASH {hash,eq,buckets,size}: hashtbl) (k: string) v : hashtbl =
      let 
        val arrsz = maybeResize t
        val w = hash k
        val i = idx arrsz w
        val b = Array.sub(!buckets,i)
      in 
        (case look eq k b of
          SOME _ =>
            Array.update(!buckets, i, (w,k,v)::rem eq k nil b)
        | NONE =>
          ( Array.update(!buckets, i, (w,k,v) :: b)
            ; size := !size + 1 )); t
      end

  fun find (t as HASH {hash,eq,buckets,size}: hashtbl) (k: string): Dynamic.void Dynamic.dyn =
    let
      val arrsz = Array.length (!buckets)
    in 
      case look eq k (Array.sub(!buckets, idx arrsz (hash k))) of
        SOME v => v
      | NONE => raise NotFound
    end

  (* fun ('a#reify) find (t as HASH {hash,eq,buckets,size}: HashTblTy.hashtbl) (k: string): 'a =
    let
      val arrsz = Array.length (!buckets)
    in 
      case look eq k (Array.sub(!buckets, idx arrsz (hash k))) of
        SOME v => _dynamic v as 'a
      | NONE => raise NotFound
    end *)

  (* FIXME *)
  val hashFn = 
    let
      fun compose f g = let fun h x = g (f x) in h end
    in
      (compose String.size Word.fromInt)
    end

  val eqFn = (op =)

  fun default () = create {hash=hashFn, eq=eqFn}

end