structure Interoperable =
struct
  fun ('a#reify,'b#reify#{}) add (t: 'b) (k: string) (v: 'a): 'b =
    let
      val dynamic_t = Dynamic.dynamic t
      val dynamic_v = Dynamic.dynamic v
    in
      case (Dynamic.dynamicToTy dynamic_t) of
        Dynamic.RECORDty _ => 
          let
            val v_term = Dynamic.dynamicToTerm dynamic_v
          in
            _dynamic (Dynamic.#> (k, v_term) dynamic_t) as 'b
          end
      | Dynamic.HASHty =>
        let
          val h = _dynamic dynamic_t as HashTbl.hashtbl
        in 
          _dynamic (Dynamic.dynamic (HashTbl.add h k dynamic_v)) as 'b
        end
      | _ => raise Fail "Unreachable fail"
    end

  fun ('a#reify,'b#reify#{}) find (t: 'b) (k: string): 'a =
    let
      val dynamic_t = Dynamic.dynamic t
    in
      case (Dynamic.dynamicToTy dynamic_t) of
        Dynamic.RECORDty _ => 
          _dynamic (Dynamic.## k dynamic_t) as 'a
      | Dynamic.HASHty =>
        let
          val h = _dynamic dynamic_t as HashTbl.hashtbl
        in 
          _dynamic (HashTbl.find h k) as 'a 
        end
      | _ => raise Fail "Unreachable fail"
    end
end