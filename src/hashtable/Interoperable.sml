structure Interoperable =
struct 
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