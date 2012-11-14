structure TyReduce =
struct
local
  structure T = Types
  structure TU = TypesUtils
  structure P = TyPrinters
  type ty = T.ty
  type varInfo = T.varInfo
  type path = T.path
  type btv = BoundTypeVarID.id
  fun bug s = Control.Bug ("TyReduce: " ^ s)
in
  type btvMap = ty BoundTypeVarID.Map.map
  val emptyBtvMap = BoundTypeVarID.Map.empty : btvMap

  fun evalBtv (btvMap:btvMap) (btv:btv) : ty =
       case BoundTypeVarID.Map.find(btvMap, btv) of
         NONE => T.BOUNDVARty btv
       | SOME ty => TyAlphaRename.copyTy TyAlphaRename.emptyBtvMap ty

  val foo = evalBtv
  fun evalSingletonTy (btvMap:btvMap) (singletonTy:T.singletonTy) : T.singletonTy =
      case singletonTy of
        T.INSTCODEty
          {
           oprimId,
           path,
           keyTyList : ty list,
           match : T.overloadMatch,
           instMap : T.overloadMatch OPrimInstMap.map
          } =>
        T.INSTCODEty
          {
           oprimId=oprimId,
           path=path,
           keyTyList = map (evalTy btvMap) keyTyList,
           match = evalOverloadMatch btvMap match,
           instMap = OPrimInstMap.map (evalOverloadMatch btvMap) instMap
          }
      | T.INDEXty (string, ty) => T.INDEXty (string, evalTy btvMap ty)
      | T.TAGty ty => T.TAGty (evalTy btvMap ty)
      | T.SIZEty ty => T.SIZEty (evalTy btvMap ty)
  and evalOverloadMatch (btvMap:btvMap) (overloadMatch:T.overloadMatch) 
      : T.overloadMatch =
      case overloadMatch  of
        T.OVERLOAD_EXVAR {exVarInfo, instTyList} =>
        T.OVERLOAD_EXVAR
          {exVarInfo = evalExVarInfo btvMap exVarInfo,
           instTyList = map (evalTy btvMap) instTyList
          }
       | T.OVERLOAD_PRIM {primInfo, instTyList} =>
         T.OVERLOAD_PRIM
           {
            primInfo = evalPrimInfo btvMap primInfo,
            instTyList = map (evalTy btvMap) instTyList
           }
       | T.OVERLOAD_CASE (ty, map:T.overloadMatch TypID.Map.map) =>
         T.OVERLOAD_CASE
           (evalTy btvMap ty,
            TypID.Map.map (evalOverloadMatch btvMap) map
           )
  and evalExVarInfo (btvMap:btvMap) {path:path,ty:ty} : T.exVarInfo =
      {path=path, ty=evalTy btvMap ty}
  and evalPrimInfo (btvMap:btvMap) ({primitive, ty}:T.primInfo) : T.primInfo =
      {primitive=primitive, ty=evalTy btvMap ty}
  and evalOprimInfo (btvMap:btvMap) ({ty, path, id}:T.oprimInfo) : T.oprimInfo =
      {ty=evalTy btvMap ty, path=path,id=id}
  and evalConInfo (btvMap:btvMap) ({path, ty, id}:T.conInfo) : T.conInfo =
      {path=path, ty=evalTy btvMap ty, id=id}
  and evalExnInfo (btvMap:btvMap) ({path, ty, id}:T.exnInfo) : T.exnInfo =
      {path=path, ty=evalTy btvMap ty, id=id}
  and evalExExnInfo (btvMap:btvMap) ({path, ty}:T.exExnInfo) : T.exExnInfo =
      {path=path, ty=evalTy btvMap ty}
  and evalTy (btvMap:btvMap) (ty:ty) : ty =
      case TU.derefTy ty of
        T.SINGLETONty singletonTy =>
        T.SINGLETONty (evalSingletonTy btvMap singletonTy)
      | T.ERRORty => 
        ty
      | T.DUMMYty dummyTyID => 
        ty
      | T.TYVARty _ => raise bug "TYVARty in Optimize"
      | T.BOUNDVARty btv => 
        evalBtv btvMap btv
      | T.FUNMty (tyList, ty) =>
        T.FUNMty (map (evalTy btvMap) tyList, evalTy btvMap ty)
      | T.RECORDty (fields:ty LabelEnv.map) =>
        T.RECORDty (LabelEnv.map (evalTy btvMap) fields)
      | T.CONSTRUCTty
          {
           tyCon =
           {id : T.typId,
            path : path,
            iseq : bool,
            arity : int,
            runtimeTy : BuiltinTypeNames.bty,
            conSet : {hasArg:bool} SEnv.map,
            extraArgs : ty list,
            dtyKind : T.dtyKind
           },
           args : ty list
          } =>
        T.CONSTRUCTty
          {
           tyCon =
           {id = id,
            path = path,
            iseq = iseq,
            arity = arity,
            runtimeTy = runtimeTy,
            conSet = conSet,
            extraArgs = map (evalTy btvMap) extraArgs,
            dtyKind = evalDtyKind btvMap dtyKind
           },
           args = map (evalTy btvMap) args
          }
      | T.POLYty
          {
           boundtvars : T.btvEnv,
           body : ty
          } =>
        let
          val boundtvars = evalBtvEnv btvMap boundtvars
        in
          T.POLYty
            {
             boundtvars = boundtvars,
             body =  evalTy btvMap body
            }
        end
  and evalBtvEnv (btvMap:btvMap) (btvEnv:T.btvEnv) =
      BoundTypeVarID.Map.map (evalBtvkind btvMap) btvEnv
  and evalBtvkind (btvMap:btvMap) {tvarKind, eqKind} =
      {tvarKind=evalTvarKind btvMap tvarKind,
       eqKind=eqKind}
  and evalTvarKind btvMap tvarKind =
      case tvarKind of
        T.OCONSTkind tyList =>
        T.OCONSTkind (map (evalTy btvMap) tyList)
      | T.OPRIMkind
          {instances:ty list,
           operators:
           {
            oprimId : OPrimID.id,
            path : path,
            keyTyList : ty list,
            match : T.overloadMatch,
            instMap : T.overloadMatch OPrimInstMap.map
           } list
          } =>
        T.OPRIMkind
          {instances = map (evalTy btvMap) instances,
           operators =
           map
             (fn {oprimId, path, keyTyList, match, instMap} =>
                 {oprimId=oprimId,
                  path=path,
                  keyTyList = map (evalTy btvMap) keyTyList,
                  match = evalOverloadMatch btvMap match,
                  instMap = OPrimInstMap.map (evalOverloadMatch btvMap) instMap}
             )
             operators
          }
      | T.UNIV => T.UNIV
      | T.REC (fields:ty LabelEnv.map) =>
        T.REC (LabelEnv.map (evalTy btvMap) fields)
  and evalDtyKind btvMap dtyKind =
      case dtyKind of
        T.DTY => dtyKind
      | T.OPAQUE {opaqueRep:T.opaqueRep, revealKey:T.revealKey} =>
        T.OPAQUE {opaqueRep = evalOpaqueRep btvMap opaqueRep,
                  revealKey=revealKey}
      | T.BUILTIN bty => dtyKind
  and evalOpaqueRep btvMap opaueRep =
      case opaueRep of
        T.TYCON
          {id : T.typId,
           path : path,
           iseq : bool,
           arity : int,
           runtimeTy : BuiltinTypeNames.bty,
           conSet : {hasArg:bool} SEnv.map,
           extraArgs : ty list,
           dtyKind : T.dtyKind
          } =>
        T.TYCON
          {id = id,
           path = path,
           iseq = iseq,
           arity = arity,
           runtimeTy = runtimeTy,
           conSet = conSet,
           extraArgs = map (evalTy btvMap) extraArgs,
           dtyKind = evalDtyKind btvMap dtyKind
          }
      | T.TFUNDEF {iseq, arity, polyTy} =>
        T.TFUNDEF {iseq=iseq,
                   arity=arity,
                   polyTy = evalTy btvMap polyTy}

  fun evalTyVar (btvMap:btvMap) ({id, ty, path}:varInfo) =
      {id=id, path=path, ty=evalTy btvMap ty}

  fun applyTys (btvMap:btvMap) (btvEnv:T.btvEnv, instTyList:ty list) : btvMap =
      let
        val btvList = BoundTypeVarID.Map.listKeys btvEnv
        val btvBinds = ListPair.zip (btvList, instTyList)
        val btvMap = 
            foldl
              (fn ((btv,ty), btvMap) =>
                  BoundTypeVarID.Map.insert(btvMap, btv, ty))
              btvMap
              btvBinds
      in
        btvMap
      end
end
end
