
module Data.Array.Repa.Plugin.Convert.ToGHC.Prim
        ( convertPrim
        , convertPolytypicPrim
        , isPolytypicPrimName

        , getPrim_add
        , getPrim_mul
        , getPrim_next
        , getPrim_writeByteArrayOpM
        , getPrim_readByteArrayOpM)
where
import Data.Array.Repa.Plugin.Convert.ToGHC.Type
import Data.Array.Repa.Plugin.Convert.ToGHC.Var

import qualified CoreSyn                 as G
import qualified Type                    as G
import qualified TysPrim                 as G
import qualified Var                     as G
import qualified UniqSupply              as G

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Flow.Prim      as D
import qualified DDC.Core.Flow.Compounds as D


-- | Convert a primop that has the same definition independent 
--   of its type arguments.
convertPrim 
        :: Env -> Env
        -> D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPrim _kenv tenv n 
 = case n of 
        D.NameOpFlow D.OpFlowRateOfStream
         | Just gv      <- findImportedPrimVar (envGuts tenv) "primRateOfStream"
         ->     return (G.Var gv, G.varType gv)

        _       -> errorMissingPrim n


-------------------------------------------------------------------------------
-- | Convert a primop that has a different definition depending on the type
--   argument. If primops handled by this function must be detected by
--   `isPolyTypicPrimName` below.
convertPolytypicPrim 
        :: Env -> Env
        -> D.Name -> D.Type D.Name
        -> G.UniqSM (G.CoreExpr, G.Type)

convertPolytypicPrim kenv _tenv n tArg
 = case n of
        D.NamePrimArith D.PrimArithAdd
         | Just gv      <- getPrim_add (envGuts kenv) tArg
         ->     return  (G.Var gv, G.varType gv)

        D.NamePrimArith D.PrimArithMul
         | Just gv      <- getPrim_mul (envGuts kenv) tArg
         ->     return  (G.Var gv, G.varType gv)

        D.NameOpStore D.OpStoreNext 
         | Just gv     <- getPrim_next (envGuts kenv) tArg
         ->     return  (G.Var gv, G.varType gv)

        D.NameOpStore D.OpStoreNewArray
         | Just gv      <- getPrim_newByteArrayOpM (envGuts kenv) tArg
         ->     return  ( G.App (G.Var gv)         (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        D.NameOpStore D.OpStoreReadArray
         | Just gv      <- getPrim_readByteArrayOpM (envGuts kenv) tArg
         ->     return  ( G.App (G.Var gv) (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        D.NameOpStore D.OpStoreWriteArray
         | Just gv <- getPrim_writeByteArrayOpM (envGuts kenv) tArg
         ->     return  ( G.App (G.Var gv) (G.Type G.realWorldTy)
                        , G.applyTy (G.varType gv) G.realWorldTy)

        D.NameOpLoop D.OpLoopLoopN
         | Just gv      <- findImportedPrimVar (envGuts kenv) "primLoop"
         ->     return  ( G.Var gv
                        , G.varType gv)

        _       -> errorMissingPrim n


-- | Check for the name of a primitive that must be handled polytypically.
isPolytypicPrimName :: D.Name -> Bool
isPolytypicPrimName n
 = elem n
        [ D.NamePrimArith D.PrimArithAdd
        , D.NamePrimArith D.PrimArithMul
        , D.NameOpStore   D.OpStoreNext
        , D.NameOpStore   D.OpStoreNewArray
        , D.NameOpStore   D.OpStoreReadArray 
        , D.NameOpStore   D.OpStoreWriteArray 
        , D.NameOpLoop    D.OpLoopLoopN ]


-------------------------------------------------------------------------------
errorMissingPrim n
        = error $ "repa-plugin.toGHC.convertPrim: no match for " ++ show n


-------------------------------------------------------------------------------
getPrim_add guts t
 | t == D.tInt  = findImportedPrimVar guts "primAddInt"
 | otherwise    = Nothing

getPrim_mul guts t
 | t == D.tInt  = findImportedPrimVar guts "primMulInt"
 | otherwise    = Nothing

getPrim_next guts t
 | t == D.tInt  = findImportedPrimVar guts "primNextInt"
 | otherwise    = Nothing

getPrim_newByteArrayOpM   guts t
 | t == D.tInt  = findImportedPrimVar guts "primNewByteArray"
 | otherwise    = Nothing

getPrim_readByteArrayOpM  guts t
 | t == D.tInt  = findImportedPrimVar guts "primReadIntArray"
 | otherwise    = Nothing

getPrim_writeByteArrayOpM guts t
 | t == D.tInt  = findImportedPrimVar guts "primWriteIntArray"
 | otherwise    = Nothing


