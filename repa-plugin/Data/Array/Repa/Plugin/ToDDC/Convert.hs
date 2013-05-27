
module Data.Array.Repa.Plugin.ToDDC.Convert
        (convertModGuts)
where
import Data.Array.Repa.Plugin.ToDDC.Convert.Base
import Data.Array.Repa.Plugin.ToDDC.Convert.Type
import Data.Array.Repa.Plugin.ToDDC.Convert.Var
import Data.Array.Repa.Plugin.FatName
import Control.Monad
import Data.Either
import Data.List
import           Data.Map                 (Map)
import qualified Data.Map               as Map
import qualified Data.Set               as Set

import qualified DDC.Core.Exp            as D
import qualified DDC.Core.Module         as D
import qualified DDC.Core.Compounds      as D
import qualified DDC.Core.Flow           as D
import qualified DDC.Core.Collect        as D
import qualified DDC.Type.Env            as D

import qualified CoreSyn                as G
import qualified HscTypes               as G
import qualified TyCon                  as G
import qualified Type                   as G
import qualified Var                    as G


-------------------------------------------------------------------------------
-- | Convert a GHC module to Disciple Core Flow.
--
--   This is a raw conversion of the AST. We still need to detect the primitive
--   flow operators before we can run the lowering pass.
--
--   We get back a Disciple Core Flow module containing all the top-level
--   bindings that we could convert, and a list of reasons why conversion 
--   for the other bindings failed.
--
convertModGuts 
        :: G.ModGuts 
        -> (D.Module () FatName, [Fail])

convertModGuts guts
 = let  (bnds', fails)  
                = convertTopBinds $ G.mg_binds guts
        body    = D.xLets () bnds' (D.xUnit ())

        -- Find the free variables in the module body
        freeX   = D.freeX D.empty body
        -- And add them all to the import types
        importT = foldl (insertImport convertType) Map.empty
                $ Set.toList freeX

        -- Then find the type constructors mentioned in the imports
        freeT   = Set.unions (map (D.supportTyCon . D.support D.empty D.empty . snd . snd) $ Map.toList importT)
        -- And add them to the import kinds
        importK = foldl (insertImport convertKind) Map.empty
                $ Set.toList freeT

        mm'     = D.ModuleCore
                { D.moduleName          = D.ModuleName ["Flow"]
                , D.moduleExportKinds   = Map.empty
                , D.moduleExportTypes   = Map.empty
                , D.moduleImportKinds   = importK
                , D.moduleImportTypes   = importT
                , D.moduleBody          = body }

   in   (mm', fails)


-- | Convert a type/kind and add it to the import map, if conversion succeeds.
insertImport :: (G.Type -> Either Fail (D.Type FatName))
             -> Map FatName (D.QualName FatName, D.Type FatName)
             -> D.Bound FatName
             -> Map FatName (D.QualName FatName, D.Type FatName)
insertImport c m bound
 | D.UName n@(FatName ghc _) <- bound
 , GhcNameVar v              <- ghc
 = ins n (c $ G.varType v)
 | D.UName n@(FatName ghc _) <- bound
 , GhcNameTyCon tc           <- ghc
 = ins n (c $ G.tyConKind tc)
 | otherwise
 = m
 where
  ins _ (Left _)  = m
  ins n (Right t) = Map.insert n (D.QualName (D.ModuleName []) n, t) m


-- Bindings -------------------------------------------------------------------
-- | Convert top-level bindings.
convertTopBinds 
        :: [G.CoreBind] 
        -> ([D.Lets () FatName], [Fail])

convertTopBinds bnds
 = let  results         = map convertTopBind bnds
        (fails, bnds')  = partitionEithers results
   in   (bnds', fails)


-- | Convert a possibly recursive top-level binding.
convertTopBind 
        :: G.CoreBind 
        -> Either Fail (D.Lets () FatName)

convertTopBind bnd
 = case bnd of
        G.NonRec b x      
         -> case convertBinding (b, x) of
                Left fails      -> Left   $ FailInBinding b fails
                Right (b', x')  -> return $ D.LLet D.LetStrict b' x'

        G.Rec bxs
         -> let bs   = map fst bxs
            in  Left $ FailNoRecursion bs


-- | Convert a single binding.
--   TODO: select the bindings we care about more generally.
convertBinding 
        :: (G.CoreBndr, G.CoreExpr)
        -> Either Fail (D.Bind FatName, D.Exp () FatName)

convertBinding (b, x)
 = do   n       <- convertVarName b
        case n of
         D.NameVar str
           | isPrefixOf "lower" str
           -> do x'      <- convertExpr x
                 fn'     <- convertFatName b
                 t'      <- convertVarType b
                 return  $ (D.BName fn' t', x')

           | otherwise
           -> Left FailNotMarked

         _ -> Left (FailDodgyTopLevelBindingName n)


-- Expr -----------------------------------------------------------------------
-- | Slurp an expression.
convertExpr :: G.CoreExpr 
            -> Either Fail (D.Exp () FatName)

convertExpr xx
 = case xx of
        G.Var v
         -> do  name'   <- convertFatName v
                return  $ D.XVar () (D.UName name')

        G.Lit lit
         -> do  lit'    <- convertLiteral lit
                return  $ D.XCon () lit'

        G.App x1 x2
         -> do  x1'     <- convertExpr x1
                x2'     <- convertExpr x2
                return  $ D.XApp () x1' x2'

        G.Lam b x
         -> do  x'      <- convertExpr x
                n'      <- convertFatName b
                t'      <- convertVarType b
                return  $  D.XLam () (D.BName n' t') x'

        G.Let (G.NonRec b x1) x2
         -> do  n'      <- convertFatName b
                t'      <- convertVarType b
                x1'     <- convertExpr x1
                x2'     <- convertExpr x2
                return  $  D.XLet () (D.LLet D.LetStrict 
                                                (D.BName n' t') x1') x2'

        -- Cannot convert recursive bindings.
        G.Let (G.Rec bxs) _
                        -> Left (FailNoRecursion $ map fst bxs)

        -- We don't handle case expressions yet.
        G.Case{}        -> Left FailUnhandledCase

        -- We can't represent type casts/
        G.Cast{}        -> Left FailNoCasts

        -- Just ditch tick nodes, we probably don't need them.
        G.Tick _ x      -> convertExpr x

        -- Type arguments.
        G.Type t        -> liftM D.XType (convertType t)

        -- Cannot convert coercions.
        G.Coercion{}    -> Left FailNoCoercions
