module AST
  ( Ty(..)
  , Tm(..)
  , Binding
  , Context
  , findInContext
  , Label
  , traverseBinders
  , substTop
  ) where

import qualified Data.Map as M
import qualified Data.List as L

type Binding = (String, Ty)
type Context = [Binding]
type Label = String

data Ty
  = TyRec (M.Map Label Ty)
  | TyVnt (M.Map Label Ty)
  | TyArr Ty Ty
  | TyTop
  | TyBool
  deriving (Eq, Show)

data Tm
  = TmVar Int
  | TmAbs String Ty Tm
  | TmApp Tm Tm
  | TmRec (M.Map Label Tm)
  | TmVnt Label Tm
  | TmProj Tm Label
  | TmCase Tm [(Label, String, Tm)] Tm
  | TmBool Bool
  | TmIf Tm Tm Tm
  deriving (Show)

traverseBinders :: (Int -> Int -> Tm) -> Tm -> Tm
traverseBinders f t = walk 0 t where
  walk c tm = case tm of
    TmVar binder -> f binder c
    TmAbs ident ty body -> TmAbs ident ty (walk (c+1) body)
    TmApp tm1 tm2 -> TmApp (walk c tm1) (walk c tm2)
    TmRec tms -> TmRec $ (walk c <$> tms)
    TmVnt lbl tm' -> TmVnt lbl $ walk c tm'
    TmCase tm css els -> TmCase (walk c tm) ((\ (lbl,ident,tm) -> (lbl,ident,walk (c+1) tm)) <$> css) (walk c els)
    TmProj tm' lbl -> TmProj (walk c tm') lbl
    TmBool b -> TmBool b
    TmIf tm1 tm2 tm3 -> TmIf (walk c tm1) (walk c tm2) (walk c tm3)

findInContext :: String -> Context -> Maybe Int
findInContext ident = L.findIndex ((==ident).fst)

shift :: Int -> Tm -> Tm
shift d = traverseBinders $ \ binder depth ->
  TmVar $ if binder >= depth then binder + d else binder

subst :: Tm -> Int -> Tm -> Tm
subst s v = traverseBinders $ \ binder depth -> if binder == v+depth then shift depth s else TmVar binder

substTop :: Tm -> Tm -> Tm
substTop s t = shift (-1) $ subst s 0 t

