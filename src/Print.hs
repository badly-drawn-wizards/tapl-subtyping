module Print
  ( showTm
  , showTy ) where

import AST (Tm(..), Ty(..), Binding, Context, Label)

import qualified Data.Map as M
import qualified Data.List as L

showTm :: Context -> Tm -> String
showTm ctxt tm = case tm of
  TmVar binding -> let (name,_) = ctxt !! binding in name
  TmAbs name ty body ->
    let ctxt' = (name,ty):ctxt
    in "fun " ++ showBinding (name,ty) ++ " => " ++ (showTm ctxt' body)

  TmApp tm1@(TmAbs _ _ _) tm2@(TmApp _ _) -> parens (showTm ctxt tm1) ++ " " ++ parens (showTm ctxt tm2)
  TmApp tm1@(TmAbs _ _ _) tm2 -> parens (showTm ctxt tm1) ++ " " ++ showTm ctxt tm2
  TmApp tm1 tm2@(TmApp _ _) -> showTm ctxt tm1 ++ " " ++ parens (showTm ctxt tm2)
  TmApp tm1 tm2 -> showTm ctxt tm1 ++ " " ++ showTm ctxt tm2

  TmRec tms -> "{" ++ (L.intercalate ", " $ map (showAssignment ctxt) $ M.toList tms) ++ "}"
  TmProj tm@(TmAbs _ _ _) label -> parens (showTm ctxt tm) ++ "." ++ label
  TmProj tm@(TmApp _ _) label -> parens (showTm ctxt tm) ++ "." ++ label
  TmProj tm label -> showTm ctxt tm ++ "." ++ label

  TmBool b -> if b then "true" else "false"
  TmIf tm1 tm2 tm3 -> "if " ++ showTm ctxt tm1 ++ " then " ++ showTm ctxt tm2 ++ " else " ++ showTm ctxt tm3 ++ " end"

showBinding :: Binding -> String
showBinding (name,ty) = name ++ " : " ++ (showTy ty)

showAssignment :: Context -> (Label, Tm) -> String
showAssignment ctxt (name,tm) = name ++ " = " ++ (showTm ctxt tm)

showTy :: Ty -> String
showTy ty = case ty of
  TyTop -> "Top"
  TyArr tm1@(TyArr _ _) tm2 -> parens (showTy tm1) ++ " -> " ++ showTy tm2
  TyArr tm1 tm2 -> showTy tm1 ++ " -> " ++ showTy tm2
  TyRec tys -> "{" ++ (L.intercalate ", " $ map showBinding $ M.toList tys) ++ "}"
  TyBool -> "Bool"

parens :: String -> String
parens s = "(" ++ s ++ ")"

