module Eval (eval) where

import qualified Data.Map as M

import Data.Maybe (fromJust)

import AST (Tm(..), substTop)

eval :: Tm -> Tm
eval tm = case tm of
  TmApp func arg ->
    let (TmAbs _ _ body) = eval func
        arg' = eval arg
    in eval $ substTop arg' body
  TmRec tms -> TmRec $ eval <$> tms
  TmVnt lbl tm' -> TmVnt lbl $ eval tm'
  TmProj tm' label ->
    let (TmRec tms) = eval tm'
    in fromJust $ M.lookup label tms
  TmIf tm1 tm2 tm3 ->
    let
      (TmBool b) = eval tm1
      tm2' = eval tm2
      tm3' = eval tm3
    in if b then tm2' else tm3'
  tm' -> tm'
