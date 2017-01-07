module Type
  (deriveType) where

import qualified Data.Map as M
import qualified Data.Set as S

import AST (Tm(..), Ty(..), Context)
import Print (showTy)

import Control.Monad (unless)
import Control.Applicative((<|>))
import Data.Maybe (fromJust)
import Data.List (intercalate)

deriveType :: Context -> Tm -> Either String Ty
deriveType ctxt tm = case tm of
  TmVar binder -> let (_,ty) = ctxt !! binder in Right ty
  TmAbs ident ty body ->
    let ctxt' = (ident,ty):ctxt in
      TyArr ty <$> (deriveType ctxt' body)
  TmApp tm1 tm2 -> do
    tyFunc <- deriveType ctxt tm1
    case tyFunc of
      (TyArr tyFunc1 tyFunc2) -> do
        tyArg <- deriveType ctxt tm2
        deriveSubsumed tyArg tyFunc1
        return tyFunc2
      _ -> Left "Expected function"
  TmRec tms -> TyRec <$> traverse (deriveType ctxt) tms
  TmProj tm' label -> do
    ty <- deriveType ctxt tm'
    case ty of
      (TyRec tys) -> maybe (Left $ "Label " ++ label ++ " is not in record") return (M.lookup label tys)
      _ -> Left "Expected record"
  TmBool _ -> Right TyBool
  TmIf tm1 tm2 tm3 -> do
    ty1 <- deriveType ctxt tm1
    unless (ty1 == TyBool) (Left $ "Expected condition to have type Bool but got" ++ showTy ty1)
    ty2 <- deriveType ctxt tm2
    ty3 <- deriveType ctxt tm3
    return $ deriveJoin ty2 ty3

deriveSubsumed :: Ty -> Ty -> Either String ()
deriveSubsumed _ (TyTop) = Right ()
deriveSubsumed (TyRec tys1) (TyRec tys2) =
  let missingLabels = S.toList $ M.keysSet tys2 `S.difference` M.keysSet tys1
      expectedLabels = M.keys tys2
      getTys tys = map (\label -> fromJust $ M.lookup label tys) expectedLabels
      subderivations = zipWith deriveSubsumed (getTys tys1) (getTys tys2)
  in do
    unless (null missingLabels) (Left $ "Expected fields in record: " ++ intercalate ", " missingLabels)
    sequence_ subderivations
deriveSubsumed (TyArr ty11 ty12) (TyArr ty21 ty22) = do
  deriveSubsumed ty21 ty11
  deriveSubsumed ty12 ty22
deriveSubsumed ty1 ty2 | ty1 == ty2 = Right ()
deriveSubsumed ty1 ty2 = Left $ "Expected " ++ showTy ty2 ++ " to subsume " ++ showTy ty1

deriveJoin :: Ty -> Ty -> Ty
deriveJoin (TyRec tys1) (TyRec tys2) =
  let commonLabels = S.toList $ M.keysSet tys1 `S.intersection` M.keysSet tys2
      getTys tys = map (\label -> fromJust $ M.lookup label tys) commonLabels
      joinTys = zipWith deriveJoin (getTys tys1) (getTys tys2)
  in TyRec $ M.fromList (zip commonLabels joinTys)
deriveJoin (TyArr ty11 ty12) (TyArr ty21 ty22) = case deriveMeet ty11 ty21 of
  Right ty1 -> let ty2 = deriveJoin ty12 ty22 in TyArr ty1 ty2
  Left _ -> TyTop
deriveJoin ty1 ty2 | ty1 == ty2 = ty1
deriveJoin _ _ = TyTop

deriveMeet :: Ty -> Ty -> Either String Ty
deriveMeet (TyRec tys1) (TyRec tys2) =
  let unionLabels = S.toList $ M.keysSet tys1 `S.union` M.keysSet tys2
      getTys tys = map (\label -> M.lookup label tys) unionLabels
      meetTys = sequence $ zipWith (\x y -> fromJust $ deriveMeet <$> x <*> y <|> Right <$> x <|> Right <$> y) (getTys tys1) (getTys tys2)
  in TyRec <$> M.fromList <$> zip unionLabels <$> meetTys
deriveMeet (TyArr t11 t12) (TyArr t21 t22) = do
  let t1 = deriveJoin t11 t21
  t2 <- deriveMeet t12 t22
  return $ TyArr t1 t2
deriveMeet ty1 ty2 | ty1 == ty2 = Right ty1
deriveMeet ty1 ty2 = Left $ "Could not find meet of types " ++ showTy ty1 ++ " and " ++ showTy ty2

