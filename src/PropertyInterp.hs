{-# LANGUAGE DeriveGeneric #-}

module PropertyInterp ( interpProps ) where

import Ast
import Invalid

import Text.PrettyPrint.GenericPretty

-- In the lists index 0 is used for predifined variables: Title, Total, MaxPoints
-- This should refactored to real monadic programming

data PropertyValue
  = StrVal String
  | DoubVal Int
  deriving (Eq, Show, Generic)

instance Out PropertyValue

interpProps :: Judgement -> Either Invalid Judgement
interpProps j =
  case (interpJudgement j) of
    Right (jnew, _) -> Right jnew
    Left invalid -> Left invalid

interpJudgement :: Judgement -> Either Invalid (Judgement, [(String, PropertyValue)])
interpJudgement (j @ (Judgement (h, prop, cs, js))) = do
  updlist <- mapM interpJudgement js
  let (jupdates, jsPropVals) = unzip updlist
  predef <- generatePredefinedValues h
  propVals <- mapM (bindProp j (predef:jsPropVals)) (addPredifinedProps prop)
  let newProps = map propValToProperties propVals
  pure (Judgement (h, newProps, cs, jupdates), propVals)
interpJudgement (Bonus (v, _, c)) = pure (Bonus (v, preProps, c), newProps)
  where
    newProps = [("Title", StrVal "Bonus"), ("Total", DoubVal v)]
    preProps = [Property ("Title", Value "Bonus"), Property ("Total", Num v)]

propValToProperties :: (String, PropertyValue) -> Property
propValToProperties (str, StrVal string) = Property (str, Value string)
propValToProperties (str, DoubVal double) = Property (str, Num double)

addPredifinedProps :: [Property] -> [Property]
addPredifinedProps p =
  Property("Title", Lookup (0, "Title")) :
  Property("Total", Sum "Total") :
  Property("MaxPoints", Sum "MaxPoints") : p

generatePredefinedValues :: Header -> Either Invalid [(String, PropertyValue)]
generatePredefinedValues (Header (t, Nothing, maxP)) =
  pure [("Title", StrVal t), ("Total", DoubVal 0), ("MaxPoints", DoubVal maxP)]
generatePredefinedValues (Header (t, (Just p), maxP)) =
  pure [("Title", StrVal t), ("Total", DoubVal p), ("MaxPoints", DoubVal maxP)]

bindProp :: Judgement -> [[(String, PropertyValue)]] -> Property ->
  Either Invalid (String, PropertyValue)
bindProp rj propEnv (Property (name, propExp)) =
  case (evalPropExp rj propExp propEnv) of
    Right (val)  -> pure (name, val)
    Left invalid -> Left invalid

evalPropExp :: Judgement -> PropertyExp -> [[(String, PropertyValue)]] ->
  Either Invalid PropertyValue
evalPropExp _ (Value s) _ = pure $ StrVal s
evalPropExp _ (Num n) _ = pure $ DoubVal n
evalPropExp rj (Lookup (i, p)) propEnv =
  case (lookup p (propEnv !! (i))) of
    Nothing -> Left $ PropertyNotFound p rj
    (Just s) -> pure s
evalPropExp rj (Sum pname) propEnv | isLeafJ rj =
    evalPropExp rj (Lookup (0, pname)) propEnv
evalPropExp _ (Sum pname) propEnv =
    pure $ sumPV vals
  where
    vals = map snd $ concatMap (filter (\x -> (fst x) == pname)) (tail propEnv)

sumPV :: [PropertyValue] -> PropertyValue
sumPV [] = DoubVal 0
sumPV ((DoubVal v):vals) =
  case sumPV vals of
    DoubVal vs -> DoubVal $ vs + v
    StrVal _ -> error "I cannot sum a string. Please report this error!"
sumPV _ = error "I cannot sum a string. Please report this error!"
