{-# LANGUAGE DeriveGeneric #-}

module PropertyInterp ( interpProps ) where

import Ast
import Invalid

import Text.PrettyPrint.GenericPretty

-- In the lists index 0 is used for predifined variables: Title, Total, MaxPoints
-- This should refactored to real monadic programming

data PropertyValue
  = StrVal String
  | DoubVal Double
  deriving (Eq, Show, Generic)

instance Out PropertyValue

interpProps :: Judgement -> Either Invalid Judgement
interpProps j =
  case (interpJudgement j) of
    Right (jnew, _) -> Right jnew
    Left invalid -> Left invalid

interpJudgement :: Judgement -> Either Invalid (Judgement, [(String, PropertyValue)])
interpJudgement (Judgement (h, prop, cs, js)) = do
  updlist <- mapM interpJudgement js
  let (jupdates, jsPropVals) = unzip updlist
  predef <- generatePredefinedValues h
  propVals <- mapM (bindProp (predef:jsPropVals)) (addPredifinedProps prop)
  let newProps = map propValToProperties propVals
  pure (Judgement (h, newProps, cs, jupdates), propVals)
interpJudgement j @ (Bonus (v, _)) = pure (j, [("Bonus", DoubVal v)])

propValToProperties :: (String, PropertyValue) -> Property
propValToProperties (str, StrVal string) = Property (str, Value string)
propValToProperties (str, DoubVal double) = Property (str, Num double)

addPredifinedProps :: [Property] -> [Property]
addPredifinedProps p =
  Property("Title", Lookup (0, "Title")) :
  Property("Total", Lookup (0, "Total")) :
  Property("MaxPoints", Lookup (0, "MaxPoints")) : p

generatePredefinedValues :: Header -> Either Invalid [(String, PropertyValue)]
generatePredefinedValues (Header (t, p, maxP)) =
  pure [("Title", StrVal t), ("Total", DoubVal p), ("MaxPoints", DoubVal maxP)]

bindProp :: [[(String, PropertyValue)]] -> Property ->
  Either Invalid (String, PropertyValue)
bindProp propEnv (Property (name, propExp)) =
  case (evalPropExp propExp propEnv) of
    Right (val)  -> pure (name, val)
    Left invalid -> Left invalid

evalPropExp :: PropertyExp -> [[(String, PropertyValue)]] ->
  Either Invalid PropertyValue
evalPropExp (Value s) _ = pure $ StrVal s
evalPropExp (Num n) _ = pure $ DoubVal n
evalPropExp (Lookup (i, p)) propEnv =
  case (lookup p (propEnv !! (i))) of
    Nothing -> Left $ PropertyNotFound p
    (Just s) -> pure s
