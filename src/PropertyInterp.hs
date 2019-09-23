{-# LANGUAGE DeriveGeneric #-}

module PropertyInterp ( interpProps ) where

import Ast
import Invalid

import Text.PrettyPrint.GenericPretty
import Control.Monad (liftM)

-- In the lists index 0 is used for predifined variables: Title, Total, MaxPoints
-- This should refactored to real monadic programming

data PropertyValue
  = ListVal [String]
  | StrVal   String
  | IntVal   Int
  deriving (Eq, Show, Generic)

instance Out PropertyValue

interpProps :: Judgement -> Either Invalid Judgement
interpProps j =
  case (interpJudgement j) of
    Right (jnew, _) -> Right jnew
    Left   invalid  -> Left invalid

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
    newProps = [("Title", StrVal "Bonus"), ("Total", IntVal v)]
    preProps = [Property ("Title", Value "Bonus"), Property ("Total", Num v)]

propValToProperties :: (String, PropertyValue) -> Property
propValToProperties (str, ListVal string) = Property (str, List  string)
propValToProperties (str, StrVal  string) = Property (str, Value string)
propValToProperties (str, IntVal  double) = Property (str, Num   double)

addPredifinedProps :: [Property] -> [Property]
addPredifinedProps p =
  Property("Title", Lookup (0, "Title")) :
  Property("Total", ArithFun Sum "Total") :
  Property("MaxPoints", ArithFun Sum "MaxPoints") : p

generatePredefinedValues :: Header -> Either Invalid [(String, PropertyValue)]
generatePredefinedValues (Header (t, (Given p), maxP)) =
  pure [("Title", StrVal t), ("Total", IntVal p), ("MaxPoints", IntVal maxP)]
generatePredefinedValues (Header (t, _, maxP)) =
  pure [("Title", StrVal t), ("Total", IntVal 0), ("MaxPoints", IntVal maxP)]

bindProp :: Judgement -> [[(String, PropertyValue)]] -> Property ->
  Either Invalid (String, PropertyValue)
bindProp rj propEnv (Property (name, propExp)) =
  case (evalPropExp rj propExp propEnv) of
    Right (val)  -> pure (name, val)
    Left invalid -> Left invalid

evalPropExp :: Judgement -> PropertyExp -> [[(String, PropertyValue)]] ->
  Either Invalid PropertyValue
evalPropExp _ (List s)  _ = pure $ ListVal s
evalPropExp _ (Value s) _ = pure $ StrVal s
evalPropExp _ (Num n)   _ = pure $ IntVal n
evalPropExp rj (Lookup (i, p)) propEnv =
  case (lookup p (propEnv !! (i))) of
    Nothing -> Left $ PropertyNotFound p rj
    (Just s) -> pure s
evalPropExp rj (ArithFun _ pname) propEnv | isLeafJ rj =
  evalPropExp rj (Lookup (0, pname)) propEnv
evalPropExp rj (ArithFun fun pname) propEnv =
  case appFun (arithListFun fun) $ getVals pname propEnv of
    Nothing -> Left $ StringInputToArithFun pname rj
    Just pv -> Right $ pv

arithListFun :: PropertyArithFun -> [Int] -> Int
arithListFun Sum = sum
arithListFun Min = minimum
arithListFun Max = maximum

getVals :: String -> [[(String, PropertyValue)]] -> [PropertyValue]
getVals pname propEnv = map snd $ concatMap (filter (\x -> (fst x) == pname)) (tail propEnv)

appFun :: ([Int] -> Int) -> [PropertyValue] -> Maybe PropertyValue
appFun fun ps = liftM (IntVal . fun) $ sequence $ map propToInt ps

propToInt :: PropertyValue -> Maybe Int
propToInt (IntVal  v) = Just v
propToInt (StrVal  _) = Nothing
propToInt (ListVal _) = Nothing
