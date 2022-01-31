{-# LANGUAGE DeriveGeneric #-}

module PropertyInterp ( interpProps ) where

import Ast
import Invalid

import Text.PrettyPrint.GenericPretty()
import Control.Monad (liftM)

-- In the lists index 0 is used for predifined variables: Title, Total, MaxPoints
-- This should refactored to real monadic programming

-- data PropertyValue
--   = ListVal [String]
--   | StrVal   String
--   | IntVal   Int
--   deriving (Eq, Show, Generic)

-- instance Out PropertyValue

interpProps :: Judgement -> Either Invalid Judgement
interpProps j =
  case (interpJudgement j) of
    Right (jnew, _) -> Right jnew
    Left   invalid  -> Left invalid

interpJudgement :: Judgement -> Either Invalid (Judgement, [(String, PropertyValue)])
interpJudgement (j@(Judgement (h, prop, cs, []))) = do
  -- updlist <- mapM interpJudgement js
  -- let (jupdates, jsPropVals) = unzip updlist
  predef <- generatePredefinedValues h
  propVals <- bindProp j [predef] (addPredifinedPropsLeaf prop)
  let newProps = map propValToProperties propVals
  pure (Judgement (h, newProps, cs, []), propVals)
interpJudgement (j@(Judgement (h, prop, cs, js))) = do
  updlist <- mapM interpJudgement js
  let (jupdates, jsPropVals) = unzip updlist
  predef <- generatePredefinedValues h
  propVals <- bindProp j (predef:jsPropVals) (addPredifinedProps prop (length js))
  let newProps = map propValToProperties propVals
  pure (Judgement (h, newProps, cs, jupdates), propVals)
interpJudgement (Bonus (v, prop, c)) = pure (Bonus (v, preProps, c), newProps)
  where
    newProps = [("Title", StrVal "Bonus"), ("Total", IntVal v), ("MaxPoints", IntVal 0), ("MaxPointsGiven", IntVal 0), ("MadeTasks", IntVal 0), ("TotalTasks", IntVal 0)]
    preProps = [Property ("Title", Value "Bonus"), Property ("Total", Num v), Property ("MadeTasks", Num 0), Property ("TotalTasks", Num 0)] ++ prop
interpJudgement (Feedback (prop, t)) = pure (Feedback (preProps, t), newProps)
  where
    newProps = [("Title", StrVal "Feedback"), ("Total", IntVal 0), ("MaxPoints", IntVal 0), ("MaxPointsGiven", IntVal 0), ("MadeTasks", IntVal 0), ("TotalTasks", IntVal 0)]
    preProps = [Property ("Title", Value "Feedback"), Property ("Total", Num 0), Property ("MadeTasks", Num 0), Property ("TotalTasks", Num 0)] ++ prop

propValToProperties :: (String, PropertyValue) -> Property
propValToProperties (str, ListVal string) = Property (str, List  string)
propValToProperties (str, StrVal  string) = Property (str, Value string)
propValToProperties (str, IntVal  double) = Property (str, Num   double)

addPredifinedPropsLeaf :: [Property] -> [Property]
addPredifinedPropsLeaf p =
  Property("Title", Lookup (0,Value "Title")) :
  Property("Total", ArithFun Sum [Lookup (0,Value "Total")]) :
  Property("MaxPoints", ArithFun Sum [Lookup (0,Value "MaxPoints")]) :
  Property("MaxPointsGiven", ArithFun Sum [Lookup (0,Value "MaxPointsGiven")]) :
  Property("MadeTasks", ArithFun Sum [Lookup (0,Value "MadeTasks")]) :
  Property("TotalTasks", ArithFun Sum [Lookup (0,Value "TotalTasks")]) : p

addPredifinedProps :: [Property] -> Int -> [Property]
addPredifinedProps p sublen =
  Property("Title", Lookup (0, Value "Title")) :
  Property("Total", ArithFun Sum (map (\x -> Lookup (x,Value "Total")) [1 .. sublen])) :
  Property("MaxPoints", ArithFun Sum (map (\x -> Lookup (x,Value "MaxPoints")) [1 .. sublen])) :
  Property("MaxPointsGiven", ArithFun Sum (map (\x -> Lookup (x,Value "MaxPointsGiven")) [1 .. sublen])) :
  Property("MadeTasks", ArithFun Sum (map (\x -> Lookup (x,Value "MadeTasks")) [1 .. sublen])) :
  Property("TotalTasks", ArithFun Sum (map (\x -> Lookup (x,Value "TotalTasks")) [1 .. sublen])) : p

generatePredefinedValues :: Header -> Either Invalid [(String, PropertyValue)]
generatePredefinedValues (Header (t, (Given p), maxP)) =
  pure [("Title", StrVal t), ("Total", IntVal p), ("MaxPoints", IntVal maxP), ("MaxPointsGiven", IntVal maxP), ("MadeTasks", IntVal 100), ("TotalTasks", IntVal 100)]
generatePredefinedValues (Header (t, NotMade, maxP)) =
  pure [("Title", StrVal t), ("Total", IntVal 0), ("MaxPoints", IntVal maxP), ("MaxPointsGiven", IntVal maxP), ("MadeTasks", IntVal 0), ("TotalTasks", IntVal 100)]
generatePredefinedValues (Header (t, NotGiven, maxP)) =
  pure [("Title", StrVal t), ("Total", IntVal 0), ("MaxPoints", IntVal maxP), ("MaxPointsGiven", IntVal 0), ("MadeTasks", IntVal 0), ("TotalTasks", IntVal 100)]

bindProp :: Judgement -> [[(String, PropertyValue)]] -> [Property] ->
  Either Invalid [(String, PropertyValue)]
bindProp _ propEnv [] = pure $ head propEnv
bindProp rj propEnv ((Property (name, propExp)):ps) =
  case (evalPropExp rj propExp propEnv) of
    Right (val)  -> do
      vals <- bindProp rj (updPropEnv name val propEnv) ps
      pure $ vals
    Left invalid -> Left invalid
  where
    updPropEnv _ _ [] = []
    updPropEnv n v (this:rest) = ((n, v):(filter (\x -> (fst x) /= n) this)):rest
    -- updlist v [] = [(name,v)]
    -- updlist _ ((n,v):rest) | n == name = (name,v):rest
    -- updlist v (e:rest) = e:(updlist v rest)

evalPropExp :: Judgement -> PropertyExp -> [[(String, PropertyValue)]] ->
  Either Invalid PropertyValue
evalPropExp _ (List s)  _ = pure $ ListVal s
evalPropExp _ (Value s) _ = pure $ StrVal s
evalPropExp _ (Num n)   _ = pure $ IntVal n
evalPropExp rj l@(Lookup (i, Value p)) propEnv =
  case (lookup p (propEnv !! (i))) of
    Nothing -> Left $ PropertyNotFound l rj propEnv
    (Just s) -> pure s
evalPropExp _ (Lookup (_, _)) _ = undefined
evalPropExp rj (ArithFun _ [pname]) propEnv | isLeafJ rj =
  evalPropExp rj pname propEnv
evalPropExp rj (ArithFun fun pname) propEnv =
  case sequence $ map (\x -> evalPropExp rj x propEnv) pname of
    Left e -> Left e
    Right list ->
      case appFun fun list of
        Nothing -> Left $ StringInputToArithFun (show pname) rj propEnv
        Just pv -> Right $ pv

appFun :: PropertyArithFun -> [PropertyValue] -> Maybe PropertyValue
appFun Sum      = appIntFun sum
appFun Prod     = appIntFun (foldl (\x y -> div (x * y) 100) 100)
appFun Div      = appIntFun (foldr (\x y -> div (x * 100) y) 100)
appFun Min      = appIntFun minimum
appFun Max      = appIntFun maximum
appFun PointMap = appIntFun pointMap
appFun Map      = gradeMap
appFun If       = gradeCond

pointMap :: [Int] -> Int
pointMap [] = 0
pointMap (p:ps) = sum $ map (\x -> if p>=x then 100 else 0) ps

gradeCond :: [PropertyValue] -> Maybe PropertyValue
gradeCond [] = Nothing
gradeCond [_] = Nothing
gradeCond [_,_] = Nothing
gradeCond (c:e1:e2:[]) =
  case propToInt c of
    Nothing  -> Just e2
    (Just 0) -> Just e2
    (Just _) -> Just e1
gradeCond _ = Nothing

gradeMap :: [PropertyValue] -> Maybe PropertyValue
gradeMap [] = Nothing
gradeMap [_] = Nothing
gradeMap (p:ps) =
  case propToInt p of
    Nothing  -> Nothing
    (Just v) -> Just $ ps !! (div v 100)

-- getVals :: String -> [[(String, PropertyValue)]] -> [PropertyValue]
-- getVals pname propEnv = map snd $ concatMap (filter (\x -> (fst x) == pname)) (tail propEnv)

appIntFun :: ([Int] -> Int) -> [PropertyValue] -> Maybe PropertyValue
appIntFun fun ps = liftM (IntVal . fun) $ sequence $ map propToInt ps

propToInt :: PropertyValue -> Maybe Int
propToInt (IntVal  v) = Just v
propToInt (StrVal  _) = Nothing
propToInt (ListVal _) = Nothing
