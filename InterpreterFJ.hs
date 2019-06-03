module InterpreterFJ where

import MonadicParserFJ;;
import TypeCheckerFJ;;
import UtilitiesFJ;;
import DataTypesFJ;;

-- reduce expressions in a context
reduce :: Exp -> ClassTable -> Either RuntimeException (Maybe Exp)

-- Val
reduce (New _ params) _ | allValues params = Right Nothing;;

-- New
reduce (New cname params) cTable =
  let onlyVals = filter (\x -> isVal x == False) params in
  let fstE     = head onlyVals in
  let others   = tail onlyVals in
    case reduce fstE cTable of
      Left ex  -> Left ex
      Right me ->
        case me of
          Nothing -> Right Nothing
          Just e' -> Right (Just (New cname (e':others)))
;;

-- FieldNew
reduce (FieldAccess v f) cTable
  | isVal v =
      let f_i = isInFields f (fields (getClass (newClassName v) cTable) cTable) 0 in
        case f_i of
          Nothing -> Right Nothing
          Just f' -> Right (Just ((newParams v) !! f'))
;;

-- Field
reduce (FieldAccess e f) cTable =
  case reduce e cTable of
    Left ex  -> Left ex
    Right me ->
      case me of
        Nothing -> Right Nothing
        Just e' -> Right (Just (FieldAccess e' f))
;;

-- CastNew
reduce (Cast cl v) cTable
  | isVal v =
    let c  = (getClass (newClassName v) cTable) in
    let c' = (getClass cl cTable) in
      case subtype c c' cTable of
        Nothing -> Right Nothing
        Just st ->
          if st
          then Right (Just v)
          else Left (ClassCastException "Cast not valid!")--Nothing
;;

-- Cast
reduce (Cast c e) cTable =
  case reduce e cTable of
    Left ex  -> Left ex
    Right me ->
      case me of
        Nothing -> Right Nothing
        Just e' -> Right (Just (Cast c e'))
;;

-- InvkNew
-- substitute params with their values and "this" with current class value
reduce (MethodInv v m params) cTable
  | isVal v && allValues params =
    let t  = typeCheck v cTable [] in
      case t of
        Left e   -> Left (RuntimeException "Dynamic type error !")
        Right t' ->
          let mb = mbody (getClass (newClassName v) cTable) m t' cTable in
            case mb of
              Nothing      -> Right Nothing
              Just (xs, e) -> Right (subst e (params ++ [v]) (xs ++ ["this"]))
;;

-- InvkArg
reduce (MethodInv v m params) cTable
  | isVal v =
    let vals     = filter (\x -> isVal x) params in
    let notVals  = filter (\x -> isVal x == False) params in
    let fstE     = head notVals in
    let others   = tail notVals in
      case reduce fstE cTable of
        Left ex  -> Left ex
        Right me ->
          case me of
            Nothing -> Right Nothing
            Just e' -> Right (Just (MethodInv v m (vals ++ (e':others))))
;;

-- InvkRcv
reduce (MethodInv e m params) cTable =
  case reduce e cTable of
    Left ex  -> Left ex
    Right me ->
      case me of
        Nothing -> Right Nothing
        Just e' -> Right (Just (MethodInv e' m params))
;;

-- substitution of a term
subst :: Exp -> [Exp] -> [String] -> Maybe Exp
subst (Variable x) params xs =
    case isInParams x xs 0 of
      Just x' -> Just (params !! x')
      Nothing -> Nothing
;;
subst (FieldAccess e f) params xs =
  case subst e params xs of
    Nothing -> Nothing
    Just e' -> Just (FieldAccess e' f)
;;
subst (MethodInv e m ps) params xs =
  -- Nothing shouldn't be received due to typechecking
  let ps' = map (\x ->
                   case subst x params xs of
                     Just x' -> x') ps
  in
    case subst e params xs of
      Nothing -> Nothing
      Just e' -> Just (MethodInv e' m ps')
;;
subst (New c ps) params xs =
  -- Nothing shouldn't be received due to typechecking
  let ps' = map (\x ->
                   case subst x params xs of
                     Just x' -> x') ps
  in
    Just (New c ps')
;;
subst (Cast c e) params xs =
  case subst e params xs of
    Nothing -> Nothing
    Just e' -> Just (Cast c e')
;;

-- -*>
reduceStar :: Exp -> ClassTable -> Either RuntimeException Exp
reduceStar e cTable =
  case reduce e cTable of
    Left ex  -> Left ex
    Right me ->
      case me of
        Just e' -> reduceStar e' cTable
        _       -> Right e
;; 
