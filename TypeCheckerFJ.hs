module TypeCheckerFJ where

import UtilitiesFJ;;
import DataTypesFJ;;

-- check type of an expression
typeCheck :: Exp -> ClassTable -> Context -> Either Exception Type

-- T-Var
typeCheck (Variable x) cTable context =
  case lookupContext x context of
    Nothing -> Left (VariableNotFoundException ("Variable " ++ x ++ " not found!"))
    Just t  -> Right t
;;

-- T-Invk
typeCheck (MethodInv e0 mname params) cTable context =
  let c0 = typeCheck e0 cTable context in
    case c0 of
      Left e               -> Left e
      Right (TypeDecl c0') ->
        case tryGetClass c0' cTable of
          Nothing -> Left (ClassNotFoundException ("Class " ++ c0' ++ " not found!"))
          Just c' ->
            let types = mtype c' mname cTable in
              case types of
                Left e'                 -> Left e'
                Right (types', retType) ->
                  case checkTypeAndNumberOfParams types' params cTable context of
                    Left e'' -> Left e''
                    Right _  -> Right retType
;;

-- T-Field
typeCheck (FieldAccess e f) cTable context =
  let t = typeCheck e cTable context in
    case t of
      Left e              -> Left e
      Right (TypeDecl t') ->
        let c = tryGetClass t' cTable in
          case c of
            Nothing -> Left (ClassNotFoundException ("Class " ++ t' ++ " not found!"))
            Just c' ->
              let fds = fields c' cTable in
              let idx = isInFields f fds 0 in
                case idx of
                  Nothing   -> Left (FieldNotFoundException ("Field " ++ f ++ " not found!"))
                  Just idx' -> Right (fst (fds !! idx'))
;;

-- T-New
typeCheck (New cname params) cTable context =
  let c = tryGetClass cname cTable in
    case c of
      Nothing -> Left (ClassNotFoundException ("Class " ++ cname ++ " not found!"))
      Just c' ->
        let fds = map (fst) (fields c' cTable) in
          case checkTypeAndNumberOfParams fds params cTable context of
            Left e  -> Left e
            Right _ -> Right (TypeDecl cname)
;;

-- T-Cast
typeCheck (Cast cname exp) cTable context =
  let t = typeCheck exp cTable context in
    case t of
      Left e   -> Left e
      Right t' ->
        let tt = fromType t' in
        case tryGetClass tt cTable of
          Nothing -> Left (ClassNotFoundException ("Class " ++ tt ++ " not found!"))
          Just c  ->
            case tryGetClass cname cTable of
              Nothing -> Left (ClassNotFoundException ("Class " ++ tt ++ " not found!"))
              Just c' ->
                -- T-UpCast
                case subtype c c' cTable of
                  Nothing -> Left (TypeException "Types are not well formed!")
                  Just b  ->
                    if b
                    then Right (TypeDecl cname)
                    else
                      -- T-DownCast
                      case subtype c' c cTable of
                        Nothing -> Left (TypeException "Types are not well formed!")
                        Just b' ->
                          if b'
                          then Right (TypeDecl cname)
                          else Left (TypeException "Types are not compatible!")
;;

-- check if a method is well-formed
typeCheckMethod :: Method -> Class -> ClassTable -> Either Exception ()
typeCheckMethod (MethodDecl retType mName params exp) c cTable =
  let context = (map (\(x, y) -> (y, x)) params) ++ [("this", TypeDecl (className c))] in
  if allDifferent (map (fst) context)
  then
    case typeCheck exp cTable context of
      Left e  -> Left e
      Right t ->
        let tt = fromType t in
        case tryGetClass tt cTable of
          Nothing -> Left (ClassNotFoundException ("Type " ++ tt ++ " not found!"))
          Just r' ->
            let ttt = fromType retType in
            case tryGetClass ttt cTable of
              Nothing       -> Left (ClassNotFoundException ("Type " ++ ttt ++ " not found!"))
              Just retType' -> 
                case subtype r' retType' cTable of
                  Nothing -> Left (TypeException "Types are not well formed!")
                  Just b  ->
                    if b
                    then Right ()
                    else Left (TypeException "Effective and declared return types are not compatible!")
  else Left (DuplicateVariableException ("There are two params with the same name in method " ++ mName ++ "!"))
;;

-- check all methods in a class
checkAllMethods :: MDS -> Class -> ClassTable -> Either Exception ()
checkAllMethods [] _ _           = Right ();;
checkAllMethods (m:mds) c cTable =
  case typeCheckMethod m c cTable of
    Left e -> Left e
    _      -> checkAllMethods mds c cTable
;;

-- check if a class is well-formed
typeCheckClass :: Class -> ClassTable -> Either Exception ()
typeCheckClass c cTable =
  let cname = className c in
  let fname = classFather c in
  if checkClassName cname cTable
  then
    case tryGetClass fname cTable of
      Nothing -> Left (ClassNotFoundException ("Class " ++ fname ++ " does not exist!"))
      _       ->
        if goToObj c cTable [] then
          let fds = fields c cTable in
            if allDifferent (map (snd) fds)
            then
              case checkFieldsType fds cTable of
                Left ex -> Left ex
                _ ->
                  let mds = methods c cTable in
                    if allDifferent (map (methodName) (classMethods c)) -- overrided allowed
                    then
                      if checkOverride mds
                      then
                        case checkAllMethods mds c cTable of
                          Left e -> Left e
                          _      -> Right ()
                      else Left (OverrideException "Methods with same name do not share same signature!")
                    else Left (DuplicateMethodException ("There are two methods with the same name in class " ++ cname ++ "!"))
            else Left (DuplicateFieldException ("There are two fields with the same name in class " ++ cname ++ "!"))
        else Left (TypeException "Cyclic dependency between two types!")
  else Left (DuplicateClassException ("There are two classes with the same name: " ++ cname ++ "!"))
;;

-- typecheck the entire program
typeCheckProg :: ClassTable -> ClassTable -> Either Exception ()
typeCheckProg [] _          = Right ();;
typeCheckProg (c:cs) cTable =
  case typeCheckClass c cTable of
    Left e -> Left e
    _      -> typeCheckProg cs cTable
;;

-- check the number and the type of parameter of a function (also contructor) invocation
checkTypeAndNumberOfParams :: [Type] -> [Exp] -> ClassTable -> Context -> Either Exception Bool
checkTypeAndNumberOfParams [] [] _ _     = Right True;;
checkTypeAndNumberOfParams ts [] _ _     = Left (MismatchParamsException "Params does not match!");;
checkTypeAndNumberOfParams [] params _ _ = Left (MismatchParamsException "Params does not match!");;
checkTypeAndNumberOfParams (tf:ts) (p:params) cTable context =
  let tf' = fromType tf in
  case tryGetClass tf' cTable of
    Nothing -> Left (ClassNotFoundException ("Class " ++ tf' ++ " not found!"))
    Just c' ->
      case typeCheck p cTable context of
        Left e   -> Left e
        Right p' ->
          let tp = fromType p' in
          case tryGetClass tp cTable of
            Nothing -> Left (ClassNotFoundException ("Class " ++ tp ++ " not found!"))
            Just c  ->
              case subtype c c' cTable of
                Nothing -> Left (MismatchParamsException "Type of params does not match!")
                Just b  ->
                  if b
                  then checkTypeAndNumberOfParams ts params cTable context
                  else Left (MismatchParamsException "Type of params does not match!")
;;
