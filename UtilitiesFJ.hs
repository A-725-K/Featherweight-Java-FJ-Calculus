module UtilitiesFJ where

import DataTypesFJ;;
import Data.Maybe;;
import Data.List;;

-- for test inside the program

-------------------------------------------------------------------------------------------
-- class Object extends "" {}
o = ClassDecl "Object" ""
    []
    []
;;

-- class A extends B {
--   Object oa;
-- }
a = ClassDecl "A" "B"
    [
      (TypeDecl "Object", "oa")
    ]
    []
;;

-- class B extends Object {
--   Object o1;
--   Object o2;
--
--   Object first()  { return this.o1; }
--   Object second() { return this.o2; }
--   B freshB(Object fst, Object snd) { return new B(fst, snd); }
-- }
b = ClassDecl "B" "Object"
    [
      (TypeDecl "Object", "o1"),
      (TypeDecl "Object", "o2")
    ]
    [
      (MethodDecl (TypeDecl "Object") "first" [] (FieldAccess (Variable "this") "o1")),
      (MethodDecl (TypeDecl "Object") "second" [] (FieldAccess (Variable "this") "o2")),
      (MethodDecl (TypeDecl "B") "freshB" [(TypeDecl "Object", "fst"), (TypeDecl "Object", "snd")] (New "B" [Variable "fst", Variable "snd"]))
    ]
;;

-- class C extends Object {
--   B b;
--
--   C changeB(B newB) { return new C(newB); }
-- }
c = ClassDecl "C" "Object"
    [
      (TypeDecl "B", "b")
    ]
    [
      (MethodDecl (TypeDecl "C") "changeB" [(TypeDecl "B", "newB")] (New "C" [(Variable "newB")]))
    ]
;;

-- class D extends Object {
--   Object x;
-- }
d = ClassDecl "D" "Object"
    [
      (TypeDecl "Object", "x")
    ]
    []
;;

-- class E extends D {}
e = ClassDecl "E" "D" [] [];;
-- class F extends Object {}
f = ClassDecl "F" "Object" [] [];;
-- class G extends Object {}
g = ClassDecl "G" "Object" [] [];;


-- to test dynamic binding
  
-- class R extends Object {
--    Object m () { return new R(); }
-- }
r = ClassDecl "R" "Object"
    []
    [(MethodDecl (TypeDecl "Object") "m" [] (New "R" []))]
;;
-- class S extends R {
--    Object m (Object x) { return new S(); }
--    Object q () { return new R(); }
-- }
s = ClassDecl "S" "R"
    []
    [
      (MethodDecl (TypeDecl "Object") "m" [{-(TypeDecl "Object", "x")-}] (New "S" [])),
      (MethodDecl (TypeDecl "Object") "q" [] (New "R" []))
    ]
;;
-- class T extends S {
--    Object m () { return new T(); }
--    Object q () { return new Object(); }
-- }
t = ClassDecl "T" "S"
    []
    [
      (MethodDecl (TypeDecl "Object") "m" [] (New "T" [])),
      (MethodDecl (TypeDecl "Object") "q" [] (New "Object" []))
    ]
;;
-- class RPair extends Object {
--    R first;
--    R second;
-- }
rp = ClassDecl "RPair" "Object"
    [
      (TypeDecl "R", "first"),
      (TypeDecl "R", "second")
    ]
    []
;;

-- class tables
ct0 =
  [
    o, r, s, t, rp
  ]
;;

ct1 =
  [
    o, b, c, a, d, e, f, g
  ]
;;

-------------------------------------------------------------------------------------------

-- utilities
className    (ClassDecl cname _ _ _)   = cname;;
classFather  (ClassDecl _ father _ _)  = father;;
classFields  (ClassDecl _ _ fields _)  = fields;;
classMethods (ClassDecl _ _ _ methods) = methods;;

methodName   (MethodDecl _ mname _ _)  = mname;;
methodParams (MethodDecl _ _ params _) = methodParamsAux params;;
methodExp    (MethodDecl _ _ _ exp)    = exp;;

newClassName (New cname _)             = cname;;
newParams    (New _ params)            = params;;

-- returns only name of the parameters without types
methodParamsAux :: [(Type, String)] -> [String]
methodParamsAux []            = [];;
methodParamsAux ((_, mn):mps) = [mn] ++ methodParamsAux mps;;

-- check if a list of parameters contains only values
allValues :: [Exp] -> Bool
allValues p = foldl (\a x -> a && isVal x ) True p;;

-- return a class that match a name
lookupClasses :: String -> ClassTable -> Maybe Class
lookupClasses _ []         = Nothing;;
lookupClasses cname (c:cs) =
  if cname == (className c)
  then Just c
  else lookupClasses cname cs
;;

-- return a method in a class that match a name
lookupMethods :: String -> MDS -> Maybe Method
lookupMethods _ []         = Nothing;;
lookupMethods mname (m:ms) =
  if mname == (methodName m)
  then Just m
  else lookupMethods mname ms
;;
-- alternative version: return a method of a specific class
lookupMethods' :: String -> Type -> [(Method, Type)] -> Maybe Method
lookupMethods' _ _ []                  = Nothing;;
lookupMethods' mname ctype ((m, t):ms) =
  if (mname == (methodName m)) && (ctype == t)
  then Just m
  else lookupMethods' mname ctype ms
;;

-- to simplify
tryGetClass :: String -> ClassTable -> Maybe Class
tryGetClass cname cTable = lookupClasses cname cTable;;

-- N.B. NOTHING couldn't be returned when used lookupClasses in this function due to typechecking
getClass :: String -> ClassTable -> Class
getClass cname cTable = fromJust (lookupClasses cname cTable);;

-- fields
fields :: Class -> ClassTable -> FDS
fields c cTable =
  let cf = classFather c in
    case cf of
      "" -> []
      cf' -> (fields (getClass cf' cTable) cTable) ++ (classFields c)
;;

-- given a list of method returns also the class in which is
appendClass :: String -> [Method] -> [(Method, Type)]
appendClass cname mds = map (\m -> (m, TypeDecl cname)) mds;;

-- methods
methods :: Class -> ClassTable -> MDS
methods c cTable =
  let cf = classFather c in
    case cf of
      ""  -> []
      cf' -> (classMethods c) ++ (methods (getClass cf' cTable) cTable)
;;
-- alternative version: returns also the class in which the method is found
methods' :: Class -> ClassTable -> [(Method, Type)]
methods' c cTable =
  let cf = classFather c in
    case cf of
      ""  -> []
      cf' -> (appendClass (className c) (classMethods c)) ++ (methods' (getClass cf' cTable) cTable)
;;

-- mbody
mbody :: Class -> String -> Type -> ClassTable -> Maybe ([String], Exp)
mbody c m t cTable =
  let ms = methods' c cTable in
  let mb = lookupMethods' m t ms in
    case mb of
      Nothing  ->
        let mmss = methods c cTable in
        let mmbb = lookupMethods m mmss in
          case mmbb of
            Nothing    -> Nothing
            Just mmbb' -> Just (methodParams mmbb', methodExp mmbb')
      Just mb' -> Just (methodParams mb', methodExp mb')
;;

-- mtype
mtype :: Class -> String -> ClassTable -> Either Exception ([Type], Type)
mtype c m cTable =
  case checkMethods m (methods c cTable) of
    Nothing -> Left (MethodNotFoundException ("Method " ++ m ++ " not found!"))
    Just mt -> Right mt
;;

-- check if a method is present in the list of methods
checkMethods :: String -> [Method] -> Maybe ([Type], Type)
checkMethods _ []                                        = Nothing;;
checkMethods m ((MethodDecl retType mname params e):mds) =
  if m == mname
  then Just (fst (unzip params), retType)
  else checkMethods m mds
;;

-- check if a field is in the fields of a class
isInFields :: String -> FDS -> Int -> Maybe Int
isInFields _ [] _                = Nothing;;
isInFields fname ((_, f):fs) acc =
  if fname == f
  then Just acc
  else isInFields fname fs (acc+1)
;;

-- check if a parameter is in the list of parameters in method
isInParams :: String -> [String] -> Int -> Maybe Int
isInParams _ [] _           = Nothing;;
isInParams pname (p:ps) acc =
  if pname == p
  then Just acc
  else isInParams pname ps (acc+1)
;;

-- subtypying relation
goToObj :: Class -> ClassTable -> ClassTable -> Bool
goToObj (ClassDecl "Object" "" _ _) _ _ = True;;
goToObj c cTable acc =
  let cf = getClass (classFather c) cTable in
    if elem c acc
    then False
    else goToObj cf cTable (c:acc)
;;

-- check if a class is a subtype of another class (fail in case of cyclic subtyping)
subtype :: Class -> Class -> ClassTable -> Maybe Bool
subtype (ClassDecl "Object" "" [] []) (ClassDecl "Object" "" [] []) _ = Just True;;
subtype (ClassDecl "Object" "" [] []) _ _                             = Just False;;
subtype c1 c2 cTable =
  let cf  = classFather c1 in
  let gto1 = goToObj c1 cTable [] in
  let gto2 = goToObj c2 cTable [] in
   if gto1 && gto2
   then
     Just ((className c1 == className c2) ||
           (cf == (className c2)) ||
           case lookupClasses cf cTable of
             Nothing -> False
             Just c' ->
               case subtype c' c2 cTable of
                 Just x -> x
                 Nothing -> False)
   else Nothing
;;

-- check if an expression is a value
isVal :: Exp -> Bool
isVal (New _ []) = True;;
isVal (New _ l) = allValues l;;
isVal _         = False;;

-- get the name of a type
fromType :: Type -> String
fromType (TypeDecl t) = t;;

-- check if a list does not contain duplicates
allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True;;
allDifferent (x:xs) =
  (not (elem x xs)) && (allDifferent xs)
;;

-- check if there are two classes with the same name
checkClassName    :: String -> ClassTable -> Bool
checkClassNameAux :: String -> ClassTable -> Int -> Bool
checkClassNameAux _ [] i         = (i == 1);;
checkClassNameAux cname (c:ct) i =
  let cn = className c in
    if cn == cname
    then checkClassNameAux cname ct (i+1)
    else checkClassNameAux cname ct i
;;
checkClassName cname cTable = checkClassNameAux cname cTable 0;;

-- check if a variable is typed in the context
lookupContext :: String -> [(String, Type)] -> Maybe Type
lookupContext _ []           = Nothing;;
lookupContext v ((s, t):ctx) =
  if v == s
  then Just t
  else lookupContext v ctx
;;

-- control types of fields
checkFieldType  :: String -> ClassTable -> Bool
checkFieldsType :: FDS -> ClassTable -> Either Exception ()
checkFieldsType [] _                 = Right ();;
checkFieldsType ((tf, _):fds) cTable =
  let t = fromType tf in
  if checkFieldType t cTable
  then checkFieldsType fds cTable
  else Left (ClassNotFoundException ("Type " ++ t ++ " does not exists!"))
;;
-- control if a type exists in a field declaration
checkFieldType t cTable =
  case tryGetClass t cTable of
    Nothing -> False
    Just t' -> True
;;


-- Utilities for dynamic binding

-- remove an element in a list xs at index idx
removeInListAt :: Int -> [a] -> [a]
removeInListAt idx xs = 
  let (lft, (_:rgt)) = splitAt idx xs in
    lft ++ rgt
;;

-- check if a method is equal in the proper sense to all the others
oneVsAll :: Method -> [Method] -> Bool
oneVsAll _ []        = True;;
oneVsAll m1 (m2:mds) =
  (methodsEq m1 m2) &&
  (oneVsAll m1 mds)
;;

-- check if a signature of a method is correct in overriding
checkSignature    :: [Method] -> Bool
checkSignatureAux :: [Method] -> Int -> [Method] -> Bool
checkSignatureAux [] _ _         = True;;
checkSignatureAux (m:mds) i mdss =
  oneVsAll m (removeInListAt i mdss) &&
  checkSignatureAux mds (i+1) mdss
;;
checkSignature mds = checkSignatureAux mds 0 mds;;

-- given a list of indexes and a list of methods gives the methods at those indexes
getMethodsByIndexes :: [Int] -> [Method] -> [Method]
getMethodsByIndexes [] _         = [];;
getMethodsByIndexes (i:idxs) mds =
  (mds !! i):(getMethodsByIndexes idxs mds)
;;

-- count how many times a method has been overrided
howManyOverrides :: [Method] -> [Method] -> [(String, [Int])]
howManyOverrides [] _         = [];;
howManyOverrides (m:mds) mdss =
  let mName = methodName m in
  let idxs  = findIndices (== mName) (map (methodName) mdss) in
    if (length idxs) > 1
    then nub ((mName, idxs):(howManyOverrides mds mdss))
    else howManyOverrides mds mdss
;;

-- returns the types from a list o parameters
getTypesNames :: [(Type, String)] -> [String]
getTypesNames []                   = [];;
getTypesNames ((TypeDecl t, _):ps) = t:(getTypesNames ps);;

-- equality between two methods
methodsEq :: Method -> Method -> Bool
methodsEq m1@(MethodDecl t1 mName1 params1 _) m2@(MethodDecl t2 mName2 params2 _) =
  t1 == t2         &&
  mName1 == mName2 &&
  (getTypesNames params1) == (getTypesNames params2)
;;

-- check overriding between methods
checkOverride :: [Method] -> Bool
checkOverrides :: [(String, [Int])] -> [Method] -> Bool
checkOverrides [] _                 = True;;
checkOverrides ((_, idxs):hmos) mds =
  checkSignature (getMethodsByIndexes idxs mds) &&
  checkOverrides hmos mds
;;
checkOverride mds =
  if allDifferent (map (methodName) mds)
  then True
  else
    let hmo = howManyOverrides mds mds in
      checkOverrides hmo mds
;;
