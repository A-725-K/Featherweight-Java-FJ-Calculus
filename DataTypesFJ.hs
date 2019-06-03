module DataTypesFJ where

-- type for expressions
data Exp =
    Variable String
  | FieldAccess Exp String
  | MethodInv Exp String [Exp]
  | New String [Exp]
  | Cast String Exp
  deriving (Eq)
;;
-- type for methods
data Method     = MethodDecl Type String [(Type, String)] Exp deriving (Eq);;
-- type for classes
data Class      = ClassDecl String String FDS MDS deriving (Eq);;
-- type for type declaration
data Type       = TypeDecl String deriving (Eq);;

-- fields in a class
type FDS        = [(Type, String)];;
-- methods in a class
type MDS        = [Method];;
-- all classes of the program
type ClassTable = [Class];;
-- all types of the program
type Context    = [(String, Type)];;

-- to print better type declarations
showType (TypeDecl s) = s;;
instance Show Type where show = showType;;

-- to print better an expression
showExp (Variable s)      = s;;
showExp (FieldAccess e s) = show e ++ "." ++ s;;
showExp (MethodInv e s p) = show e ++ "." ++ s ++ "(" ++ show p ++ ")";;
showExp (New s p)         = "new " ++ s ++ "(" ++ show p ++ ")";;
showExp (Cast s e)        = "(" ++ s ++ ")" ++ show e;;
instance Show Exp where show = showExp;;

-- to print better a method declaration
showMethodDecl (MethodDecl t s p e) =
  show t ++ " " ++ s ++ "(" ++ show p ++ ") { " ++ "return " ++ show e ++ "; }";;
instance Show Method where show = showMethodDecl;;

-- to print better a class declaration
showNewLines [] = "";
showNewLines (x:xs) = show x ++ "\n\t" ++ showNewLines xs;
showClassDecl (ClassDecl s1 s2 f m) =
  "\nclass " ++ s1 ++ " extends " ++ s2 ++ " {\n\t" ++ showNewLines f ++ "\n\t" ++ showNewLines m ++ "\n}\n";;
instance Show Class where show = showClassDecl;;

-- static exceptions
data Exception =
    VariableNotFoundException  String
  | MethodNotFoundException    String
  | FieldNotFoundException     String
  | ClassNotFoundException     String
  | DuplicateVariableException String
  | DuplicateClassException    String
  | DuplicateMethodException   String
  | DuplicateFieldException    String
  | MismatchParamsException    String
  | TypeException              String
  | OverrideException          String
  | Exception                  String
  deriving (Eq)
;;

-- to print better static exceptions
showException (VariableNotFoundException e)  = "*** VariableNotFoundException ==> " ++ e ++ " ***";;
showException (MethodNotFoundException e)    = "*** MethodNotFoundException ==> " ++ e ++ " ***";;
showException (FieldNotFoundException e)     = "*** FieldNotFoundException ==> " ++ e ++ " ***";;
showException (ClassNotFoundException e)     = "*** ClassNotFoundException ==> " ++ e ++ " ***";;
showException (DuplicateVariableException e) = "*** DuplicateVariableException ==> " ++ e ++ " ***";;
showException (DuplicateClassException e)    = "*** DuplicateClassException ==> " ++ e ++ " ***";;
showException (DuplicateMethodException e)   = "*** DuplicateMethodException ==> " ++ e ++ " ***";;
showException (DuplicateFieldException e)    = "*** DuplicateFieldException ==> " ++ e ++ " ***";;
showException (MismatchParamsException e)    = "*** MismatchParamsException ==> " ++ e ++ " ***";;
showException (TypeException e)              = "*** TypeException ==> " ++ e ++ " ***";;
showException (OverrideException e)          = "*** OverrideException ==> " ++ e ++ " ***";;
showException (Exception e)                  = "*** Exception ==> " ++ e ++ " ***";;
instance Show Exception where show           = showException;;

--runtime exceptions
data RuntimeException =
    ClassCastException String
  | RuntimeException   String
;;

-- to print better exceptions at runtime
showRuntimeException (ClassCastException e)  = "*** ClassCastException ==> " ++ e ++ " ***";;
showRuntimeException (RuntimeException e)    = "*** RuntimeException ==> " ++ e ++ " ***";;
instance Show RuntimeException where show    = showRuntimeException;;
