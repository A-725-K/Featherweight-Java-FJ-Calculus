module MonadicParserFJ where
-- THANKS TO:
--
-- https://www.cs.rit.edu/~swm/cs561/monadic-parsing-jfp.pdf 
-- https://wiki.haskell.org/Functor-Applicative-Monad_Proposal

-- Monadic Parser
import Control.Applicative (Applicative(..));;
import Control.Monad (liftM, ap);;
import DataTypesFJ;;
import Data.Char;;

-- Grammar for exp:
-- p   ::= cd_1 ... cd_n
-- cd  ::= class C extends C' { fds mds }
-- fds ::= fd_1 ... fd_n
-- fd  ::= C f;
-- mds ::= md_1 ... md_n
-- md  ::= C_0 m(C_1 x_1, ..., C_n x_n) { return e; }
-- e   ::= x | e.f | e_0.m(e_1, ..., e_n) | new C(e_1, ..., e_n) | (C)e | ( e )

-- parser for strings
newtype Parser a = Parser(String -> [(a, String)]);;

-- function that apply a parser to a string
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) s = p s;;

-- parser that ever fails
failure :: Parser a
failure = Parser(\_ -> []);;

-- parse a char
item :: Parser Char
item = Parser(\s ->
                case s of
                  []     -> []
                  (x:xs) -> [(x, xs)]
             )
;;

-- if the first parser fail, apply the second one
(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser(\s ->
                     let p = parse p1 s in
                       case p of
                         [] -> parse p2 s
                         p' -> p'
                  )
;;

-- monadic behavior of parser
instance Functor Parser where
  fmap = liftM
;;

instance Applicative Parser where
  pure c = Parser(\s -> [(c,s)])
  (<*>)  = ap
;;

instance Monad Parser where
  return    = pure
  p1 >>= p2 = Parser(\s ->
                       let p = parse p1 s in
                         concat (map (\(c, cs) -> parse (p2 c) cs) p)
                    )
  ;;
;;

-- if a predicate on char is satisfied
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item;
  if p c
  then return c
  else failure
;;

-- useful parsers
digit, upper, lower, letter :: Parser Char
digit  = sat isDigit;;
upper  = sat isUpper;;
lower  = sat isLower;;
letter = sat isLetter;;

-- parser of a specific char
char :: Char -> Parser Char
char c = sat (c==);;

-- parser of a specific string
string :: String -> Parser String
string ""     = return "";;
string (c:cs) = do
  x  <- char c;
  xs <- string cs;
  return (x:xs)
;;

-- try to apply one or more times a parser
many, many1 :: Parser a -> Parser [a]
many p  = many1 p +++ return [];;
many1 p = do
  x  <- p;
  xs <- many p;
  return (x:xs)
;;

-- parser for space character
space :: Parser String
space = many (sat isSpace);;

-- parsers for symbols
symbol :: String -> Parser String
symbol s = do
  space;
  ss <- string s;
  space;
  return ss
;;
  
-- Parser for FJ
-- parser for a generic string, that represents a variable or a class name
varname :: Parser String
varname = do
  fst  <- letter +++ (char '_');
  rest <- many (letter +++ (digit +++ (char '_')));
  return (fst:rest)
;;

-- apply a parser and then a parser for a delimiter many times
separate_by, separate_by1 :: Parser a -> Parser b -> Parser [a]
separate_by  p sep = (p `separate_by1` sep) +++ return [];;
separate_by1 p sep = do
  a  <- p;
  as <- many (do {sep; p});
  return (a:as)
;;

-- UTILS: base for method invocation and field access
parseE0 :: Parser Exp
parseE0 = parseNew +++ parseVariable;;

-- parse parenthesis to change the evaluation order
parseParenthesis :: Parser Exp
parseParenthesis = do
  {
    symbol "(";
    e <- term;
    symbol ")";
    leftAssociate e
  }
;;
---------------------------------------------------------------------------

-- parser for variable and its type
parseTypeAndName :: Parser (Type, String)
parseTypeAndName = do
  {
    t <- varname;
    space;
    e <- varname;
    return (TypeDecl t, e)
  }
;;

-- x
parseVariable :: Parser Exp
parseVariable = do
  {
    x <- varname;
    return (Variable x)
  }
;;

-- (C)e
parseCast :: Parser Exp
parseCast = do
  {
     symbol "(";
     c <- varname;
     symbol ")";
     e <- term;
     return (Cast c e)
  }
;;

-- new C(e1,...,en)
parseNew :: Parser Exp
parseNew = do
  {
     symbol "new";
     c <- varname;
     symbol "(";
     params <- term `separate_by` (symbol ",");
     symbol ")";
     return (New c params)
  }
;;

-- e.f
parseFieldAccess :: Parser Exp
parseFieldAccess = do
  {
     e0 <- parseE0;
     symbol ".";
     f <- varname;
     leftAssociate (FieldAccess e0 f)
  }
;;

-- e0.m(e1,...,en)
parseMethodInv :: Parser Exp
parseMethodInv = do
  {
    e0 <- parseE0;
    symbol ".";
    m <- varname;
    symbol "(";
    params <- term `separate_by` (symbol ",");
    symbol ")";
    leftAssociate (MethodInv e0 m params)
  } 
;;

-- left association for . operator
leftAssociate :: Exp -> Parser Exp
leftAssociate accP = do
  {
    e <- (symbol ".") +++ return [];
    case e of
      []  -> return accP
      "." -> do
        {
          e' <- varname;
          openP <- symbol "(" +++ return [];
          case openP of
            [] -> leftAssociate (FieldAccess accP e')
            _ -> do
              {
                params <- term `separate_by` (symbol ",");
                symbol ")";
                leftAssociate (MethodInv accP e' params)
              }
        }
  }
;;

-- parser for a single term
term :: Parser Exp
term =
  parseCast        +++
  parseParenthesis +++
  parseMethodInv   +++
  parseFieldAccess +++
  parseNew         +++
  parseVariable    
;;

-- C0 m (C1 x1,...,Cn xn) {return e;}
parseMethodDecl :: Parser Method
parseMethodDecl = do
  {
     (c0, m) <- parseTypeAndName; 
     symbol "(";
     params <- parseTypeAndName `separate_by` (symbol ",");
     symbol ")";
     symbol "{";
     symbol "return";
     e <- term;
     symbol ";";
     symbol "}";
     return (MethodDecl c0 m params e)
  }
;;

-- m1...mn
parseMethodsDecls :: Parser MDS
parseMethodsDecls = many parseMethodDecl;;

-- parse the body of a class
-- C1 f1;...;Cn fn;
parseClass :: Parser (FDS, MDS)
parseClass = do
  {
    m <- parseMethodsDecls;
    case m of
      [] -> do
        {
          f <- parseTypeAndName +++ return (TypeDecl "", "");
          case f of
            (TypeDecl "", "") -> return ([], [])
            f' -> do
              {
                symbol ";";
                (fs, ms) <- parseClass;
                return (f':fs, ms)
              }
        }
      m' -> return ([], m')
  }
;;

-- class C extends C' { mds ; fds }
parseClassDecl :: Parser Class
parseClassDecl = do
  {
    symbol "class";
    c <- varname;
    symbol "extends";
    cc <- varname;
    symbol "{";
    (fields, methods) <- parseClass;
    symbol "}";
    return (ClassDecl c cc fields methods)
  }
;;

-- parse all the program
parseProg :: Parser ClassTable
parseProg = many1 parseClassDecl;;

-- parse all terms
terms :: Parser [Exp]
terms = many1 term;;
