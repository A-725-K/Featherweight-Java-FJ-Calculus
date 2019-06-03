import System.Directory;;
import MonadicParserFJ;;
import TypeCheckerFJ;;
import InterpreterFJ;;
import UtilitiesFJ;;
import DataTypesFJ;;
import System.IO;;

-- one-line main, using the classes in the environment
interpMain :: ClassTable -> IO ()
interpMain ct = do
  putStr "\nInsert main (one-line) or 0 to terminate:\n:- ";
  hFlush stdout;
  main <- getLine;
  if main == "0"
  then return ()
  else
    case parse terms main of
      [(parsed, residual)] -> do
        {
          case residual of
            [] -> do
              {
                putStrLn "\n--> Parsing OK!";
                case typeCheck (head parsed) ct [] of
                  Left e -> putStrLn (show e)
                  _      -> do
                    {
                      putStrLn "--> Typecheck OK!\n";
                      --putStrLn (show (reduceStar (head parsed) ct));
                      case reduceStar (head parsed) ct of
                        Left ex -> putStrLn (show ex)
                        Right p -> do
                          {
                            putStrLn (show p);
                            putStrLn "_______________________________________";
                            interpMain ct;
                          }
                    }
              }
            _ -> putStrLn ("Parsing Error ==> " ++ residual)
        }
      _  -> putStrLn "### Parser failed ! ###";
;;

-- load the environment from file
interpProgram :: IO ()
interpProgram = do
  putStr "\nInsert the path of the program:\t";
  hFlush stdout;
  fileName <- getLine;
  putStrLn "";
  ex <- doesFileExist fileName;
  if ex
  then
    do {
      f <- readFile fileName;
      case parse parseProg f of
        [(parsed, residual)] ->
          case residual of
            [] -> do
              {
                putStrLn "\n--> Parsing OK!";
                let { cTable = parsed ++ [(ClassDecl "Object" "" [] [])] };
                -- putStrLn (show (map (className) parsed)); -- class table loaded (obj implicit)
                case typeCheckProg parsed cTable of
                  Left e -> putStrLn (show e)
                  _      -> do
                    {
                      putStrLn "--> Typecheck OK!";
                      interpMain cTable;
                    }
              }
            _  -> putStrLn ("### Parser Error ==> " ++ residual);
        _ -> putStrLn "### Parser Error ! ###"
    }
  else do { putStrLn ("ERROR ==> File " ++ fileName ++ " does not exist!"); interpProgram; }
;;

-- gentle introduction to the script
main :: IO ()
main = do
  putStrLn "/-----------------------------------------------------------------\\";
  putStrLn "| FEATHER-WEIGHT JAVA (Interactive) INTERPRETER v. 1.0.0          |";
  putStrLn "| Author: A-725-K (a.k.a. Andrea Canepa 4185248)                  |";
  putStrLn "| Principles and Paradigms of Programming Languages               |";
  putStrLn "| a.y. 2018/2019 University of Genoa                              |";
  putStrLn "\\-----------------------------------------------------------------/";
  interpProgram;
;;
