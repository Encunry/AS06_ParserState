module Main (main) where

import Lang.Imp.Ast
import Lang.Imp.Interpreter
import Lang.Imp.Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Map.Strict as M

-- assuming arguments like m=5. Replacing = with :=, to parse the args using parser
replace_eq :: String -> String
replace_eq = concatMap (\char -> if char == '=' then ":=" else [char])

run :: Cmd -> State -> IO State
run cmd st = case run_state (exec cmd) st of
    Left err       -> putStrLn ("Runtime Error" ++ "err") >> exitFailure
    Right (st', _) -> pure st'


show_state :: State -> String
show_state state = concatMap line (M.toList state) where
    line (name, val) = name ++ " -> " ++ show val ++ "\n"


main :: IO ()
main = do
    -- read file
    (file: args) <- getArgs
    content <- readFile file

    -- parse args to commands & print
    let arg_cmds = map (parse_imp . replace_eq) args

    -- parse file to command
    let program = parse_imp content

    -- check for parse errors, then run
    case (sequence arg_cmds, program) of
        (Left _ , _) -> putStrLn "Parser Error in Args"
        (_ , Left _) -> putStrLn "Parser Error in Program"
        (Right arg, Right prog) -> do
            input_state <- run (Cmd_Sequence arg) emptyState
            putStrLn "input state: "
            putStrLn $ show_state input_state

            final_state <- run prog input_state
            putStrLn "final state: "
            putStrLn $ show_state final_state




--main = do
--    [fileName] <- getArgs
--    content <- readFile fileName
--    putStrLn "Parsed file:"
--   print $ parse_imp content
