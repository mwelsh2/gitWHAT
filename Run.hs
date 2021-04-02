import System.IO (stdout, hFlush)
import System.Directory.Internal.Prelude (unless)
import CommandInterface as CI
import Model as M

-- Reads in a new command
read_ :: IO String
read_ = putStr "gitWHAT?> "
    >> hFlush stdout
    >> getLine

-- Evaluates command and passes along repository list
eval_ :: String -> [RepositoryState] -> (String, [RepositoryState])
eval_ input repoStates =
    let (command, params) = CI.parseInput input
    in CI.evalCommand command params repoStates

-- Prints informative message
print_ :: String -> IO ()
print_ = putStrLn

-- Driver function of the REPL loop
run :: [RepositoryState] -> IO ()
run repoStates = do
    input <- read_
    
    unless (input == ":quit") $
        let evalInput = eval_ input repoStates in
        print_ (fst evalInput) >> run (snd evalInput)

-- Main function
main :: IO ()
main = do 
    putStrLn "Enter :quit at anytime to exit."
    run []
    
    