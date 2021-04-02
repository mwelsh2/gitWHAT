import CommandInterface as CI

main :: IO ()
main = do
line <- getLine
let words = CI.splitString line
let first = CI.testForValidityOfCommand (words !! 0)
-- print (testForValidityOfCommand first)
print first
print (fmap CI.testForValidityOfCommand words) --tests 
