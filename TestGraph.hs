import Tree as T 
-- import View as V

type TestType = (Int, String)

tree = Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 1 Empty Empty)

testKey :: TestType -> Int
testKey (i, s) = i

tree2 = Branch (3, "3") (Branch (2, "2") (Branch (1, "1") Empty Empty) Empty) (Branch (1, "1") Empty Empty)

main = do
    -- putStrLn $ T.printTree tree2
    let node = T.getNodeKey 3 testKey tree2
    print (T.getParents tree)
  --  print (T.addOneParentKey 4 1 tree)
    print (T.getParents tree2)