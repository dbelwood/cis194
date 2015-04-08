{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

  import Log

  parseMessage :: String -> LogMessage
  parseMessage string
    | (parts !! 0) == "E" = LogMessage 
        (Error ((read (parts !! 1))::Int))
        ((read (parts !! 2))::Int)
        (unwords (drop 3 parts))
    | (parts !! 0) == "I" = LogMessage 
        Info 
        ((read (parts !! 1))::Int)
        (unwords (drop 2 parts))
    | (parts !! 0) == "W" = LogMessage 
        Warning 
        ((read (parts !! 1))::Int)
        (unwords (drop 2 parts))
    | otherwise = Unknown string
      where parts = words(string)

  parse :: String -> [LogMessage]
  parse string = map parseMessage (lines string)

  insert :: LogMessage -> MessageTree -> MessageTree
  insert (Unknown _) tree = tree
  insert (LogMessage _ _ _) tree@(Node _ (Unknown _) _) = tree
  insert message Leaf = Node Leaf message Leaf
  insert message@(LogMessage _ timestamp _) 
      tree@(Node leftTree nodeMessage@(LogMessage _ nodeTs _) rightTree)
    | timestamp < nodeTs = (Node (insert message leftTree) nodeMessage rightTree)
    | timestamp > nodeTs = (Node leftTree nodeMessage (insert message rightTree))
    | otherwise = tree

  build :: [LogMessage] -> MessageTree
  build messages = foldl (\acc x -> (x acc)) Leaf fns
    where fns = (map insert messages) -- [(MessageTree -> MessageTree)]

  inOrder :: MessageTree -> [LogMessage]
  inOrder Leaf = []
  inOrder (Node leftTree message rightTree) = 
    inOrder(leftTree) ++ [message] ++ inOrder(rightTree)

  whatWentWrong :: [LogMessage] -> [String]
  whatWentWrong xs =  [ msg | (LogMessage _ _ msg) <-
                      (inOrder 
                      (build 
                      [ x | x@(LogMessage ( Error sev ) _ _) <- xs, sev >= 50 ]
                      )
                      )
                      ]
