{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

    import Log

    parseWords :: [String] -> LogMessage
    parseWords ("I":t:m) = LogMessage Info (read t) (unwords m)
    parseWords ("W":t:m) = LogMessage Warning (read t) (unwords m)
    parseWords ("E":s:t:m) = LogMessage (Error (read s)) (read t) (unwords m)
    parseWords s = Unknown (unwords s)

    parseMessage :: String -> LogMessage
    parseMessage s = parseWords (words s)


    parse :: String -> [LogMessage]        
    parse s = map parseMessage (lines s)


              
    insert :: LogMessage -> MessageTree -> MessageTree
    insert (Unknown _) t = t
    insert l Leaf = Node Leaf l Leaf
    insert l@(LogMessage _ ts _) (Node lb il@(LogMessage _ its _) rb ) = if ts < its
                                                                                  then Node (insert l lb) il rb
                                                                                  else Node lb il (insert l rb)

    insert (LogMessage _ _ _) (Node _ (Unknown _) _) = undefined

    -- swap args to make compatible with fold
    myinsert :: MessageTree -> LogMessage -> MessageTree
    myinsert a b = insert b a

    build :: [LogMessage] -> MessageTree
    build ms = foldl myinsert Leaf ms 
               
    inOrder :: MessageTree -> [LogMessage]
    inOrder Leaf = []
    inOrder (Node lb l rb) = (inOrder lb) ++ [l] ++ (inOrder rb)
