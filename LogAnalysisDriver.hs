module Main where

    import LogAnalysis
    import Log
    main :: IO String
    main = return $ show $ testWhatWentWrong parse whatWentWrong "sample.log"
