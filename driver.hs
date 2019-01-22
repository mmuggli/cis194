import LogAnalysis
import Log

main :: IO ()
main = do m <- (testParse parse 5523 "error.log")
          print $ show $ whatWentWrong m
     
