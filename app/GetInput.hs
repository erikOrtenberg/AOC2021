module GetInput (getFile) where

getFile :: String -> IO String
getFile s = readFile ("../input/" ++ s)
