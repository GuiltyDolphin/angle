module Angel.Main.RunProg where
    

import System.Environment
import System.IO
    

main = do
  (progName:args) <- getArgs
  
