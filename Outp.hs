module Outp where

import Data.List
import qualified Data.Set as Set
import Data.Bool.HT
import Testfelder       
                 
                 

          
          
             
sStr :: Sudoku -> Integer  -> String 
sStr [] _  = ""
sStr (h:t)  i 
        | mi 27 = next "\n\n"
        | mi 9  = next "\n"
        | mi 3  = next "   "
        | True  = next " "
        where mi m = (mod i m == 0)
              next zeichen = concat [show h, zeichen, sStr t (i + 1)] 

sShow :: Sudoku -> String
sShow s = sStr s 1 
  where 
    sStr :: Sudoku -> Integer  -> String 
    sStr [] _  = ""
    sStr (h:t)  i 
        | mi 27 = next "\n\n"
        | mi 9  = next "\n"
        | mi 3  = next "   "
        | True  = next " "
          where 
            mi m = (mod i m == 0)
            next zeichen = concat [show h, zeichen, sStr t (i + 1)] 

putSudoku :: Sudoku -> IO ()
putSudoku a =  putStrLn $ sShow a



