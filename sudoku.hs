import Data.List
import qualified Data.Set as Set
import Data.Bool.HT
import Testfelder       
import Outp 
                 
import Debug.Trace
                 
column :: Sudoku -> Int -> [Int]
column f i = let ii = 1 + mod (i -1) 9
                 n9 = [0..8]
                 ns = map (\x -> (9 * x) + ii -1) n9
               in (map (f !!) ns)  

rowI :: Int -> Int
rowI i = 1 + floor (fromIntegral (i - 1) / 9)

row  :: Sudoku -> Int -> [Int]
row f i    = let ii = rowI i
                 n0 = (ii - 1) * 9
                 n1 = ii * 9
               in drop n0 $ take n1 f 

                 
boxIJ f i j  = let i3  = 27 * (i - 1)
                   j3  = 3  * (j - 1)
                   ij1 = i3 + j3 
                   ij2 = ij1 + 9
                   ij3 = ij2 + 9
                   n   = [ij1..(ij1 + 2)] ++ 
                         [ij2..(ij2 + 2)] ++ 
                         [ij3..(ij3 + 2)] in
                 map (f!!) n

boxI :: Int -> Int
boxJ :: Int -> Int
boxI  i   = 1 +     (floor $ (fromIntegral  (i - 1)) / 27)
boxJ  i   = 1 + mod (floor $ (fromIntegral  (i - 1)) /  3) 3


box :: Sudoku -> Int -> [Int]
box f i = let l = boxI i
              m = boxJ i in
            boxIJ f l m   
           
empties :: Sudoku -> [Int]
empties  f = let z = zip [1..] f  
                 isEmpty x = if (snd x == 0) then fst x else 0
                 fil = filter  (\x ->  x /= 0) (map isEmpty z)
             in  fil 
                 

replace :: Sudoku -> Int -> Int -> Sudoku
replace f i new = let (fs,sn) = (splitAt i f)  in
                   (init fs) ++ [new] ++ sn



possible :: Sudoku -> Int -> Sudoku
possible f i     = let r = row f i 
                       c = column f i 
                       b = box f i
                       rcb = r `union` c `union` b
                       invert x  = [1..9] \\ x
                     in 
                     invert (filter (/= 0) rcb)

possibles f = let es = empties f 
                  posi i = (i ,(possible f i))
              in
              map posi  es


possiblesLen1 f = let p = possibles f 
                      fil = filter len1 p in
                      fil
                  where len1 =  (\i -> (length $ snd i) == 1)


               
                            
             
solve1 f = let p1 = possiblesLen1 f 
               h  = head p1 
               n  = fst h
               x  = head (snd h)
            in                            
             trace ("s1: " ++ show p1 ++ "\n")
             replace f n x             
            
solveMore f = let p1     =  possibles f 
                  p      = head p1 
                  n = fst p 
                  xs = snd p
                  x  = head xs    in
               trace ("sm: " ++ show p1 ++ "\n")
               trace ("smf: \n" ++ sShow f  )
               replace f n x

solve f
          | null $ empties f                = f 
          | not (null $ possiblesLen1 f)    = solve $ solve1    f                           
          | null $ possiblesLen1 f          = solve $ solveMore f
          | otherwise              = f
          
            
