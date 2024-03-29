module Testfelder where       

type Sudoku = [Int]                       
                       

field :: Sudoku 
field =  [1, 2, 3,   4, 5, 6,   7, 8, 9,
         4, 5, 6,   7, 8, 9,   1, 2, 3,
         7, 8, 9,   1, 2, 3,   4, 5, 6,

         2, 3, 4,   5, 6, 7,   8, 9, 1,
         5, 6, 7,   8, 9, 1,   2, 3, 4,
         8, 9, 1,   2, 3, 4,   5, 6, 7,

         3, 4, 5,   6, 7, 8,   9, 1, 2,
         6, 7, 8,   9, 1, 2,   3, 4, 5,
         9, 1, 2,   3, 4, 5,   6, 7, 8]
field1 :: Sudoku         
field1 = [0, 2, 3,   4, 5, 6,   7, 8, 9,
         4, 5, 6,   7, 8, 9,   1, 2, 3,
         7, 8, 9,   1, 2, 3,   4, 5, 6,

         2, 3, 4,   5, 6, 7,   8, 9, 1,
         5, 6, 7,   8, 9, 1,   2, 3, 4,
         8, 9, 1,   2, 3, 4,   5, 6, 7,

         3, 4, 5,   6, 7, 8,   9, 1, 2,
         6, 7, 8,   9, 1, 2,   3, 4, 5,
         9, 1, 2,   3, 4, 5,   6, 7, 8]        

field11 :: Sudoku         
field11 = [0, 2, 3,   4, 5, 6,   7, 8, 9,
           4, 5, 6,   0, 8, 9,   1, 2, 3,
           7, 8, 9,   1, 2, 3,   4, 5, 6,

           2, 3, 4,   5, 6, 7,   8, 9, 1,
           5, 6, 7,   8, 9, 1,   2, 3, 4,
           8, 9, 1,   2, 3, 4,   5, 6, 7,

         3, 4, 5,   6, 7, 8,   9, 1, 2,
         6, 7, 8,   9, 1, 2,   3, 4, 5,
         9, 1, 2,   3, 4, 5,   6, 7, 8]        

field111 :: Sudoku         
field111 = [1, 2, 3,   4, 5, 6,   7, 8, 9,
            4, 5, 6,   0, 8, 9,   1, 2, 3,
            7, 8, 9,   1, 2, 3,   4, 5, 6,

            2, 3, 4,   5, 6, 7,   8, 9, 1,
            5, 6, 7,   8, 9, 1,   2, 3, 4,
            8, 9, 1,   2, 3, 4,   5, 6, 7,

            3, 4, 5,   6, 7, 8,   9, 1, 2,
            6, 7, 8,   9, 1, 2,   3, 4, 5,
            9, 1, 2,   3, 4, 5,   6, 7, 8]        

field2 :: Sudoku         
field2 = [0, 2, 3,   4, 5, 6,   7, 8, 9,
          4, 5, 6,   7, 8, 9,   1, 2, 3,
          7, 8, 9,   1, 2, 3,   4, 5, 6,

          2, 3, 4,   5, 6, 7,   8, 9, 1,
          5, 6, 7,   8, 9, 1,   2, 3, 4,
          8, 9, 1,   2, 3, 4,   5, 6, 7,

          3, 4, 5,   6, 7, 8,   9, 1, 2,
          6, 7, 8,   9, 1, 2,   3, 4, 5,
          9, 1, 2,   3, 4, 5,   6, 7, 8]        

field3 :: Sudoku
field3 = [0, 0, 3,   4, 5, 6,   7, 8, 9,
          4, 5, 6,   7, 8, 9,   1, 2, 3,
          7, 8, 9,   1, 2, 3,   4, 5, 6,

          0, 3, 4,   5, 6, 7,   8, 9, 1,
          5, 6, 7,   8, 9, 1,   2, 3, 4,
          8, 9, 1,   2, 3, 4,   5, 6, 7,

          3, 4, 5,   6, 7, 8,   9, 1, 2,
          6, 7, 8,   9, 1, 2,   3, 4, 5,
          9, 1, 2,   3, 4, 5,   6, 7, 0]        

field4 :: Sudoku
field4 = [0, 0, 0,   4, 5, 6,   7, 8, 9,
          4, 5, 6,   7, 8, 9,   1, 2, 3,
          7, 8, 9,   0, 0, 0,   4, 5, 6,

          0, 0, 0,   5, 6, 7,   8, 9, 1,
          5, 6, 7,   8, 9, 1,   2, 3, 4,
          8, 9, 1,   2, 3, 4,   5, 6, 7,

          0, 4, 5,   6, 7, 8,   9, 1, 2,
          6, 7, 8,   9, 1, 2,   3, 4, 5,
          9, 1, 2,   3, 4, 5,   6, 7, 0]        



field5 :: Sudoku
field5 = [0, 0, 0,   0, 0, 0,   7, 8, 9,
          0, 0, 0,   0, 0, 0,   1, 2, 3,
          0, 0, 0,   0, 0, 0,   4, 5, 6,

          0, 0, 0,   5, 6, 7,   8, 9, 1,
          5, 6, 7,   8, 9, 1,   2, 3, 4,
          8, 9, 1,   2, 3, 4,   5, 6, 7,

          0, 4, 5,   6, 7, 8,   9, 1, 2,
          6, 7, 8,   9, 1, 2,   3, 4, 5,
          9, 1, 2,   3, 4, 5,   6, 7, 0]        

field6 :: Sudoku
field6 = [1, 2, 3,   4, 5, 6,   7, 8, 9,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          8, 9, 1,   2, 3, 4,   5, 6, 7,
          
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          6, 7, 8,   9, 1, 2,   3, 4, 5,
          0, 0, 0,   0, 0, 0,   0, 0, 0]


field7 :: Sudoku
field7 = [1, 2, 3,   4, 5, 6,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0]



field10 :: Sudoku
field10 = [0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0,
          0, 0, 0,   0, 0, 0,   0, 0, 0]
