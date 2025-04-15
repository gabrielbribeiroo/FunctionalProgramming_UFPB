-- Exemplos do Capítulo 10

import System.IO

act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

getLin :: IO String
getLin = do x <- getChar
            if x == '\n' then
                return []
            else
                do xs <- getLin
                   return (x:xs)

putSt :: String -> IO ()
putSt []     = return ()
putSt (x:xs) = do putChar x
                  putSt xs

putStLn :: String -> IO ()
putStLn xs = do putSt xs
                putChar '\n'

strlen :: IO ()
strlen = do putStr "Entre com uma string: "
            xs <- getLin
            putStr "A string tem "
            putStr (show (length xs))
            putStrLn " caracteres"

-- Jogo da forca
hangman :: IO ()
hangman = do putStrLn "Pense numa palavra: "
             word <- sgetLine
             putStrLn "Tente advinhá-la:"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)    

getCh :: IO Char  
getCh = do hSetEcho stdin False
           x <- getChar              
           hSetEcho stdin True
           return x


play :: String -> IO ()
play word =
   do putStr "? "
      guess <- getLine
      if guess == word then
         putStrLn "Você acertou!"
      else
         do putStrLn (match word guess)
            play word


match :: String -> String -> String
match xs ys =
   [if elem x ys then x else '-' | x <- xs]
   

