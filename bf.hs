module Main where
import Debug.Trace
import Char
import Maybe
import IO

data Tape a = Tape a [a] [a]
            deriving Show


data Machine = Machine { tape :: (Tape Int),               
                         input :: String,
                         output :: String }
             deriving Show
               
doOn :: Tape a -> (a -> a) -> Tape a
doOn (Tape val left right) f = Tape (f val) left right

goLeft :: Tape Int -> Tape Int
goLeft (Tape val [] right) = Tape 0 [] (val:right)
goLeft (Tape val (l:ls) right) = Tape l ls (val:right)

goRight :: Tape Int -> Tape Int
goRight (Tape val left []) = Tape 0 (val:left) []
goRight (Tape val left (r:rs)) = Tape r (val:left) rs

getTapeData :: Tape Int -> Char
getTapeData (Tape val _ _ ) = chr val

replaceInput :: Machine -> String -> Machine
replaceInput (Machine t i o) ni = Machine t ni o

toBrace :: String -> String
toBrace (x:xs) = case x of
  ']' -> ""
  _ -> x : toBrace xs
  
fromBrace :: String -> String  
fromBrace (x:xs) = case x of
  ']' -> xs
  _ -> fromBrace xs

getOutput :: Machine -> String
getOutput (Machine _ _ o) = o

evalLoop :: Machine -> Machine
--evalLoop m@(Machine t i o) | trace (show m) False = undefined
evalLoop m@(Machine t@(Tape val left right) i o) = case val of 
  0 -> m
  _ -> evalLoop (replaceInput (eval m) i)


eval :: Machine -> Machine
--eval (Machine t i o) | trace ("tape: " ++ (show t)) False = undefined
eval m@(Machine t (i:is) o) = case i of
  '+' -> eval (Machine (doOn t succ) is o)
  '-' -> eval (Machine (doOn t pred) is o)
  '<' -> eval (Machine (goLeft t) is o)
  '>' -> eval (Machine (goRight t) is o)
  '.' -> eval (Machine t is (getTapeData(t):o))
--  ',' -> eval (Machine (fillChar t) is o)
  '[' -> eval (replaceInput (evalLoop (Machine t (toBrace is) o)) (fromBrace is))
  _ -> m 
  
eval m@(Machine _ _ _) = m

--fillChar :: Tape a -> Tape a
--fillChar (Tape val left right) = do
--  c <- hGetChar stdin
--  return (ord c)
  

main :: IO ()               
main = let code = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." in
           (putStrLn . reverse . getOutput . eval) (Machine (Tape 0 [] []) code "")
           