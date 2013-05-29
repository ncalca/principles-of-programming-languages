module Main where

-- Monad definition: commented because it is already defined in Prelude.
--class Monad m where
--
--   return :: a -> m a
--   
--   (>>=) :: m a -> (a -> m b)  -> m b
--   
--   (>>)  :: m a -> m b -> m b
--    >> k = m >>=\_ -> k -- default implementation


-- State monad
newtype State st a = State (st -> (st, a))

instance Monad (State state) where

    return x = State (\s -> (s,x))
--  alternatively
--  return x = let f st = (st, x)
--             in State f

    State f >>= g = State(\oldstate -> 
                        let (newstate, val) = f oldstate
                            State f'        = g val
                        in f' newstate)

-- Recursive data structure for representing trees
data Tree a = Leaf a | Branch (Tree a) (Tree a) 
              deriving (Show,Eq)

mapTreeM :: (a -> State state b) -> Tree a -> State state (Tree b)
mapTreeM f (Leaf a) = 
     f a >>= (\b -> return (Leaf b))

mapTreeM f (Branch lhs rhs) = do
     lhs' <- mapTreeM f lhs
     rhs' <- mapTreeM f rhs
     return (Branch lhs' rhs')


-- Utility methods     
getState :: State state state
getState = State(\state -> (state, state))

putState :: state -> State state ()
putState new = State(\_ -> (new, ()))

collectTree :: Tree b -> State [b] (Tree b)
collectTree tree = mapTreeM collectList tree
         where collectList v = do
                cur <- getState
                putState ( [v])
                return v
                
tree2list tree = let State f = (collectTree tree)
                     in f []                
 
-- An instance of a tree                         
testTree = (Branch 
              (Branch
                  (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c')))
              (Branch
                  (Leaf 'd') (Leaf 'e')))


-- Greatest common divisor
-- Classic definition
gcdf x y | x == y = x
gcdf x y | x < y = gcdf x (y-x)
gcdf x y = gcdf (x-y) y


-- Using monads...
type ImpState = (Int, Int)

getX, getY :: State ImpState Int
getX = State(\(x,y) -> ((x,y), x))
getY = State(\(x,y) -> ((x,y), y))

putX, putY :: Int -> State ImpState ()
putX x' = State (\(x,y) -> ((x', y), ()))
putY y' = State (\(x,y) -> ((x, y'), ()))

gcdST = do {
                x <- getX; 
                y <- getY;
                (if x == y
                 then return x
                 else
                    if x < y
                    then do {putY (y-x); gcdST }
                    else do {putX (x-y); gcdST }
                 )
           } 

runStateM :: State state a -> state -> a
runStateM (State f) st = snd (f st)


main :: IO()              
main = print $ runStateM gcdST (21, 17)




