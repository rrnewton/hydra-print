{-# LANGUAGE CPP #-}
-- This isn't a unit test.

module Main where

import UI.HydraPrint (hydraPrint)
import qualified System.IO.Streams as S
import Control.Concurrent
import Control.Monad (forM_)
import Data.ByteString.Char8 as B
import Data.IORef
import Prelude as P

main :: IO ()
main = do
  c1 <- newIORef 0
  c2 <- newIORef 0    
  s1 <- S.makeInputStream $ do
          c <- readIORef (c1 :: IORef Int)
          threadDelay$ 300 * 1000
          writeIORef c1 (c+1)
          return$ Just$ B.pack$ show$ 100 * c
  s2 <- S.makeInputStream $ do
          c <- readIORef (c2 :: IORef Int)
          threadDelay$ 500 * 1000
          writeIORef c2 (c+1)
          return$ Just$ B.pack$ show (10 * c) ++"\n"
--  src <- S.fromList [("s1",s1),("s2",s2)]
  src <- S.fromList [("s2",s2)]

#if 1
  hydraPrint src
#else
  Just (_,s2') <- S.read src
  forM_ [1..] $ \i -> do
    x <- S.read s2'
    P.putStrLn$ "Read from stream ["++show i++"]: "++ show x
#endif
