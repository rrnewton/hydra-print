-- This isn't a unit test.

module Main where

import UI.HydraPrint (hydraPrint)
import qualified System.IO.Streams as S
import Control.Concurrent
import Data.ByteString.Char8 as B
import Data.IORef

main :: IO ()
main = do
  c1 <- newIORef 0
  c2 <- newIORef 0    
  s1 <- S.makeInputStream $ do
          c <- readIORef (c1 :: IORef Int)
          threadDelay$ 300 * 1000
          return$ Just$ B.pack$ show$ 100 * c
  s2 <- S.makeInputStream $ do
          c <- readIORef (c2 :: IORef Int)
          threadDelay$ 500 * 1000
          return$ Just$ B.pack$ show$ 10 * c
  src <- S.fromList [("s1",s1),("s2",s2)]
  hydraPrint src
