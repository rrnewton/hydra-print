{-# LANGUAGE CPP #-}
-- This isn't a unit test.

module Main where

import UI.HydraPrint (hydraPrint, dbgLogLn)
import qualified System.IO.Streams as S
import System.IO.Streams.Concurrent as CS 
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forM_)
import Data.ByteString.Char8 as B
import Data.IORef
import Prelude as P

-- baseStrmRate = 1000 -- Seconds
baseStrmRate = 500
-- baseStrmRate = 100 


main :: IO ()
main = do
  c1 <- newIORef 1
  s1 <- S.makeInputStream $ do
          c <- readIORef (c1 :: IORef Int)
          threadDelay$ 2000 * baseStrmRate
          writeIORef c1 (c+1)
          return$ Just$ B.pack$ show (100 * c) ++"\n"
   
  strmChan <- newChan
  writeChan strmChan (Just ("Strm-0",s1))

  -- Add a new stream every 3 seconds, they each expire after 5.
  ----------------------------------------
  forkIO $ do 
    forM_ [1.. 3] $ \ix -> do 
      dbgLogLn "[strmsrc] Waiting before spawning additional strm..."
      threadDelay$ 3000 * 1000
      c2 <- newIORef 1
      sN <- S.makeInputStream $ do
              c <- readIORef (c2 :: IORef Int)
              threadDelay$ 1000 * baseStrmRate
              writeIORef c2 (c+1)
              if c >= 8000 `quot` baseStrmRate
               then return Nothing
               else return$ Just$ B.pack$ show ix ++":"++ show (10 * c) ++"\n" 
      dbgLogLn$"[strmsrc] Spawning additional stream! "++show ix
      writeChan strmChan (Just ("strm-"++show ix, sN))
    dbgLogLn "[strmsrc] Waiting before cutting off stream source..."
    threadDelay$ 8 * 1000 * 1000
    dbgLogLn "[strmsrc] Shutting off stream source!"
    writeChan strmChan Nothing
    dbgLogLn "[strmsrc] All done, exiting thread."
  ----------------------------------------

  strmSrc <- CS.chanToInput strmChan
  hydraPrint strmSrc
