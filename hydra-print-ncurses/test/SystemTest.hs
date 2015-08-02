{-# LANGUAGE CPP #-}

-- | Simple system test of hydra print.  Creates a main stream, and then adds three
--   child streams dynamically, which all expire before the main stream does.

module Main where

-- import UI.HydraPrint
import UI.HydraPrint.NCurses
import qualified System.IO.Streams as S
import System.IO.Streams.Concurrent as CS
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forM_)
import Data.ByteString.Char8 as B
import Data.IORef
import Prelude as P
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  -- Milliseconds:
  let baseStrmRate =
        case args of
          [b] -> read b
          _   -> 100

      -- A scaling parameter for how long the windows stay open:
      windowLife = 500 -- 1000
      -- windowLife = 1500

  c1 <- newIORef 1
  s1 <- S.makeInputStream $ do
          c <- readIORef (c1 :: IORef Int)
          threadDelay$ 2000 * baseStrmRate
          writeIORef c1 (c+1)
          if c >= (20 * windowLife) `quot` baseStrmRate
           then return Nothing
           else return$ Just$ B.pack$ show (100 * c) ++"\n"

  strmChan <- newChan
  writeChan strmChan (Just ("Strm-0",s1))

  -- Add a new stream every 3 seconds, they each expire after 5.
  ----------------------------------------
  forkIO $ do
    forM_ [1.. 3] $ \ix -> do
      dbgLogLn "[strmsrc] Waiting before spawning additional strm..."
      threadDelay$ 3000 * windowLife
      c2 <- newIORef 1
      sN <- S.makeInputStream $ do
              c <- readIORef (c2 :: IORef Int)
              threadDelay$ 1000 * baseStrmRate
              writeIORef c2 (c+1)
              if c >= (8 * windowLife) `quot` baseStrmRate
               then return Nothing
               else return$ Just$ B.pack$ show ix ++":"++ show (10 * c) ++"\n"
      dbgLogLn$"[strmsrc] Spawning additional stream! "++show ix
      writeChan strmChan (Just ("strm-"++show ix, sN))
    dbgLogLn "[strmsrc] Waiting before cutting off stream source..."
    threadDelay$ 8 * 1000 * windowLife
    dbgLogLn "[strmsrc] Shutting off stream source!"
    writeChan strmChan Nothing
    dbgLogLn "[strmsrc] All done, exiting thread."
  ----------------------------------------

  strmSrc <- CS.chanToInput strmChan
  hydraPrint defaultHydraConf strmSrc
