{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

#define UPDATE_ALL_ALWAYS

#ifndef NOTESTING
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | A simple utility to multiplex of *dynamic* collection of text streams.  As the
-- number of streams varies, the multiplexing of the terminal output does too.
module UI.HydraPrint
       (
         -- * hydraPrint and friends
         -- hydraPrint, hydraPrintStatic,
         hydraPrintInterleaved,
       )
       where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Prelude as P
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO, MonadIO)

import qualified System.IO.Streams as S
import System.IO.Streams (InputStream)
import System.IO.Streams.Concurrent (chanToOutput)

import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getEnvironment)

dbg :: Bool
dbg = case P.lookup "HYDRA_DEBUG" theEnv of 
        Nothing      -> False
        Just ""      -> False
        Just "0"     -> False
        Just "False" -> False
        Just  _      -> True

theEnv :: [(String, String)]
theEnv = unsafePerformIO$ getEnvironment

io :: MonadIO m => IO a -> m a
io x = liftIO x

--------------------------------------------------------------------------------

-- -- For blanking out inactive windows.
-- blankChar :: Char
-- blankChar = ' '

-- dbgLn :: String -> IO ()
-- dbgLn s = when dbg$ 
--   do dbgLogLn s 
--      P.putStrLn s

-- dbgLogLn :: String -> IO ()
-- dbgLogLn s = when dbg$ 
--  do B.hPutStrLn dbgLog (B.pack s)
--     hFlush dbgLog
  
-- dbgLog :: Handle
-- dbgLog = unsafePerformIO $ do
--   let file = "/tmp/hydraprint_debug.log"
--   removeIfExists file
--   openFile file WriteMode

--------------------------------------------------------------------------------

-- | A simple and robust alternative that simply interleaves the lines distinguishing
--   them based on prefix, color, or both.  
hydraPrintInterleaved :: InputStream (String, InputStream ByteString) -> IO ()
hydraPrintInterleaved srcs = do
  nexus <- newChan
  outStrm <- chanToOutput nexus
  let adder = do
        x <- S.read srcs
        case x of
          Nothing   -> writeChan nexus Nothing
          Just (name,src) -> do 
            src' <- S.map (\x -> B.concat ["[", B.pack name, "] ",x]) src
            -- writeChan nexus (Just src')
            _ <- forkIO $ S.supply src' outStrm
            adder
      reader = do
        x <- readChan nexus
        case x of
          Nothing -> return ()
          Just s  -> B.putStrLn s
  reader  

--------------------------------------------------------------------------------
