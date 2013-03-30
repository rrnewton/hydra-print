{-# LANGUAGE OverloadedStrings #-}
-- | A simple utility to multiplex of *dynamic* collection of text streams.  As the
-- number of streams varies, the multiplexing of the terminal output does too.

-- module UI.MultiPipe where
module Main where

import Data.ByteString.Char8 as B
import Prelude as P
import Control.Monad
import Control.Concurrent
import System.IO.Streams as S
import System.IO.Streams.Concurrent (concurrentMerge)
import UI.HSCurses.Curses as C
import UI.HSCurses.CursesHelper as CH

-- | Takes a /source/ of input streams, which may be added dynamically.  A stream
-- that joins dynamically, exits once it issues an end-of-stream.
--
-- For a static collection of input streams, just use `System.IO.Streams.fromList`
-- runMultiPipe :: InputStream (InputStream ByteString) -> IO ()
-- runMultiPipe strmSrc = do

runMultiPipe :: [InputStream ByteString] -> IO ()
runMultiPipe ls = do

  m <- concurrentMerge ls

  CH.start
  w <- initScr
  
  w1 <- C.newWin 10 40 10 10  
  w2 <- C.newWin 10 35 11 51
  wBorder w1 defaultBorder
  wBorder w2 defaultBorder

  -- (y,x) <- getYX w1
  -- move (y+1) (x+1)
  wMove w1 1 1
  wAddStr w1 "hello"
  wRefresh w1
  wRefresh w2
--  refresh
--  _ <- CH.getKey C.refresh

  let [s1,s2] = ls
  s1' <- S.map Left s1
  s2' <- S.map Right s2
  s3 <- concurrentMerge [s1', s2']

  x <- S.read s3
  wMove w1 2 1  
  wAddStr w1$ "first: "++show x
  wRefresh w1

  let
      f i x = do
        wMove w2 (1 + i`mod`8) 2
        wAddStr  w2 $"stream "++show x
        wRefresh w2
    
      f _ (Left str) = do
                        wMove w1 1 1
                        wAddStr w1 (P.take 38 (B.unpack str))
                        wRefresh w1
                        return ()
      f _ (Right str) = do 
                        wMove w2 1 1
                        wAddStr w2 (P.take 33 (B.unpack str))
                        wRefresh w2
                        return ()                        
  -- Attach a side effect:
--  S.mapM_ f s3 
  
  let ref = do wRefresh w1; wRefresh w2
  forM_ [1..8] $ \i -> do 
    wMove w1 (2+(i `mod` 6)) (3+i)
    wAddStr w1 $"ERGH "++show i
    wRefresh w1    
    x <- S.read s3
    case x of
      (Just y) -> f i y
      Nothing -> do
        -- move 2 2
        -- wAddStr w "DONE"
        -- refresh
        wMove    w2 1 3
        wAddStr  w2 $"DONE "++show i
        wRefresh w2        
    ref
    threadDelay$ 500 * 1000
    return ()
  _ <- CH.getKey ref -- C.refresh    
  CH.end

test = do
  -- Weird, what will happen:
--  inlines <- S.lines S.stdin
  s1 <- S.fromList ["hi","there","you","blah"]
  s2 <- S.fromList ["aaa","bbb","ccc"]

  -- x <- S.read s1
  -- y <- S.read s1  
  -- P.putStrLn$"READ FIRST INPUTs: "++show (x,y)
  -- P.getLine
  
  runMultiPipe [s1,s2]

main = test

puts s = drawLine (P.length s) s
