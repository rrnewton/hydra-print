{-# LANGUAGE OverloadedStrings #-}
-- | A simple utility to multiplex of *dynamic* collection of text streams.  As the
-- number of streams varies, the multiplexing of the terminal output does too.

-- module UI.MultiPipe where
module Main where

import Data.IORef
import Data.List as L 
import Data.ByteString.Char8 as B
import Prelude as P
import Control.Monad
import Control.Concurrent
import System.IO.Streams as S
import System.IO.Streams.Concurrent (concurrentMerge)
import UI.HSCurses.Curses as C
import UI.HSCurses.CursesHelper as CH

import Control.Monad.State
import Control.Monad.Reader

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The state of an active `runMultiPipe` computation.
data MPState =
  MPState {
    activeStrms :: [(InputStream ByteString, StreamHistory)],
    finishedStrms :: [StreamHistory]
    }

-- | The history of a stream.  The view (multiple windows) changes, but the underlying
-- stream histories persist.
data StreamHistory =
  StreamHistory {
    -- ^ The name of a stream might identify a client hostname, or a subprogram run,
    -- or a file being compiled.  These are displayed so as to help distinguish
    -- different windows from one another, especially as the layout changes.  
    streamName :: String, 
    -- ^ A (reverse) list of lines with the most recently produced at the head.
    revhist :: IORef [ByteString]
  }

-- | Most of the computation for this module happens in the context of a global,
-- mutable state.
type MP a = StateT (IORef MPState) IO a

--------------------------------------------------------------------------------

-- | Create a new batch of windows and display the current state of aset of
-- streamHistories.
createWindows :: [StreamHistory] -> IO ()
createWindows shists = do
--  CH.start
  
  w0 <- initScr
  (curY,curX) <- scrSize 
  let num = P.length shists
      (nX,nY) = computeTiling num
      panelDims = applyTiling (curY,curX) (nY,nX)

--  P.putStrLn$"Using Tiling: "++show panelDims; P.getLine

  
  forM_ panelDims $ \ tup@(hght,wid, posY, posX) -> do

    wMove w0 1 1
    wAddStr w0 ("Creating: "++show tup)
    -- puts ("Creating: "++show tup)
    
    w1 <- C.newWin wid hght posY posX
    wBorder w1 defaultBorder
--    wMove w1 0 0
--    wAddStr w1 "hello"    
    wRefresh w1    
    return ()


-- | If at least `n` windows are required, this computes the x-by-y tiling such that
--   `x * y >= n`.  It returns `(x,y)` where `x` represents the number of horizontal
--   tiles and `y` the number of vertical.
computeTiling :: Int -> (Int,Int)
computeTiling reqWins =
  if (n' - 1) * n' >= reqWins
  then (n' - 1, n')
  else (n', n')     
  where
    n :: Double
    n = sqrt (fromIntegral reqWins)
    n' = ceiling n 

-- | Position of a window: (Height,Width, PosY, PosX)
--   The same order as accepted by `newWin`.
type WinPos = (Int,Int,Int,Int)

-- | Split a space into a given X-by-Y tile arrangement, leaving room for borders.
applyTiling :: (Int, Int) -> (Int, Int) -> [WinPos]
applyTiling (screenY,screenX) (splitsY,splitsX) =
  [ (height,width, yStrt, xStrt)
  | (yStrt,height) <- doDim screenY splitsY
  , (xStrt,width)  <- doDim screenX splitsX ]
  where
    
    -- This is used both for horizontal and vertical, but I use horizontal
    -- terminology below:
    doDim screen splits = P.zip starts widths' 
      -- Every window must "pay" for its left border, the rightmost border is paid for
      -- globally, hence the minus-one here:                          
      where
      -- Every window must "pay" for its left border, the rightmost border is paid for
      -- globally, hence the minus-one here:        
      usable = screen - 1
      (each,left) = usable `quotRem` splitsX
      -- Here we distribute the remainder as evenly as possible:
      widths = let (hd,tl) = L.splitAt left (L.replicate splits each) in
               (L.map (+1) hd) ++ tl
      -- Starting positions are based on the raw widths not counting overlap
      starts = L.init$ L.scanl (+) 0 widths
      -- Final widths get bumped to include their rightmost border:
      widths' = L.map (+1) widths
    
--------------------------------------------------------------------------------


-- | Takes a /source/ of input streams, which may be added dynamically.  A stream
-- that joins dynamically, exits once it issues an end-of-stream.
--
-- For a static collection of input streams, just use `System.IO.Streams.fromList`
-- runMultiPipe :: InputStream (InputStream ByteString) -> IO ()
-- runMultiPipe strmSrc = do
runMultiPipe :: [InputStream ByteString] -> IO ()
runMultiPipe ls = do
  CH.start
  w <- initScr
  
  w1 <- C.newWin 10 40 10 10  
  w2 <- C.newWin 10 35 10 49
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

  Just x <- S.read s1
  Just y <- S.read s1
  P.putStrLn$"READ FIRST INPUTs: "++show (x,y)
  S.unRead y s1
  S.unRead x s1
  P.getLine
  
  runMultiPipe [s1,s2]

main = do
--  CH.start
  createWindows (L.replicate 6 undefined) 
  _ <- CH.getKey C.refresh      
  CH.end

puts s = drawLine (P.length s) s
