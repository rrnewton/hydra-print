{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

#ifndef NOTESTING
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | A simple utility to multiplex of *dynamic* collection of text streams.  As the
-- number of streams varies, the multiplexing of the terminal output does too.
module UI.HydraPrint
       (
         -- * Main Entrypoints
         createWindows, initialize,
         
         -- * Types
         
         -- * Tiling behavior
         -- computeTiling, applyTiling

#ifndef NOTESTING
         -- * Testing
         testSuite
#endif         
       )
       where

import Data.IORef
import Data.Word
import Data.List as L 
import Data.ByteString.Char8 as B
import Prelude as P hiding (unzip4) 
import Control.Monad
import Control.Concurrent
import System.IO.Streams as S
import System.IO.Streams.Concurrent (concurrentMerge)
import UI.HSCurses.CursesHelper as CH
-- import qualified UI.HSCurses.Curses as C
-- import UI.HSCurses.Curses (wMove, defaultBorder)
import UI.HSCurses.Curses as C hiding (s1,s3,tl,ls)

import Control.Monad.State
import Control.Monad.Reader

import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

#ifndef NOTESTING
import Test.QuickCheck hiding (NonEmpty)
import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.Framework  (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
#endif

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The state of an active `runMultiPipe` computation.
data MPState =
  MPState
  {
    activeStrms :: [(InputStream ByteString, WindowWidget)],
    finishedStrms :: [StreamHistory]
  }

-- | All the state for a widget, that persists beyond the creation and destruction of
-- individual NCurses windows (and even the reinitialization of the whole system).
--
-- These text widgets currently do NOT support line wrapping.  They crop to the right
-- and at the bottom.
data WindowWidget =
  WindowWidget
  {
    -- | The current and previous text in the widget.
    hist :: StreamHistory,
    -- | Get the current size of the writable area.
    textSizeYX :: IO (Word,Word),    
    
    -- | Replace a line within the window, clearing the rest of the line if the
    -- string is too short, and cropping it if it is too long.  The `Word` argument
    -- is a zero-based index into the writable area of the window.  Drawing off the
    -- end of the window willbe ignored.
    putLine :: Word -> String -> IO ()
  }

-- | The history of a stream.  The view changes, but the underlying stream histories
-- persist.
data StreamHistory =
  StreamHistory {
    -- | The name of a stream might identify a client hostname, or a subprogram run,
    -- or a file being compiled.  These are displayed so as to help distinguish
    -- different windows from one another, especially as the layout changes.  
    streamName :: String, 
    -- | A (reverse) list of lines with the most recently produced at the head.
    revHist :: IORef [ByteString]    
  }

-- | Most of the computation for this module happens in the context of a global,
-- mutable state.
type MP a = StateT (IORef MPState) IO a

-- | Position of a window: (Height,Width, PosY, PosX)
--   The same order as accepted by `newWin`.
type WinPos = (Word,Word,Word,Word)

--------------------------------------------------------------------------------

-- | Create a new batch of NCurses windows (deleting the old ones) and display the
-- current state of a set of stream histories.
createWindows :: Word -> IO [Window]
createWindows num = do
  (curY,curX) <- scrSize
  let (nX,nY)   = computeTiling num
      panelDims = applyTiling (i2w curY, i2w curX) (nY,nX)
  
  forM (NE.toList panelDims) $ \ tup@(hght,wid, posY, posX) -> do
    w1 <- C.newWin (w2i hght) (w2i wid) (w2i posY) (w2i posX)
    wBorder w1 defaultBorder
    wMove w1 1 1
    wAddStr w1 ("Created: "++show tup)    
    wRefresh w1
    return w1

-- | Create a new, persistent scrolling text widget.
createWindowWidget :: String -> InputStream ByteString -> IO WindowWidget
createWindowWidget streamName ioStrm = do
  revHist <- newIORef []
  yRef <- newIORef 0
  xRef <- newIORef 0
  let hist = StreamHistory{streamName, revHist}
      putLine ln str = do
        error "Finish putLine"
        return ()
      textSizeYX = do
        x <- readIORef xRef
        y <- readIORef yRef
        error "textSizeYX"
      obj = WindowWidget { hist, textSizeYX, putLine }

  return obj



initialize :: IO ()
initialize = do
  _ <- leaveOk True
  _ <- cursSet CursorInvisible
  return ()

    
--------------------------------------------------------------------------------

-- | Takes a /source/ of input streams, which may be added dynamically.  A stream
-- that joins dynamically, exits once it issues an end-of-stream.
--
-- For a static collection of input streams, just use `System.IO.Streams.fromList`
--
-- `runMultiPipe` is a blocking call that doesn't return until ALL streams that
-- appear produce an end-of-stream, AND the stream-source itself reaches
-- end-of-stream.
runMultiPipe :: InputStream (InputStream ByteString) -> IO ()
runMultiPipe strmSrc = phase0 =<< S.map NewStream strmSrc
----------------------------------------PHASE0----------------------------------------  
-- Nothing to do before there is at least ONE stream...   
phase0 :: InputStream Event -> IO ()
phase0 strmSrc' = do 
  ms1 <- S.read strmSrc'
  case ms1 of
   Nothing -> return ()
   Just (NewStream s1) -> do
     s1'      <- preProcess 0 s1
     -- Next, we need a "select/epoll".  We use concurrentMerge.
     merge1 <- concurrentMerge [strmSrc', s1']
     phase1 merge1
   _ -> error "runMultiPipe: Internal error. Unexpected event."
----------------------------------------PHASE1----------------------------------------
-- Initially, we start in "cooked" (non-ncurses) mode, and stay there as long as
-- there is only one output stream.     
phase1 :: InputStream Event -> IO ()
phase1 merge1 = do 
  nxt <- S.read merge1
  case nxt of
    Nothing                          -> return ()
    Just (NewStrLine _ (StrmElt ln)) -> B.putStrLn ln >> phase1 merge1
    Just (NewStrLine _ EOS)          -> phase0 merge1
    Just (NewStream s2)              -> do
      -- Transition to the steady state.
      CH.start
      cursesEvts <- S.nullInput
      -- Warning, because the curses events go into a concurrentMerge, they will keep
      -- being read into Haskell, irrespective of what this "main" thread does.     
      merge2 <- concurrentMerge [merge1, cursesEvts]
      steadyState 1 s2 merge2
    Just (CursesKeyEvent _) -> error "Internal error.  Shouldn't see Curses event here."

----------------------------------------PHASE3----------------------------------------
-- Re-enter this loop every time there is a new stream.
steadyState :: StreamID -> InputStream ByteString -> InputStream Event -> IO ()
steadyState sidCnt newStrm merged = do
  newStrm' <- preProcess sidCnt newStrm
  merged'  <- concurrentMerge [merged, newStrm']
  nxt      <- S.read merged'
  case nxt of
    Nothing -> return ()
    Just (NewStream s2)              -> steadyState (sidCnt+1) s2 merged'
    Just (NewStrLine _ (StrmElt ln)) -> do
      error "FINISHME - display a line"
    Just (NewStrLine _ EOS)          -> do
      error "FINISHME - Retile and redraw..."
    Just (CursesKeyEvent key) -> do
      case key of
        KeyResize -> do           
         C.endWin
         C.update
--           reCreate
--           dispAll 3$ "RESIZING! "
--           loop (i+1)

--   let ref = do wRefresh w1; wRefresh w2
--    wMove w1 (2+(i `mod` 6)) (3+i)
--    wAddStr w1 $"ERGH "++show i

-- Helper: import a bytestring into our system.
preProcess :: StreamID -> InputStream ByteString -> IO (InputStream Event)
preProcess id s = do
  s'  <- S.lines s
  s'' <- liftStream s'
  S.map (NewStrLine id) s''



type StreamID = Word64


-- | There are three relevant kinds of events inside our inner loop.
data Event = NewStream (InputStream ByteString)
           | NewStrLine {-# UNPACK #-} !StreamID (Lifted ByteString)
           | CursesKeyEvent Key
--  deriving (Show,Eq,Read,Ord)
           
--------------------------------------------------------------------------------
-- Tiling behavior
--------------------------------------------------------------------------------    

-- | If at least `n` windows are required, this computes the x-by-y tiling such that
--   `x * y >= n`.  It returns `(x,y)` where `x` represents the number of horizontal
--   tiles and `y` the number of vertical.
computeTiling :: Word -> (Word,Word)
computeTiling reqWins =
  if   (n' - 1) * n' >= reqWins
  then (n' - 1, n')
  else (n', n')     
  where
    n :: Double
    n = sqrt (fromIntegral reqWins)
    n' = ceiling n 

-- | Split a space into a given X-by-Y tile arrangement, leaving room for borders.
applyTiling :: (Word, Word) -> (Word, Word) -> NonEmpty WinPos
applyTiling _ a2@(splitsY,splitsX)
  | splitsX < 1 || splitsY < 1 =
    error$"applyTiling: cannot split ZERO ways in either dimension: "++show(a2)
applyTiling (screenY,screenX) (splitsY,splitsX) = NE.fromList$ 
  [ (height,width, yStrt, xStrt)
  | (yStrt,height) <- doDim screenY splitsY
  , (xStrt,width)  <- doDim screenX splitsX ]
  where
    -- This is used both for horizontal and vertical, but I use horizontal
    -- terminology below:
    doDim :: Word -> Word -> [(Word,Word)]
    doDim screen splits = P.zip starts widths' 
      -- Every window must "pay" for its left border, the rightmost border is paid for
      -- globally, hence the minus-one here:                          
      where
      -- Every window must "pay" for its left border, the rightmost border is paid for
      -- globally, hence the minus-one here:        
      usable = screen - 1
      (each,left) = usable `quotRem` splits
      -- Here we distribute the remainder as evenly as possible:
      widths = let (hd,tl) = L.splitAt (fromIntegral left)
                             (L.replicate (w2i splits) each) in
               (L.map (+1) hd) ++ tl
      -- Starting positions are based on the raw widths not counting overlap
      starts = L.init$ L.scanl (+) 0 widths
      -- Final widths get bumped to include their rightmost border:
      widths' = L.map (+1) widths

--------------------------------------------------------------------------------    

test :: IO ()
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
  _ <- P.getLine
  
  runMultiPipe =<< S.fromList [s1,s2]

puts :: String -> IO ()
puts s = drawLine (P.length s) s

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- Returns (inclusive,exclusive) bounds.
boundingBox :: NE.NonEmpty WinPos -> WinPos
boundingBox wps = (maxY,maxX, minY,minX)
  where
    minY = F.foldl1 min ys
    minX = F.foldl1 min xs
    maxY = F.foldl1 max (NE.zipWith (+) hs ys)
    maxX = F.foldl1 max (NE.zipWith (+) ws xs)
    (hs,ws,ys,xs) = UI.HydraPrint.unzip4 wps

t0 :: NonEmpty WinPos
t0 = applyTiling (48,173) (3,2)

t1 :: WinPos
t1 = boundingBox t0

#ifndef NOTESTING

case_t0 :: Assertion
case_t0 = assertBool "Basic tiling example"
          (noprop_goodtiling (48,173) (3,2))

-- DISABLING: This hangs under quickcheck.
noprop_goodtiling :: (Word,Word) -> (Word,Word) -> Bool
noprop_goodtiling (y,x) (splitY,splitX) =
  if (y>0 && x>0 && splitY>0 && splitX > 0) 
  then (h == y && w == x)
  else True -- Trivially.
 where
   tiles     = applyTiling (y,x) (splitY,splitX)
   (h,w,0,0) = boundingBox tiles 

testSuite :: Test
testSuite = $(testGroupGenerator)
            
instance (Arbitrary a) => Arbitrary (NE.NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
  shrink x = NE.fromList <$> shrink (NE.toList x)

#endif

-----------------------------------------
-- Missing bits that should be elsewhere:
-----------------------------------------

-- | 'unzip4' for `NonEmpty` lists
unzip4 :: Functor f => f (a,b,c,d) -> (f a, f b, f c, f d)
unzip4 xs = ((\(x,_,_,_) -> x) <$> xs,
             (\(_,x,_,_) -> x) <$> xs,
             (\(_,_,x,_) -> x) <$> xs,
             (\(_,_,_,x) -> x) <$> xs)


-- | Annoyingly, many libraries use Int where negative values are not allowed.
i2w :: Int -> Word
i2w i | i < 0 = error$"i2w: Cannot convert negative Int to Word: "++show i
i2w i = fromIntegral i

w2i :: Word -> Int
w2i w = if i < 0
        then error$"w2i: Cannot convert Word to Int: "++show w
        else i
  where 
  i = fromIntegral w

-- | io-streams do not by default support tee or fan-out, because all reads "pop".
--   This broadcasts an InputStream to two new InputStreams, each of which will
--   receive every element.
--
--   One useful application of this is creating additional copies of a stream before
--    it is connected to a downstream operator like
--    `System.IO.Streams.Concurrent.concurrentMerge`.  Once that connection happens,
--    even if one stops demanding output from the `concurrentMerge`, one cannot know
--    how many elements it popped from the original input stream already.
dupStream :: InputStream a -> IO (InputStream a, InputStream a)
dupStream = error "dupStream unimplemented"


-- | This makes the EOS into an explicit, penultimate message. This way it survives
-- `concurrentMerge`.
liftStream :: InputStream a -> IO (InputStream (Lifted a))
liftStream =  error "liftStream"

-- | Datatype for reifying end-of-stream.
data Lifted a = EOS | StrmElt a
