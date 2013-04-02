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
         hydraPrint
         -- createWindows, initialize,
         
         -- * Types
         
         -- * Tiling behavior
         -- computeTiling, applyTiling

#ifndef NOTESTING
         -- * Testing
         , testSuite
#endif         
       )
       where

import Data.IORef
import Data.Word
import Data.Map  as M
import Data.List as L 
import Data.ByteString.Char8 as B
import Prelude as P hiding (unzip4) 
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.IO.Streams as S
import System.IO.Streams.Concurrent (concurrentMerge)
import UI.HSCurses.CursesHelper as CH
-- import qualified UI.HSCurses.Curses as C
-- import UI.HSCurses.Curses (wMove, defaultBorder)
import UI.HSCurses.Curses as C hiding (s1,s3,tl,ls)

-- import Control.Monad.State
-- import Control.Monad.Reader

import System.IO (hFlush, hPutStrLn, stderr, openFile, IOMode(WriteMode), Handle)
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Error (isDoesNotExistError)
import System.Directory

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

-- | The state of an active `hydraPrint` computation.
data MPState =
  MPState
  {
    activeStrms :: M.Map StreamID WindowWidget,
-- [(InputStream ByteString, WindowWidget)]
    finishedStrms :: [StreamHistory],

    -- | All active windows.  Need to be explicitly deleted.
    windows :: [CWindow]
    -- Log: TODO: could log stream create/delete events and their times.
  }

-- | All the state for a widget, that persists beyond the creation and destruction of
-- individual NCurses windows (and even the reinitialization of the whole system).
--
-- These text widgets currently do NOT support line wrapping.  They crop to the right
-- and at the bottom.
data WindowWidget =
  WindowWidget
  {
    -- "Methods"
    ----------------------------------------
    -- | The current and previous text in the widget.
    hist :: StreamHistory,
    -- | Get the current size of the writable area.
    textSizeYX :: IO (Word,Word),    
    
    -- | Replace a line within the window, clearing the rest of the line if the
    -- string is too short, and cropping it if it is too long.  The `Word` argument
    -- is a zero-based index into the writable area of the window.  Drawing off the
    -- end of the window will be ignored.
    putLine :: ByteString -> IO (),
    -- putLineN :: Word -> String -> IO ()

    setWin :: CWindow -> IO (),
    
    destroy :: IO (),
    
    -- "Private" state:    
    ----------------------------------------
    winRef :: IORef CWindow
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
-- type MP a = StateT (IORef MPState) IO a

-- | Position of a window: (Height,Width, PosY, PosX)
--   The same order as accepted by `newWin`.
type WinPos = (Word,Word,Word,Word)

-- | Along with the raw pointer, remember the size at which a window was created:
data CWindow = CWindow C.Window WinPos 

--------------------------------------------------------------------------------

-- | Create a new batch of NCurses windows (deleting the old ones) and display the
-- current state of a set of stream histories.
createWindows :: [String] -> Word -> IO [CWindow]
createWindows names num = do
  (curY,curX) <- scrSize
  let (nX,nY)   = computeTiling num
      panelDims = applyTiling (i2w curY, i2w curX) (nY,nX)
  forM (P.zip names (NE.toList panelDims)) $ 
   \ (name, tup@(hght,wid, posY, posX)) -> do
    w1 <- C.newWin (w2i hght) (w2i wid) (w2i posY) (w2i posX)
    wMove w1 1 2
    let msg = ("CreatedWindow: "++show w1++" at "++show tup++", name "++name)
    dbgLogLn msg
    wAddStr w1 msg
    wBorder w1 defaultBorder
    wRefresh w1
    return (CWindow w1 tup)

-- How many characters to avoid at the edges of window, for the border:
borderTop :: Word
borderTop = 2
borderBottom :: Word
borderBottom = 1
borderLeft :: Word
borderLeft = 1
borderRight :: Word
borderRight = 1

-- | Create a new, persistent scrolling text widget.
createWindowWidget :: String -> IO WindowWidget
createWindowWidget streamName = do -- ioStrm
  revHist <- newIORef []
  winRef  <- newIORef (error "winRef field uninialized.  Call setWin.")
  let hist = StreamHistory{streamName, revHist}
      putLine bstr = do
        CWindow wp (y,x,_,_) <- readIORef winRef
        oldhist <- readIORef revHist
        let msg     = bstr `B.append` (B.pack (" <line "++show (P.length oldhist)++" y "++show y++">"))
        let newhist = msg : oldhist
        writeIORef revHist newhist    
        let y'    = y - borderTop - borderBottom
            shown = P.take (w2i y') newhist
            padY  = y' - i2w(P.length shown)
        forM_ (P.zip [1..] shown) $ \ (ind,oneline) -> do
          wMove wp (w2i (y - borderBottom - ind - padY)) (w2i borderLeft)
          let padded = oneline `B.append`
                       B.replicate (w2i x - B.length oneline) ' '
              cropped = B.take (w2i (x - borderLeft - borderRight)) padded
          wAddStr wp (B.unpack cropped)
        wBorder wp defaultBorder
        -- For now refresh the window on every line written..
        wRefresh wp -- TODO: use wnoutrefresh instead
        -- TODO: Do we need to refresh all the OTHER windows to avoid problems!?
--        C.update
        
      textSizeYX = do
        CWindow _ (y,x,_,_) <- readIORef winRef
        return (y,x)
      destroy = error "implement destroy"

      setWin cwin@(CWindow wp _) = do
        writeIORef winRef cwin
        -- wBorder wp defaultBorder
        -- wRefresh wp  -- TODO: use wnoutrefresh instead
        -- C.update
        return ()
      
      obj = WindowWidget { hist, textSizeYX, putLine,
                           destroy, setWin, winRef }
  return obj

initialize :: IO ()
initialize = do
  _ <- leaveOk True
  _ <- cursSet CursorInvisible
  return ()

dbgLn s = do dbgLogLn s 
             P.putStrLn s

dbgLogLn s = do 
  B.hPutStrLn dbgLog (B.pack s)
  hFlush dbgLog
  
dbgLog :: Handle
dbgLog = unsafePerformIO $ do
  let file = "/tmp/hydraprint.log"
  removeIfExists file
  openFile file WriteMode

--------------------------------------------------------------------------------

-- | Takes a /source/ of input streams, which may be added dynamically.  A stream
-- that joins dynamically, exits once it issues an end-of-stream.
--
-- For a static collection of input streams, just use `System.IO.Streams.fromList`
--
-- `hydraPrint` is a blocking call that doesn't return until ALL streams that
-- appear produce an end-of-stream, AND the stream-source itself reaches
-- end-of-stream.
hydraPrint :: InputStream (String, InputStream ByteString) -> IO ()
hydraPrint strmSrc = phase0 =<< S.map NewStream strmSrc
----------------------------------------PHASE0----------------------------------------  
-- Nothing to do before there is at least ONE stream...   
phase0 :: InputStream Event -> IO ()
phase0 strmSrc' = do 
  dbgLn $ "phase0: blocking for event."
  ms1 <- S.read strmSrc'
  case ms1 of
   Nothing -> do 
     dbgLn $ "phase0: stream ended"
     return ()
   Just (NewStream (s1name,s1)) -> do
     dbgLn $ "phase0: new (first) stream! ("++s1name++")  Moving to phase1."
     s1'      <- preProcess 0 s1
     -- Next, we need a "select/epoll".  We use concurrentMerge.
     merge1 <- concurrentMerge [strmSrc', s1']
     phase1 s1name merge1
   _ -> error "hydraPrint: Internal error. Unexpected event."
----------------------------------------PHASE1----------------------------------------
-- Initially, we start in "cooked" (non-ncurses) mode, and stay there as long as
-- there is only one output stream.     
phase1 :: String -> InputStream Event -> IO ()
phase1 s1name merge1 = do 
  dbgLn $ "phase1: blocking for event."
  nxt <- S.read merge1
  case nxt of
    Nothing                          -> do
      dbgLn $ "Streams ended in phase1!"
      return ()
    Just (NewStrLine _ (StrmElt ln)) -> do
      B.putStrLn ln
      phase1 s1name merge1
    Just (NewStrLine sid EOS)          -> do 
      dbgLn $ "Got stream EOS! ID "++show sid
      phase0 merge1
    Just (NewStream (s2name,s2))     -> do
      dbgLn $ "Got newStream! "++s2name++".  Transition to steady state..." -- (press enter)
--      P.getLine

      -- Transition to the steady state.
      CH.start
--      cursesEvts <- S.nullInput
      cursesEvts <- S.makeInputStream $ fmap (Just . CursesKeyEvent) C.getCh 
      
      -- Warning, because the curses events go into a concurrentMerge, they will keep
      -- being read into Haskell, irrespective of what this "main" thread does.     
      merge2 <- concurrentMerge [merge1, cursesEvts]
      wid0   <- createWindowWidget s1name
      [win0] <- createWindows [s1name] 1
      setWin wid0 win0
      let initSt = MPState { activeStrms= M.fromList [(0,wid0)],
                             finishedStrms= [],
                             windows= [win0] }
      steadyState initSt 1 (s2name,s2) merge2
    Just (CursesKeyEvent _) -> error "Internal error.  Shouldn't see Curses event here."

----------------------------------------PHASE3----------------------------------------
-- Re-enter this loop every time there is a new stream.
steadyState :: MPState -> StreamID -> (String,InputStream ByteString) -> InputStream Event -> IO ()
steadyState state0@MPState{activeStrms,windows} sidCnt (newName,newStrm) merged = do
  -- First, deal with the new stream.
  newStrm' <- preProcess sidCnt newStrm
  merged'  <- concurrentMerge [merged, newStrm']
  widg     <- createWindowWidget newName
  let active2  = M.insert sidCnt widg activeStrms
  windows2 <- reCreate active2 windows
  let state1 = state0{activeStrms=active2, windows=windows2}
  
  -- Second, enter an event loop:
  let loop mps@MPState{activeStrms, finishedStrms, windows} = do
        nxt <- S.read merged'
--        System.IO.hPutStrLn System.IO.stderr $ " [dbg] GOT EVENT: "++ show nxt
--        dbgPrnt $ " [dbg] GOT EVENT: "++ show nxt
        case nxt of
          Nothing -> return ()
          Just (NewStrLine sid (StrmElt ln)) -> do
            putLine (activeStrms!sid) ln
            loop mps
          Just (NewStrLine sid EOS)          -> do
#if 1
            dbgPrnt $ " [dbg] Stream ID "++ show sid++" got end-of-stream "
--            destroy (activeStrms!sid)
            let active' = M.delete sid activeStrms
            windows' <- reCreate active' windows
            loop mps{ activeStrms  = active',
                      finishedStrms= hist (activeStrms!sid) : finishedStrms,
                      windows      = windows' }
#else
            loop mps
#endif
          Just (NewStream (s2name,s2))       -> do
--            dbgPrnt $ " [dbg] NEW stream: "++ show s2name
            steadyState mps (sidCnt+1) (s2name,s2) merged'
          Just (CursesKeyEvent key) -> do
            case key of
              KeyChar 'q' -> do
                CH.end
                dbgLn " [dbg] NCurses finished." 
              KeyResize -> do           
               C.endWin
               C.update
               windows' <- reCreate activeStrms windows
               loop mps{windows=windows'}
              _ -> do dbgPrnt $ " [dbg] CURSES Key event: "++show key
                      loop mps
  loop state1
 where
   dbgPrnt s = do 
     dbgLogLn s
     putLine (P.head$ M.elems activeStrms) (B.pack s)
   reCreate active' oldWins = do
      let names = P.map (streamName . hist) $ M.elems active'
      ws <- createWindows names (fromIntegral(M.size active'))
      -- Guaranteed to be in ascending key order, which in our case is
      -- first-stream-to-join first.
      forM_ (P.zip ws (M.assocs active')) $ \ (win,(sid,wid)) -> do
        setWin wid win 
      -- Actually delete the old windows:
      forM_ oldWins (\ (CWindow w _) -> delWin w)
      dbgPrnt$ " [dbg] Deleted windows: "++show (P.map (\ (CWindow w _) -> w) oldWins)
               ++ " created "++ show(P.map (\ (CWindow w _) -> w) ws)
      return ws
      
-- Helper: import a bytestring into our system.
preProcess :: StreamID -> InputStream ByteString -> IO (InputStream Event)
preProcess id s = do
  s'  <- S.lines s
  s'' <- liftStream s'
  S.map (NewStrLine id) s''



type StreamID = Word


-- | There are three relevant kinds of events inside our inner loop.
data Event = NewStream (String, InputStream ByteString)
           | NewStrLine {-# UNPACK #-} !StreamID (Lifted ByteString)
           | CursesKeyEvent Key
--  deriving (Show,Eq,Read,Ord)

instance Show Event where
  show (NewStream (s,_)) = "NewStream "++s
  show (NewStrLine sid str) = "NewStrLine "++show sid++" "++show str
  show (CursesKeyEvent k) = "CursesKeyEvent "++ show k

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
  
  hydraPrint =<< S.fromList [("s1",s1),("s2",s2)]

puts :: String -> IO ()
puts s = drawLine (P.length s) s

--------------------------------------------------------------------------------
-- Missing bits that should be elsewhere:
--------------------------------------------------------------------------------

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


-- | This makes the EOS into an /explicit/, penultimate message. This way it survives
-- `concurrentMerge`.
liftStream :: InputStream a -> IO (InputStream (Lifted a))
liftStream ins =
  do flag <- newIORef True
     makeInputStream $ do
       x   <- S.read ins
       flg <- readIORef flag
       case x of
         Just y -> return (Just (StrmElt y))
         Nothing | flg -> do writeIORef flag False
                             return (Just EOS)
                 | otherwise -> return Nothing
         
-- | Datatype for reifying end-of-stream.
data Lifted a = EOS | StrmElt a
  deriving (Show,Eq,Read,Ord)
                      
  

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

case_lift :: Assertion
case_lift = do 
  x <- liftStream =<< S.fromList [1..4]
  y <- S.toList x
  assertEqual "eq" [StrmElt 1,StrmElt 2,StrmElt 3,StrmElt 4,EOS] y 

#endif

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
