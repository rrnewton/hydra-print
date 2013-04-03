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

         -- TEMPORARY:
         , dbgLogLn
       )
       where

import Data.IORef
import Data.Word
import Data.Char (ord)
import Data.Map  as M
import Data.List as L 
import Data.ByteString.Char8 as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Prelude as P hiding (unzip4) 
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Exception
import Foreign.C.String (withCAStringLen)
-- import Foreign.Ptr      (castPtr, Ptr)
import System.IO.Streams as S
import System.IO.Streams.Concurrent (concurrentMerge)

#if 0 
import UI.HSCurses.CursesHelper as CH
import UI.HSCurses.Curses as C hiding (s1,s3,tl,ls)
#else
import UI.NCurses hiding (Event)
import qualified UI.NCurses as C
#endif

-- import Control.Monad.State
-- import Control.Monad.Reader

import System.IO (hFlush, hPutStrLn, stderr, openFile, IOMode(WriteMode), Handle)
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Error (isDoesNotExistError)
import System.Directory (removeFile)
import System.Environment (getEnvironment)

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

dbg :: Bool
dbg = case P.lookup "DEBUG" theEnv of 
        Nothing      -> False
        Just ""      -> False
        Just "0"     -> False
        Just "False" -> False
        Just  _      -> True

theEnv = unsafePerformIO$ getEnvironment

io x = liftIO x

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
    putLine :: ByteString -> Curses (),
    -- putLineN :: Word -> String -> IO ()

    setWin :: CWindow -> Curses (),
    
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
  deriving Show

--------------------------------------------------------------------------------

-- | Create a new batch of NCurses windows (deleting the old ones) and display the
-- current state of a set of stream histories.
createWindows :: [String] -> Word -> Curses ([CWindow],Word,Word)
createWindows names num = do
  (curY,curX) <- screenSize
  let (nX,nY)   = computeTiling num
      panelDims = applyTiling (i2w curY, i2w curX) (nY,nX)
  ws <- forM (P.zip names (NE.toList panelDims)) $ 
   \ (name, tup@(hght,wid, posY, posX)) -> do
    w1 <- newWindow (w2i hght) (w2i wid) (w2i posY) (w2i posX)
    
    let msg = ("CreatedWindow:  at "++show tup++", name "++name)   
    -- when dbg $ do dbgLogLn msg
    --               moveCursor 1 2
    --               drawString msg
    --               drawBox Nothing Nothing
    let cwin = CWindow w1 tup
    updateWindow w1$ drawNamedBorder name cwin
    return cwin  
  return (ws,nX,nY)

-- For blanking out inactive windows.
blankChar :: Char
blankChar = ' '

{-
-- | Use the simple method of writing blanks to clear.  Convention: overwrite the
--   lower & right borders, but not the top/left.
clearWindow :: CWindow -> IO ()
clearWindow (CWindow wp (hght,wid,_,_)) = do 
  let 
      width' = wid - borderLeft -- - borderRight
      blank  = P.replicate (w2i width') blankChar
  forM_ [borderTop .. hght - borderBottom - 1 ] $ \ yind -> do
--  forM_ [borderTop .. hght - 1] $ \ yind -> do
--  forM_ [hght - 1 .. hght - 1] $ \ yind -> do          
    wMove wp (w2i yind) (w2i borderLeft)
    wAddStr wp blank
--    blit wp blank
  writeToCorner wp (w2i$ hght-1) (w2i borderLeft) blank 
  wnoutRefresh wp  
-}

-- Nah, this won't do it... odd that there's no clear or fill function?

#if 0
clearWindow :: CWindow -> Curses ()
clearWindow (CWindow wp _) = do
  updateWindow wp $ 
    setBackground (Glyph ' ' [])
#else
clearWindow :: CWindow -> Curses ()
clearWindow (CWindow wp (hght,wid,_,_)) = updateWindow wp $ do
  let 
      width' = wid - borderLeft -- - borderRight
      blank  = P.replicate (w2i width') blankChar
  io$ evaluate hght
  io$ evaluate wid
  io$ evaluate wp
  forM_ [borderTop .. hght - borderBottom - 1 ] $ \ yind -> do
    moveCursor (w2i yind) (w2i borderLeft)
    drawString blank
--    drawString "!"
    return ()
  writeToCorner (w2i$ hght-1) (w2i borderLeft) blank 
--  wnoutRefresh wp  
#endif

-- | Write out a string that goes all the way to the bottom/right corner.
writeToCorner :: Int -> Int -> String -> Update ()
writeToCorner y x str = do
  let len = P.length str
  moveCursor  (fromIntegral y) (fromIntegral x)
  drawString  (P.init str)
  -- Uh oh!  'ncurses' doesn't expose winsch either.  SKIP IT for now:  
--  moveCursor  y (len-1)
--  throwIfErr_ "winsch" $ winsch wp (fromIntegral$ ord$ P.last str)
  return ()


{-
-- | Write out a string that goes all the way to the bottom/right corner.
writeToCorner :: Window -> Int -> Int -> String -> IO ()
writeToCorner wp y x str = do
  -- I'm getting a Curses error if I try to write the lower-right corner character!?9
  let len = P.length str
  wMove   wp y x
  wAddStr wp (P.init str)
  wMove   wp y (len-1)
  ------------
  -- Hack: Even waddch directly is throwing an error in the lower right
  -- corner... Hmm.  A hack is to ignore the error code.
  --throwIfErr_ "waddch" $
  -- waddch wp (fromIntegral$ ord blankChar) -- Don't advance the cursor.
  ------------
  -- This seems to be a known issue:
  -- http://lists.gnu.org/archive/html/bug-ncurses/2007-09/msg00002.html
  throwIfErr_ "winsch" $ winsch wp (fromIntegral$ ord$ P.last str)
  return ()

wAddCh :: Window -> Char -> IO ()
wAddCh wp ch = throwIfErr_ "waddch" $ waddch wp (fromIntegral$ ord ch)

wInsCh :: Window -> Char -> IO ()
wInsCh wp ch = throwIfErr_ "winsch" $ winsch wp (fromIntegral$ ord ch)

blit :: Window -> String -> IO ()
blit wp s =
  -- Ignore all non-ascii at the moment:
  withCAStringLen s $ \ (s',len) -> 
    throwIfErr_ "waddchnstr" $ waddchnstr wp s' (fromIntegral len)

blitB :: Window -> ByteString -> IO ()
blitB wp s =
  -- Ignore all non-ascii at the moment:
--  B.useAsCStringLen s $ \ (s',len) ->
  unsafeUseAsCStringLen s $ \ (s',len) ->
    throwIfErr_ "waddchnstr" $ waddchnstr wp s' (fromIntegral len)
  
-- This SHOULDNT be necessary, but I'm having problems with blanking and blinking
-- otherwise.
redrawAll :: [CWindow] -> IO ()
redrawAll wins = do
--  forM_ wins $ \ (CWindow wp _) -> do
--    wBorder  wp defaultBorder
--    wRefresh wp     -- TODO: use wnoutrefresh instead
--    wnoutRefresh wp 
  C.update
-}
redrawAll :: [CWindow] -> Curses ()
redrawAll _ = C.render


-- How many characters to avoid at the edges of window, for the border:
borderTop :: Word
borderTop = if dbg then 2 else 1
-- borderTop = 2 
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
        cwin@(CWindow wp (y,x,_,_)) <- io$ readIORef winRef
        updateWindow wp $ do
         oldhist <- io$ readIORef revHist
         let msg     = bstr `B.append` (B.pack (" <line "++show (P.length oldhist)++" y "++show y++">"))
         let newhist = msg : oldhist
         io$ writeIORef revHist newhist    
         let y'    = y - borderTop - borderBottom
             shown = P.take (w2i y') newhist
             padY  = y' - i2w(P.length shown)
         forM_ (P.zip [1..] shown) $ \ (ind,oneline) -> do
           moveCursor (w2i (y - borderBottom - ind - padY)) (w2i borderLeft)
           let padded = oneline `B.append`
                        B.replicate (w2i x - B.length oneline) ' '
               cropped = B.take (w2i (x - borderLeft - borderRight)) padded
           drawString (B.unpack cropped)
           ------ Line is put! ----
           drawNamedBorder streamName cwin
        
      textSizeYX = do
        CWindow _ (y,x,_,_) <- readIORef winRef
        return (y,x)

      setWin cwin@(CWindow wp _) = do
        io$ writeIORef winRef cwin
        updateWindow wp $ 
          drawNamedBorder streamName cwin
        return ()
      
      obj = WindowWidget { hist, textSizeYX, putLine,
                           setWin, winRef }
  return obj

drawNamedBorder :: String -> CWindow -> Update ()
drawNamedBorder name (CWindow wp (hght,wid,y,_)) = do
--  wBorder wp defaultBorder
  drawBox Nothing Nothing
  let isTop = (y == 0)
      -- name' = llCorner : name ++ [lrCorner]
      name' = "[" ++ name ++ "]"
      mid  = wid `quot` 2
      strt = w2i mid - (fromIntegral (P.length name' `quot` 2))
  if isTop then
     moveCursor 0 strt
   else
     moveCursor (w2i$ hght-1) strt
  drawString name'

dbgLn :: String -> IO ()
dbgLn s = when dbg$ 
  do dbgLogLn s 
     P.putStrLn s

dbgLogLn :: String -> IO ()
dbgLogLn s = when dbg$ 
 do B.hPutStrLn dbgLog (B.pack s)
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
      -- Transition to the steady state:
      runCurses $ do
        setCursorMode CursorInvisible
        -- _ <- leaveOk True

        -- PROBLEM: We don't want to do a nested runCurses here...
        -- cursesEvts <- io$ S.makeInputStream $ fmap (Just . CursesKeyEvent)
        --                                       (C.getEvent defaultWindow Nothing)

        -- Warning, because the curses events go into a concurrentMerge, they will keep
        -- being read into Haskell, irrespective of what this "main" thread does.     
--        merge2 <- io$ concurrentMerge [merge1, cursesEvts]
        let merge2 = merge1
        wid0         <- io$ createWindowWidget s1name
        ([win0],_,_) <- createWindows [s1name] 1
        setWin wid0 win0
        let initSt = MPState { activeStrms= M.fromList [(0,wid0)],
                               finishedStrms= [],
                               windows= [win0] }
        redrawAll [win0]
        steadyState initSt 1 (s2name,s2) merge2
    Just (CursesKeyEvent _) -> error "Internal error.  Shouldn't see Curses event here."

----------------------------------------PHASE3----------------------------------------
-- Re-enter this loop every time there is a new stream.
steadyState :: MPState -> StreamID -> (String,InputStream ByteString) -> InputStream Event -> Curses ()
steadyState state0@MPState{activeStrms,windows} sidCnt (newName,newStrm) merged = do
  -- First, deal with the new stream.
  newStrm' <- io$ preProcess sidCnt newStrm
  merged'  <- io$ concurrentMerge [merged, newStrm']
  widg     <- io$ createWindowWidget newName
  let active2  = M.insert sidCnt widg activeStrms
  windows2 <- reCreate active2 windows
  let state1 = state0{activeStrms=active2, windows=windows2}

  -- TODO: Merge a heartbeat (timer) in here:
  
  -- Second, enter an event loop:
  let loop mps@MPState{activeStrms, finishedStrms, windows} = do
        redrawAll windows
        nxt <- io$ S.read merged'
        case nxt of
          Nothing -> return ()
          Just (NewStrLine sid (StrmElt ln)) -> do
            io$ dbgLogLn (B.unpack ln)
            putLine (activeStrms!sid) ln
            loop mps
          Just (NewStrLine sid EOS)          -> do
            io$ dbgPrnt $ " [dbg] Stream ID "++ show sid++" got end-of-stream "
--            destroy (activeStrms!sid)
            let active' = M.delete sid activeStrms

            -- Deleting always shifts down the LAST window (should improve this)
            clearWindow (P.last windows)
            -- case P.last windows of
            --   CWindow wp _ -> wclear wp
            windows' <- reCreate active' windows
            loop mps{ activeStrms  = active',
                      finishedStrms= hist (activeStrms!sid) : finishedStrms,
                      windows      = windows' }
          Just (NewStream (s2name,s2))       -> do
            io$ dbgPrnt $ " [dbg] NEW stream: "++ show s2name
            steadyState mps (sidCnt+1) (s2name,s2) merged'
          Just (CursesKeyEvent key) -> do
            case key of
{-              
              KeyChar 'q' -> do
                CH.end
                dbgLn " [dbg] NCurses finished."
              KeyChar 'p' -> do
                -- Pause until another key is hit.
                _ <- C.getCh
                loop mps
              KeyResize -> do           
               C.endWin
               C.update
               windows' <- reCreate activeStrms windows
--               redrawAll windows'
               loop mps{windows=windows'}
-}
              _ -> do io$ dbgPrnt $ " [dbg] CURSES Key event: "++show key
                      loop mps
  loop state1
 where
   dbgPrnt s = when dbg $ do 
     dbgLogLn s
     -- AGAIN, a problem with the Curses monad here... we want to call this before curses is initialized:
     -- putLine (P.head$ M.elems activeStrms) (B.pack s)
     -- redrawAll windows
     
   reCreate active' oldWins = do
      let names = P.map (streamName . hist) $ M.elems active'
          numactive = fromIntegral (M.size active')
      (ws,numHoriz,numVert) <- createWindows names numactive
      -- Guaranteed to be in ascending key order, which in our case is
      -- first-stream-to-join first.
      forM_ (P.zip ws (M.assocs active')) $ \ (win,(sid,wid)) -> do
        setWin wid win 
      -- Actually delete the old windows:
--      forM_ oldWins (\ (CWindow w _) -> delWin w)
      forM_ oldWins (\ (CWindow w _) -> closeWindow w)
      -- io$ dbgPrnt$ " [dbg] Deleted windows: "++show (P.map (\ (CWindow w _) -> w) oldWins)
      --          ++ " created "++ show(P.map (\ (CWindow w _) -> w) ws)
      ----------------------------------------
      -- Erase the bit of border which may be unused:
      (nLines,nCols) <- screenSize
      dummies <- case P.last ws of
        CWindow wp (hght,wid,y,x) ->
          let lastCol = w2i$ x + wid - 1  in
          if (lastCol < nCols - 1) then do
            ----------- First wipe the horizontal lower border:
            let startX     = lastCol+1
                remainingX = fromIntegral$ nCols - startX 
            dummy <- newWindow 1 remainingX (w2i$ y+hght-1) startX
            let dummyCW = CWindow dummy (1, i2w remainingX, (w2i$ y+hght-1), i2w startX)
            -- wclear dummy; wnoutRefresh dummy
            io$ dbgPrnt$ "Dummy horiz (screen "++show (nLines,nCols)++"): "++show dummyCW
--            clearWindow dummyCW
            ----------- Then the vertical right border:
            let startY     = (w2i$ y+1)
                remainingY = fromIntegral$ nLines - startY - 1
            dummy2 <- newWindow remainingY 1 startY (nCols-1)
            let dummy2CW = CWindow dummy2 (i2w remainingY, 1, i2w startY, i2w (nCols-1))
            io$ dbgPrnt$ "Dummy vert: "++show dummy2CW
            -- wclear dummy2; wnoutRefresh dummy2
--            clearWindow dummy2CW
            return [dummy,dummy2]
           else return []
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
i2w :: (Show n, Integral n) => n -> Word
i2w i | i < 0 = error$"i2w: Cannot convert negative Int to Word: "++show i
i2w i = fromIntegral i

w2i :: (Show n, Integral n) => Word -> n
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
