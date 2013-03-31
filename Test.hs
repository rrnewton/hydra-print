
import Data.IORef
import Control.Monad
import Control.Concurrent
import UI.DemuxPrint
import UI.HSCurses.CursesHelper as CH
import UI.HSCurses.Curses as C hiding (s1,s3,tl,ls)

main :: IO ()
main = do
  CH.start
  initialize
  ref <- newIORef []
  let redraw = do
        old <- readIORef ref
        mapM_ delWin old
        ws <- createWindows (replicate 6 undefined)
        mapM_ wRefresh ws
        writeIORef ref ws
        return ()
    
  forM_ [1..100] $ \i -> do
    let dispAll str = do
          ws <- readIORef ref
          forM ws $ \w ->
            do wMove w 2 1
               wAddStr w str
--    _ <- CH.getKey redraw
--    _ <- CH.getKey (return ())
    dispAll$ "Iter "++show i
    redraw               
    threadDelay$ 100 * 1000

  CH.end

