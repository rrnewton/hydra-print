
import Data.IORef
import Control.Monad
import Control.Concurrent
import UI.HydraPrint (initialize, createWindows)
import UI.HSCurses.CursesHelper as CH
import UI.HSCurses.Curses as C hiding (s1,s3,tl,ls)

main :: IO ()
main = do
  CH.start
  initialize
  ref <- newIORef []
  let reCreate = do
        old <- readIORef ref
        mapM_ delWin old
        ws <- createWindows 6
        writeIORef ref ws
  -- Do it once to create the inital window structure.
  reCreate

  let dispAll y str = do
        ws <- readIORef ref
        forM ws $ \w ->
          do wMove w y 1
             wAddStr w str

      refreshAll = mapM_ wRefresh =<< readIORef ref 
  
  let loop i = do 
   --    _ <- CH.getKey redraw
       sz <- scrSize
       dispAll 2$ "Iter "++show i++" "++" size "++show sz
       refreshAll
       k <- C.getCh
       case k of
         KeyChar 'q' -> do
           CH.end
           putStrLn "NCurses finished."
         KeyResize -> do           
           C.endWin
           C.update
           reCreate
           dispAll 3$ "RESIZING! "
           loop (i+1)
         c -> do
           dispAll 3$ "KeyPress "++show c
           refreshAll
           loop (i+1)
  loop 0

