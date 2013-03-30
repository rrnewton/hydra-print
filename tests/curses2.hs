
import UI.HSCurses.Curses
import UI.HSCurses.Widgets
import UI.HSCurses.CursesHelper

import Control.Applicative
import Control.Monad

main = do
  start

  w <- initScr
  (x,y) <- scrSize

  cur <- getYX w
  move (x `quot` 2) (y `quot` 2)
  puts$"Helol, size "++show (x,y)++" init cursor was "++show cur

  wBorder w defaultBorder

  let tw = newTextWidget defaultTWOptions "my TW"
      draw1 = drawTextWidget (10,10) (10,10) DHNormal
  draw1 tw
  refresh

  k <- getKey refresh
  let tw2 = textWidgetSetText tw "my tw \nyay haoeuthaoeu aohteoah uaoehu "
  draw1$ tw2
  refresh

  k <- getKey refresh
  draw1$ textWidgetScrollDown (1,0) tw2 
  refresh

-- newWin

--  

  -- Scrolling off the bottom creates an error:
  forM_ [1..10] $ \ i -> do
    puts$ "Hello "++show i++"\n"
    k <- getKey refresh

    refresh

    
--  update
  end

  
puts s = drawLine (length s) s