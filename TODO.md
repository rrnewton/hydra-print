
A staging ground for small TODOs.  These could be promoted to github
issues at some point.

 * Put the name/title on each tile
 * indicate blankness for unused tiles?
 * keep tiles for exited streams around for a couple seconds.

 * add wnoutrefresh to hscurses lib (and use it) 
   (Then maybe get more sophisticated about when to do updates)

 * Rather than always having a dense packing of tiles (deleting the
   last window), allow "gaps" in the middle, to avoid unnecessary
   shifting.

 * Could implement scroll-wheel scrolling within the panels.... but
   that might be a bridge too far, may be better to just do a tmux
   backend.
 
BUGS
---- 

 * Existing windows blank when new ones are added.
   - Still have blinking issues [2013.04.03] 
