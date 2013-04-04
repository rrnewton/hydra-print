
module Scripting.Parallel.ThreadPool
       (
         -- * Helpers for parallel scripting, i.e. prepping input to `hydraPrint`
         parForM
       )
       where

import Data.IORef
import Control.Monad (forM)
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Control.Concurrent.Async as A
import Prelude as P

import qualified System.IO.Streams as S
-- import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.Concurrent (chanToInput, chanToOutput)

--------------------------------------------------------------------------------


-- | A helper for parallel scripting.  Run work items in parallel on N worker threads
-- (a thread pool), creating only ONE output stream per worker, not one per parallel
-- task.  Specifically, each invocation of the user's function is given an
-- OutputStream that it holds a "lock" on -- it is the only thread accessing that
-- output stream.
--
-- This function returns immediately with
--   (1) a list of input streams that will carry results on the fly, as they are produced, and
--   (2) a barrier action that waits for all parallel work to be finished and yields the final results.
-- The first list is `numWorkers` long, and the second is `numTasks`.
--
-- Additional contracts: 
parForM :: Int -> [a] -> (S.OutputStream c -> a -> IO b) -> IO ([S.InputStream c], IO [b])
parForM numWorkers inputs action = 
  do let numTasks = P.length inputs
     answers     <- sequence$ P.replicate numTasks newEmptyMVar
     workLeft    <- newIORef (P.zip inputs answers)
     chans       <- sequence $ P.replicate numWorkers newChan
     resultStrms <- mapM chanToInput  chans 
     outStrms    <- mapM chanToOutput chans
     ------------
     -- That didn't seem to work.... let's try a different way of creating an input/output pair.     
     ------------     
     asyncs <- forM (zip outStrms [(0::Int)..]) $ \ (strm,idx) -> 
       A.async $ do
--           B.hPutStrLn System.IO.stderr (B.pack$ "INSIDE ASYNC, "++show idx++" of "++show (P.length outStrms))
           let pfloop = do -- Pop work off the queue:                           
                           x <- atomicModifyIORef workLeft 
                                 (\ls -> if P.null ls 
                                         then ([], Nothing) 
                                         else (P.tail ls, Just ((\ (h:_)->h) ls)))
--                           B.hPutStrLn System.IO.stderr (B.pack$ "POPPED work "++show (idx, fmap (const ()) x))
                           case x of 
                             Nothing -> S.write Nothing strm -- End the stream.
                             Just (input,mv) -> 
                               do result <- action strm input
                                  putMVar mv result
                                  pfloop
           pfloop
     let barrier = do 
           mapM_ A.wait asyncs -- For exception handling.
           mapM readMVar answers

     return (resultStrms, barrier)

