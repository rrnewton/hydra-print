
-- | The client, goes with hydra-view.

module Main where

import Data.IORef 
import qualified Data.ByteString.Char8 as B
import Control.Monad       (when, forM_)
-- import Control.Concurrent.Chan
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs, getEnvironment)
import System.IO          (IOMode(..), openFile, hClose)
import System.IO.Unsafe   (unsafePerformIO)
import System.Directory   (doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath    ((</>),takeFileName)
import System.Posix.Files (createNamedPipe)
import System.Posix.Types (CMode(..))
import System.Process     (runInteractiveCommand)
import System.Exit        (exitFailure, exitSuccess)

import UI.HydraPrint (hydraPrint, defaultHydraConf)
import qualified System.IO.Streams as S
import System.IO.Streams.Concurrent (concurrentMerge)

--------------------------------------------------------------------------------

-- | Extra usage docs beyond the flag info.
usageStr :: String
usageStr = unlines $
 [ "\n Hydra-head works with 'hydra-view' to add a new stream."
 , " "   
 , " There are two main modes.  Hydra-head either returns immediately,"
 , " producing the name of a pipe you can use.  Or it pipes its "   
 , " standard input to the hydra-view session."
 , " "
 , " If you use the named pipe method, hydra-view will continue monitoring"
 , " the pipe until you explicitly DELETE it.  Thus the pipe can be used to "
 , " aggregate the output of multiple commands." 
 ]

-- | Datatype for command line options.
data Flag =
       ReturnPipe         -- | Return the name of a pipe immediately.
     | SessionID FilePath -- | Use a session ID to match-up with the view server.
     | ShowHelp       
  deriving (Eq,Read,Ord,Show)

-- | Command line options.
cli_options :: [OptDescr Flag]
cli_options = 
     [ 
       Option ['s'] ["session"] (ReqArg SessionID "STRING")
       "Use a sessionID to avoid collision with other hydra-view servers."
     , Option ['p'] ["pipe"]    (NoArg  ReturnPipe)
       "Return immediately [the path of] a named pipe for subsequent use."
     , Option ['h'] ["help"]    (NoArg  ShowHelp)
       "Show help and exit."
     ]

theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

main :: IO ()
main = do
  cli_args <- getArgs
  let (options,restargs,errs) = getOpt Permute cli_options cli_args
      showUsage = do putStrLn "USAGE: hydra-head [OPTIONS] -- commands to run"
                     putStrLn$ usageStr
                     putStr$ usageInfo " OPTIONS:" cli_options                     
  when (not (null errs)) $ do
    putStrLn$ "Errors parsing command line options:" 
    mapM_ (putStr . ("   "++)) errs       
    showUsage
    exitFailure      
  when (ShowHelp `elem` options) $ do showUsage; exitSuccess



  return ()

