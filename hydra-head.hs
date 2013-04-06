
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
import System.Exit        (exitFailure)

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
 ]

-- | Datatype for command line options.
data Flag =
       ReturnPipe         -- | Return the name of a pipe immediately.
     | SessionID FilePath -- | Use a session ID to match-up with the view server.
  deriving (Eq,Read,Ord,Show)



theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

main = do
  return ()

