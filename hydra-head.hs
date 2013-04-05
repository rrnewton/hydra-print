
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


main = do

  
