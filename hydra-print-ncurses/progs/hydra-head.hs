
-- | The client, goes with hydra-view.

module Main where

import Data.IORef
import Data.Word
import qualified Data.ByteString.Char8 as B
import Control.Monad       (when, forM_)
import Control.Concurrent (threadDelay)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs, getEnvironment)
import System.IO          (IOMode(..), openFile, hClose, hPutStrLn)
import System.IO.Unsafe   (unsafePerformIO)
import System.Directory   (doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath    ((</>),takeFileName)
import System.Posix.Files (createNamedPipe)
import System.Posix.Types (CMode(..))
import System.Process     (runInteractiveCommand)
import System.Exit        (exitFailure, exitSuccess)
import System.Random

import Prelude as P

import UI.HydraPrint ()
import UI.HydraPrint.NCurses (hydraPrint, defaultHydraConf)
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

isSessionID :: Flag -> Bool
isSessionID (SessionID _ ) = True
isSessionID _ = False

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

--------------------------------------------------------------------------------
-- <DUPLICATED> FIXME, factor these out:

theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

-- Here we make some attempt to work on Windows:
defaultTempDir :: String
defaultTempDir = unsafePerformIO $ do
  b <- doesDirectoryExist "/tmp/"
  if b then return "/tmp/" else
    case lookup "TEMP" theEnv of
      Nothing -> error "hydra-view: Could not find a temporary directory to put the pipe source."
      Just d  -> return d

-- | There's a simple policy on where to put the pipes, so that other clients can
-- find it.
defaultPipeSrc :: String
defaultPipeSrc = defaultTempDir ++ "hydra-view.pipe"

sessionPipe :: String -> String
sessionPipe id =
  defaultTempDir </> "hydra-view_session_"++id++".pipe"

-- </DUPLICATED>
--------------------------------------------------------------------------------


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

  let pipePerms = CMode 0o777
      openPipe p = do b <- doesFileExist p
                      when b $ removeFile p
                      createNamedPipe p pipePerms

      getName g = do let suffix :: Word64
                         (suffix,g') = random g
                     let path = defaultTempDir </> "hydra-head_tmp_pipe_"++show suffix
                     b <- doesFileExist path
                     if b then getName g'
                       else return path

      serverPipe = case filter isSessionID options of
                     []             -> defaultPipeSrc
                     [SessionID id] -> sessionPipe id
                     x  -> error "hydra-head: Multiple sessions specified: "++show x

  stdGen <- getStdGen
  newPipe <- getName stdGen
  openPipe newPipe

  -- Tell the server about the new pipe:
  hnd <- openFile serverPipe AppendMode
  hPutStrLn hnd newPipe
  hClose hnd

  if ReturnPipe `elem` options then
     putStrLn newPipe
   else do
    -- SO terribly hackish.  I get this error if I don't wait a bit:
-- hydra-head.exe: /tmp/hydra-head_tmp_pipe_14447421285220617689: openFile: does not exist (Device not configured)
    threadDelay$ 10 * 1000
    outH <- openFile newPipe AppendMode
    out <- S.handleToOutputStream outH
    S.connect S.stdin out
    hClose outH
    removeFile newPipe
  return ()
