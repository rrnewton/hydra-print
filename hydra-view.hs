{-# LANGUAGE CPP#-}

-- | The server, sets up a named pipe to receive the names of named pipes.

module Main where

import Data.IORef
import qualified Data.List as L
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
 [ "\n Hydra-view provides multi-\"headed\" output. That is, it provides a"
 , " way to view a dynamic collection of output streams."
 , " "   
 , " Hydra-view works by creating a named pipe to which file names are sent."
 , " Each one one of those filenames is a 'stream source', usually a named-"
 , " pipe itself.  Each such stream gets its own panel while it is producing "
 , " output. "
 , " "
 , " See the command 'hydra-head' for a convenient way to connect new"
 , " streams to an existing hydra-view session."
 ]


theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

-- | There's a simple policy on where to put the pipes, so that other clients can
-- find it.
defaultPipeSrc :: String
defaultPipeSrc = defaultTempDir ++ "hydra-view.pipe"

-- Here we make some attempt to work on Windows:
defaultTempDir :: String
defaultTempDir = unsafePerformIO $ do 
  b <- doesDirectoryExist "/tmp/"
  if b then return "/tmp/" else
    case lookup "TEMP" theEnv of
      Nothing -> error "hydra-view: Could not find a temporary directory to put the pipe source."
      Just d  -> return d

-- | Datatype for command line options.
data Flag =
       PipeSrc FilePath   -- | Use a user-provided file as the source of pipes.
     | SessionID FilePath -- | Use a session ID to match-up with clients.
     | ShowHelp 
  deriving (Eq,Read,Ord,Show)

isSrcFlag (PipeSrc _)   = True
isSrcFlag (SessionID _) = True
isSrcFlag _             = False

-- | Command line options.
cli_options :: [OptDescr Flag]
cli_options = 
     [ Option ['p'] ["pipesrc"] (ReqArg PipeSrc "FILENAME")
       "Use a specific file as the source of streams."
     , Option ['s'] ["session"] (ReqArg SessionID "STRING")
       "Use a sessionID to avoid collision with other hydra-view servers."
     , Option ['h'] ["help"]    (NoArg  ShowHelp)
       "Show help and exit."
     ]

main :: IO ()
main = do
  cli_args <- getArgs
  let x@(options,restargs,errs) = getOpt Permute cli_options cli_args
      showUsage = do putStrLn "USAGE: hydra-view [OPTIONS] -- commands to run"
                     putStrLn$ usageStr
                     putStr$ usageInfo " OPTIONS:" cli_options                     
  when (not (null errs)) $ do
    putStrLn$ "Errors parsing command line options:" 
    mapM_ (putStr . ("   "++)) errs       
    showUsage
    exitFailure    

  -- let pipepath = foldl fn defaultPipeSrc options
  --     fn _ (PipeSrc file) = file
  --     fn _ (SessionID id) = defaultTempDir </> "hydra-view_session_"++id++".pipe"
  let pipePerms = CMode 0o777
      openPipe p = do b <- doesFileExist p
                      when b $ removeFile p
                      createNamedPipe p pipePerms
      (hlp, options') = L.partition (==ShowHelp) options
  case hlp of
    [] -> return () 
    _  -> do showUsage
             exitSuccess
  pipe <- case options' of
           [PipeSrc file] -> return file -- Pre-existing! 
           [SessionID id] -> do let p = defaultTempDir </> "hydra-view_session_"++id++".pipe"
                                openPipe p
                                return p
           [] -> do openPipe defaultPipeSrc
                    return defaultPipeSrc
           _ -> error$"hydra-view: too many options!  Can only set the pipe to use once: "++show options

-- The problem is that this busy waits... better to use tail -F:
#if 0
  -- Persistently read the pipe again and again unless the file is removed.
  let persistent = do
        hnd <- openFile pipe ReadMode
        strm <- S.lines =<< S.handleToInputStream hnd
        let loop :: IO (Maybe B.ByteString)
            loop = do
              x <- S.read strm
              case x of
                Nothing -> do
                  putStrLn "End of file..."
                  hClose hnd
                  b <- doesFileExist pipe
                  if b then persistent
                    else return Nothing
                Just _ -> return x
        loop
  strmSrc <- S.makeInputStream persistent
#else
  (_, stdoutH, _, pid) <- runInteractiveCommand$ "tail -f "++pipe
  strmSrc <- S.lines =<< S.handleToInputStream stdoutH
#endif  

  openHnds <- newIORef []
  -- Lazy resource cleanup: keeps a set of open files, and collects old ones as new
  -- ones are added:
  let addNPoll newpth pr = do 
        open <- atomicModifyIORef openHnds
                 (\ls -> let new = (newpth,pr) : ls in (new,new))
        forM_ open $ \ (_pth,(hnd,strm)) -> do
           b <- S.atEOF strm
           when b $ hClose hnd
        return ()

  -- Read new pipes to get bytestring streams.
  let rd pth = do
--        putStrLn$ " NEW string on pipe! "++show pth
        let str = B.unpack pth
            strmName = takeFileName str
        b <- doesFileExist str 
        if not b then do 
           nl <- S.nullInput
           return (strmName, nl)
         else do 
           hnd <- openFile str ReadMode              
           -- TODO: Catch error here and recover:
           -- mhnd <- safeOpenFile str ReadMode
   --          case mhnd of
   --            Nothing -> do putStrLn ("[hydra-view]")
           strm <- S.handleToInputStream hnd
           addNPoll pth (hnd,strm)
           return (strmName,strm)

  srcs <- S.mapM rd strmSrc

  srcs' <-
    case restargs of
      [] -> do putStrLn$ " [hydra-view] No command to run; send named-pipe names to: "++show pipe
               return srcs
      _  -> do 
               let cmd = unwords restargs
               putStrLn$" [hydra-view] First output stream from command: "++show cmd
               (_stdinH, stdoutH, stderrH, pid) <- runInteractiveCommand cmd
               inS    <- S.lines =<< S.handleToInputStream stdoutH
               errS   <- S.lines =<< S.handleToInputStream stderrH
               merged <- concurrentMerge [inS,errS]
               merged'   <- S.map (B.append (B.pack "\n")) merged -- Unlines it.
               singleton <- S.fromList [("main",merged')]
               S.appendInputStream singleton srcs

  hydraPrint defaultHydraConf srcs'
  return ()
