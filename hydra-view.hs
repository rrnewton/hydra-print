{-# LANGUAGE CPP#-}

-- | The server, sets up a named pipe to receive the names of named pipes.

module Main where

import Data.IORef 
import qualified Data.ByteString.Char8 as B
import Control.Monad       (when, forM_)
import Control.Concurrent.Chan
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(..), usageInfo)
import System.Environment (getArgs, getEnvironment)
import System.IO          (IOMode(..), openFile, hClose, hPutStrLn, stderr)
import System.IO.Unsafe   (unsafePerformIO)
import System.Directory   (doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath    ((</>),takeFileName)
import System.Posix.Files (createNamedPipe)
import System.Posix.Types (CMode(..))
import System.Process     (runInteractiveCommand)
import System.Exit        (exitSuccess, exitFailure)

import UI.HydraPrint (hydraPrint, defaultHydraConf)
import qualified System.IO.Streams as S
import System.IO.Streams.Concurrent (concurrentMerge, chanToInput, chanToOutput)

--------------------------------------------------------------------------------

-- | Extra usage docs beyond the flag info.
usageStr :: String
usageStr = unlines $
 [ "     "
 , "     "
 , "     "
 , "     "
 , "     "
 , "     "
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
  deriving (Eq,Read,Ord,Show)

-- | Command line options.
cli_options :: [OptDescr Flag]
cli_options = 
     [ Option ['p'] ["pipesrc"] (ReqArg PipeSrc "FILENAME")
       "Use a specific file as the source of streams."
     , Option ['s'] ["session"] (ReqArg SessionID "STRING")
       "Use a sessionID to avoid collission with other hydra-view servers."
     ]

main :: IO ()
main = do
  cli_args <- getArgs
  print cli_args
  let x@(options,restargs,errs) = getOpt Permute cli_options cli_args
  print x      
  when (not (null errs)) $ do
    putStrLn$ "Errors parsing command line options:" 
    mapM_ (putStr . ("   "++)) errs       
    putStrLn "\nUSAGE: hydra-view [OPTIONS] -- commands to run"
    putStrLn$ usageStr    
    exitFailure    

  -- let pipepath = foldl fn defaultPipeSrc options
  --     fn _ (PipeSrc file) = file
  --     fn _ (SessionID id) = defaultTempDir </> "hydra-view_session_"++id++".pipe"
  let pipePerms = CMode 0o777
      openPipe p = do b <- doesFileExist p
                      when b $ removeFile p
                      createNamedPipe p pipePerms
  pipe <- case options of
           [PipeSrc file] -> return file -- Pre-existing! 
           [SessionID id] -> do let p = defaultTempDir </> "hydra-view_session_"++id++".pipe"
                                openPipe p
                                return p
           [] -> do openPipe defaultPipeSrc
                    return defaultPipeSrc
           _ -> error$"hydra-view: too many options!  Can only set the pipe to use once: "++show options

  putStrLn$ "GOT PIPE TO USE "++show pipe

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
        putStrLn$ " NEW string on pipe! "++show pth
        let str = B.unpack pth
        hnd <- openFile str ReadMode              
        -- mhnd <- safeOpenFile str ReadMode
--          case mhnd of
--            Nothing -> do putStrLn ("[hydra-view]")
        strm <- S.handleToInputStream hnd
        addNPoll pth (hnd,strm)
        return (takeFileName str,strm)

  srcs <- S.mapM rd strmSrc

  -- Create and endpoint to read and write to:
  -- chan    <- newChan
  -- chanIn  <- chanToInput  chan
--    chanOut <- chanToOutput chan

  -- Open a subprocess for the command, if it exists.
  srcs' <-
    case restargs of
      [] -> return srcs
      _  -> do 
               let cmd = unwords restargs
               putStrLn$" RUNNING COMMAND "++show cmd
               (_stdinH, stdoutH, stderrH, pid) <- runInteractiveCommand cmd
               inS    <- S.lines =<< S.handleToInputStream stdoutH
               errS   <- S.lines =<< S.handleToInputStream stderrH
               merged <- concurrentMerge [inS,errS]
               merged'   <- S.map (B.append (B.pack "\n")) merged -- Unlines it.
               singleton <- S.fromList [("main",merged')]
               S.appendInputStream singleton srcs

  hydraPrint defaultHydraConf srcs'
  return ()

  --   cli_args <- getArgs
  -- let (options,args,errs) = getOpt Permute cli_options cli_args
  -- unless (null errs && null args) $ do
  --   putStrLn$ "Errors parsing command line options:" 
  --   mapM_ (putStr . ("   "++)) errs       
  --   putStrLn "\nUSAGE: [set ENV VARS] ./benchmark.hs [CMDLN OPTIONS]"    
  --   putStr$ usageInfo "\n CMDLN OPTIONS:" cli_options
  --   putStrLn$ usageStr    
  --   exitFailure
  return ()
