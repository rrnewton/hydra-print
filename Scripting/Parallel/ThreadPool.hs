
-- | Deprecated, this has been renamed to "Scripting.Parallel.Hydra"

module Scripting.Parallel.ThreadPool
       (
         -- * Helpers for parallel scripting, i.e. prepping input to `hydraPrint`
         parForM
       )
       where

import qualified Scripting.Parallel.Hydra as H
import qualified System.IO.Streams as S

{-# DEPRECATED parForM "This has been moved to the module Hydra" #-}
parForM :: Int -> [a] -> (S.OutputStream c -> a -> IO b) -> IO ([S.InputStream c], IO [b])
parForM = H.parForM
