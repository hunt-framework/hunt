module Fox.Index.Directory where

import Control.Monad.IO.Class
import Control.Monad.Except
import System.Directory

newtype IndexDirectory = IndexDirectory { unIndexDirectory :: FilePath }
                       deriving (Eq, Ord, Show)

data IndexDirErr = ErrInvalidDirectory
                 | ErrIndexLocked

openIndexDirectory :: FilePath -> IO (Either IndexDirErr IndexDirectory)
openIndexDirectory indexDir = runExceptT $ do
    -- bail out early so we don't overwrite anything
  -- in case indexDir refers to a file.
  fileExists <- liftIO $ doesFileExist indexDir
  when fileExists $
    throwError ErrInvalidDirectory

  -- check explicitly for existence to avoid
  -- exceptions.
  dirExists <- liftIO $ doesDirectoryExist indexDir
  unless dirExists $
    liftIO $ createDirectoryIfMissing True indexDir

  return $ IndexDirectory indexDir
