module Fox.Index.Directory where

newtype IndexDirectory = IndexDirectory { unIndexDirectory :: FilePath }
                       deriving (Eq, Ord, Show)

openIndexDirectory :: FilePath -> IO IndexDirectory
openIndexDirectory indexDir = do
  undefined
