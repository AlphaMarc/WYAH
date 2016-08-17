module Main where

import           ParserTest
import           Test.DocTest

import           Data.List
import           System.Directory
import           System.FilePath

import           Control.Monad

main :: IO ()
main = getHaskellSourceFiles >>= doctest


getHaskellSourceFiles :: IO [FilePath]
getHaskellSourceFiles = filter (isSuffixOf ".hs") <$> go "src"
                    where go dir =
                                    do (dirs, files) <- getFilesAndDirectories dir
                                       (files ++) . concat <$> mapM go dirs


getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  liftM2 (,) (filterM doesDirectoryExist c) (filterM doesFileExist c)
