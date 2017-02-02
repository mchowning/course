{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = do args <- getArgs
          case args of
            fn :. Nil -> run fn
            _         -> putStrLn "usage: runhaskell FileIO.hs filename"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run fp = do content <- readFile fp
            results <- getFiles $ lines content
            printFiles results

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles = sequence . map getFile
-- getFiles = mapM getFile
-- getFiles = sequence . (<$>) getFile
-- getFiles = foldRight helper (return Nil)
--   where
--     helper :: FilePath -> IO (List (FilePath, Chars)) -> IO (List (FilePath, Chars))
--     helper fp ls = do tup <- getFile fp
--                       ls' <- ls
--                       return $ tup :. ls'

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fp = do cs <- readFile fp
                return (fp, cs)
-- getFile fp = readFile fp >>= \cs -> return (fp, cs)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles = void . sequence . map (uncurry printFile)
-- printFiles = void . mapM (uncurry printFile)
-- printFiles = foldLeft helper (return ())
--   where
--     helper :: IO () -> (FilePath, Chars) -> IO ()
--     helper _ = void . uncurry printFile

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fp cs = do putStrLn ("============ " ++ fp)
                     putStrLn cs
-- printFile fp cs = putStrLn ("============ " ++ fp) >>
--                   putStrLn cs

