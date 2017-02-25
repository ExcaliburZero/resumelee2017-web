module Main where

import Prelude hiding (append)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (on, append, create, body, ready, setText, getValue, select, getProp, setProp, remove)
import Control.Monad.Eff.Ref (newRef, REF, readRef, writeRef)
import Control.Monad.Except (runExcept)
import Data.Array (toUnfoldable)
import Data.Foreign.Class (read)
import Data.Foldable (for_)
import Data.List (List(..), (:), init)
import Data.String.Utils (words)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import Partial.Unsafe (unsafePartial)

main :: forall eff. Eff ( ref :: REF
                        , dom :: DOM
                        , console :: CONSOLE
                        | eff
                        ) Unit
main = ready $ do
    body <- body

    log "Script imported"

    cmdForm <- select "#commandForm"
    prompt <- select "#prompt"
    input  <- select "#command"
    output <- select "#output"

    let dir = FilePath ("~" : Nil)
    curDir <- newRef dir
    setText (show dir <> promptSym) prompt

    files <- newRef defaultFiles

    on "change" (handleCommand prompt input output files curDir) cmdForm
  where
--    handleCommand :: JQuery
--      -> JQuery
--      -> JQueryEvent
--      -> JQuery
--      -> Eff ( dom :: DOM
--             , console :: CONSOLE
--             | eff
--             ) Unit
    handleCommand prompt input output files curDir _ _ = unsafePartial do
      val <- getValue input
      for_ (runExcept (read val)) \command -> do
        outLine <- create "<p>"
        dir <- readRef curDir
        setText (show dir <> promptSym <> command) outLine
        setText (show dir <> promptSym) prompt
        append outLine output
        setProp "value" "" input
        runCommand command input output files curDir
        nDir <- readRef curDir
        setText (show nDir <> promptSym) prompt
        scrollVal <- getProp "scrollHeight" output
        for_ (runExcept (read scrollVal)) \(scrollH :: Int) -> do
          setProp "scrollTop" scrollH output
    runCommand comm input output files curDir = case words comm of
      ["clear"] -> do
        outLines <- select "#output p"
        remove outLines
      ["ls"] -> do
        outLine <- create "<p>"
        fs <- readRef files
        cd <- readRef curDir
        let cd' = case cd of
                    FilePath (_:xs) -> FilePath xs
                    x -> x
        let d = case navDir cd' fs of
                  Just f -> f
                  Nothing -> File "" ""
        setText (showFiles d) outLine
        append outLine output
      ["cd", ".."] -> do
        cd <- readRef curDir
        if cd /= FilePath ("~" : Nil)
           then case cd of
             FilePath xs -> do
               case init xs of
                 Just x -> writeRef curDir (FilePath x)
                 Nothing -> log "Empty current file path."
           else
             writeRef curDir cd
      ["cd", dir] -> do
        let nfp = parseFP dir
        fs <- readRef files
        cd <- readRef curDir
        let cd' = case cd of
                    FilePath (_:xs) -> FilePath xs
                    x -> x
        let d = navDir cd' fs
        let nd = d >>= navDir nfp
        outLine <- create "<p>"
        case nd of
          Just sd -> do
            writeRef curDir (cd <> nfp)
            setText "Entered directory" outLine
          Nothing -> do
            setText "Directory not found" outLine
        append outLine output
      ["cat", file] -> do
        outLine <- create "<p>"
        fs <- readRef files
        cd <- readRef curDir
        let cd' = case cd of
                    FilePath (_:xs) -> FilePath xs
                    x -> x
        let d = navDir cd' fs
        let f = case d of
                  Just (Dir _ dfiles) -> findFile file dfiles
                  _ -> Nothing
        _ <- case f of
               Just (File _ contents) -> setText contents outLine
               _ -> setText ("Invalid file: " <> file) outLine
        append outLine output

      _ -> do
        outLine <- create "<p>"
        setText ("Invalid command: " <> comm) outLine
        append outLine output

promptSym :: String
promptSym = " $ "

defaultFiles :: File
defaultFiles = Dir "~/" $
    Dir "tmp" (
        File "test.txt" "This is a test file."
      : Nil
    )
  : File "hello.txt" "Hello there."
  : File "README" "You shoud probably read this."
  : Nil

parseFP :: String -> FilePath
parseFP s = FilePath (toUnfoldable (words s))

navDir :: FilePath -> File -> Maybe File
navDir _ (File _ _) = Nothing
navDir (FilePath Nil) dir = Just dir
navDir (FilePath (x:xs)) (Dir name files) =
    conDir >>= (\d -> navDir (FilePath xs) d)
  where
    conDir = findFile x files

findFile :: String -> List File -> Maybe File
findFile _ Nil = Nothing
findFile n (f:fs) =
  let 
    fName = case f of
      File name _ -> name
      Dir name _ -> name
  in
    if n == fName 
       then Just f
       else findFile n fs

concat :: String -> List String -> String
concat _   Nil    = ""
concat sep (x:xs) = x <> sep <> concat sep xs

showFiles :: File -> String
showFiles (Dir name files) = concat "\t" (map show files)
showFiles file = show file

data File = Dir String (List File) | File String String

instance showFile :: Show File where
    show (File name _) = name
    show (Dir  name _) = name

data FilePath = FilePath (List String)

instance eqFilePath :: Eq FilePath where
    eq (FilePath xs) (FilePath ys) = xs == ys

instance semigroupFilePath :: Semigroup FilePath where
    append (FilePath xs) (FilePath ys) = FilePath (xs <> ys)

show' :: FilePath -> String
show' (FilePath Nil) = ""
show' (FilePath (x:xs)) = "/" <> x <> show' (FilePath xs)

instance showFilePath :: Show FilePath where
    show (FilePath Nil) = ""
    show (FilePath (x:xs)) = x <> show' (FilePath xs)
