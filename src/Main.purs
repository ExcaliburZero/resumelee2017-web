module Main where

import Prelude hiding (append)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (on, append, create, body, ready, setText, getValue, select, getProp, setProp, remove, JQuery, JQueryEvent)
import Control.Monad.Eff.Ref (newRef, REF, readRef, writeRef, Ref)
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

    -- Setup files, current diretory, and prompt
    files <- newRef defaultFiles

    let dir = FilePath ("~" : Nil)
    curDir <- newRef dir

    setText (show dir <> promptSym) prompt

    -- Watch for commands
    on "change" (handleCommand prompt input output files curDir) cmdForm
  where
    -- | Handles the execution of terminal commands.
    handleCommand :: JQuery
      -> JQuery
      -> JQuery
      -> Ref File
      -> Ref FilePath
      -> JQueryEvent
      -> JQuery
      -> Eff ( dom :: DOM
             , ref :: REF
             , console :: CONSOLE
             | eff
             ) Unit
    handleCommand prompt input output files curDir _ _ = unsafePartial do
      val <- getValue input
      for_ (runExcept (read val)) \command -> do
        printCommand command prompt output curDir
        clearInput input

        runCommand command input output files curDir

        updatePrompt prompt curDir
        scrollDown output

    -- | Prints out the prompt for the command that the user entered.
    printCommand command prompt output curDir = do
      outLine <- create "<p>"
      dir <- readRef curDir
      setText (show dir <> promptSym <> command) outLine
      append outLine output

    -- | Clears out the command input box.
    clearInput input = setProp "value" "" input

    -- | Updates the prompt to use the new current directory.
    updatePrompt prompt curDir = do
        nDir <- readRef curDir
        setText (show nDir <> promptSym) prompt

    -- | Scrolls the terminal window down to the bottom of the output.
    scrollDown output = do
      scrollVal <- getProp "scrollHeight" output
      for_ (runExcept (read scrollVal)) \(scrollH :: Int) -> do
        setProp "scrollTop" scrollH output


    -- | Runs the given terminal command.
    runCommand :: String
      -> JQuery
      -> JQuery
      -> Ref File
      -> Ref FilePath
      -> Eff ( dom :: DOM
             , ref :: REF
             , console :: CONSOLE
             | eff
             ) Unit
    runCommand comm input output files curDir = case words comm of
      ["clear"]     -> runClear
      ["ls"]        -> runLs output files curDir
      ["cd", ".."]  -> runCdDD curDir
      ["cd", dir]   -> runCdDir dir output files curDir
      ["cat", file] -> runCat file output files curDir
      _             -> runInvalidCommand comm output

    -- | Clears all of the output from the terminal screen.
    runClear = do
      outLines <- select "#output p"
      remove outLines

    -- | Lists all of the files in the directory.
    runLs output files curDir = do
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

    -- | cd's down one directory.
    runCdDD curDir = do
      cd <- readRef curDir
      if cd /= FilePath ("~" : Nil)
         then case cd of
           FilePath xs -> do
             case init xs of
               Just x -> writeRef curDir (FilePath x)
               Nothing -> log "Empty current file path."
         else
           writeRef curDir cd

    -- | Attempts to cd into the given subdirectory.
    runCdDir dir output files curDir = do
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

    -- | Attempts to print out the contents of the given file in the current directory.
    runCat file output files curDir = do
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

    -- | Prints out a message indicating that an invalid command was run.
    runInvalidCommand comm output = do
      outLine <- create "<p>"
      setText ("Invalid command: " <> comm) outLine
      append outLine output

-- | The symbol used for the terminal prompt.
promptSym :: String
promptSym = " $ "

-- | The files in the simulated file system.
defaultFiles :: File
defaultFiles = Dir "~/" $
    Dir "tmp" (
        File "test.txt" "This is a test file."
      : Nil
    )
  : File "hello.txt" "Hello there."
  : File "README" "You shoud probably read this."
  : Nil

-- | Parses the given String into a FilePath.
parseFP :: String -> FilePath
parseFP s = FilePath (toUnfoldable (words s))

-- | Attempts to return the directory at the given file path in the given directory.
navDir :: FilePath -> File -> Maybe File
navDir _ (File _ _) = Nothing
navDir (FilePath Nil) dir = Just dir
navDir (FilePath (x:xs)) (Dir name files) =
    conDir >>= (\d -> navDir (FilePath xs) d)
  where
    conDir = findFile x files

-- | Attempts to find the given file in the given list of files.
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

-- | Concatenates the given list of strings together using the given seperator string.
concat :: String -> List String -> String
concat _   Nil    = ""
concat sep (x:xs) = x <> sep <> concat sep xs

-- | Shows the files that are in the given directory.
showFiles :: File -> String
showFiles (Dir name files) = concat "\t" (map show files)
showFiles file = show file

-- | A directory or file.
data File = Dir String (List File) | File String String

instance showFile :: Show File where
    show (File name _) = name
    show (Dir  name _) = name

-- | A path for a file location.
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
