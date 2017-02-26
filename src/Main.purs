module Main where

import Prelude hiding (append)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (on, append, create, body, ready, setText, getValue, select, getProp, setProp, remove, JQuery, JQueryEvent, css, appendText)
import Control.Monad.Eff.Ref (newRef, REF, readRef, writeRef, Ref)
import Control.Monad.Except (runExcept)
import Data.Array (toUnfoldable)
import Data.Foreign.Class (read)
import Data.Foldable (for_)
import Data.List (List(..), (:), init)
import Data.Maybe (Maybe(..))
import Data.String.Utils (words)
import Data.Traversable (traverse)
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
    input  <- select "input"
    output <- select "#output"

    -- Setup files, current diretory, and prompt
    files <- newRef defaultFiles

    let dir = FilePath ("~" : Nil)
    curDir <- newRef dir

    setPrompt prompt dir

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

    setPrompt prompt dir = do
      dirTxt <- create "<span>"
      css {"color": colorSecondary} dirTxt
      setText (show dir) dirTxt
      append dirTxt prompt

      promptTxt <- create "<span>"
      css {"color": colorTri} promptTxt
      setText promptSym promptTxt
      append promptTxt prompt

    -- | Prints out the prompt for the command that the user entered.
    printCommand command prompt output curDir = do
      outLine <- create "<p>"
      dir <- readRef curDir

      setPrompt outLine dir

      appendText command outLine

      append outLine output

    -- | Clears out the command input box.
    clearInput input = setProp "value" "" input

    -- | Updates the prompt to use the new current directory.
    updatePrompt prompt curDir = do
      setText "" prompt

      dir <- readRef curDir

      setPrompt prompt dir

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
      ["help"]      -> runHelp output
      ["man", prog] -> runMan prog output
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
      contents <- showFiles d
      _ <- traverse (\x -> do
                    append x outLine
                    addTab outLine
                    ) contents
      append outLine output

    addTab outLine = do
      space <- create "<span>"
      setText "\t" space
      append space outLine

    -- | Shows the files that are in the given directory.
    showFiles (Dir name files) = traverse showF files
    showFiles file = (\x -> x : Nil) <$> showF file

    showF (Dir name _) = do
      span <- create "<span>"
      setText name span
      css {"color": colorPrimary} span
      css {"font-weight": "bold"} span
      pure span
    showF (File name _) = do
      span <- create "<span>"
      setText name span
      pure span

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

    -- | Prints out the given lines of output.
    printLines lines output = do
      let printLine m = do
                          outLine <- create "<p>"
                          setText m outLine
                          append outLine output
      _ <- traverse printLine lines
      pure unit

    -- | Prints out a message indicating what commands are available.
    runHelp output = printLines helpMessage output

    -- | Prints out a message indicating what commands are available.
    runMan prog output = case prog of
      "clear" -> printLines manClear output
      "ls"    -> printLines manLs    output
      "cd"    -> printLines manCd    output
      "cat"   -> printLines manCat   output
      "help"  -> printLines manHelp  output
      "man"   -> printLines manMan   output
      _       -> printLines (manInvalid prog) output

    -- | Prints out a message indicating that an invalid command was run.
    runInvalidCommand comm output = do
      outLine <- create "<p>"
      setText ("Invalid command: " <> comm) outLine
      append outLine output

colorPrimary :: String
colorPrimary = "#839496"

colorSecondary :: String
colorSecondary = "#b58900"

colorTri :: String
colorTri = "#268bd2"

-- | The symbol used for the terminal prompt.
promptSym :: String
promptSym = " $ "

helpMessage :: List String
helpMessage = (
    "This is a simple terminal emulator which supports the following commands."
  : "clear - clears the terminal window"
  : "ls    - lists the files in the current directory"
  : "cd    - changes the current directory to the specified subdirectory"
  : "cat   - prints the contents of the specified file"
  : "help  - prints this usage information"
  : "man   - prints information on the given command"
  : Nil
  )

manClear :: List String
manClear = (
    "NAME: clear"
  : "SYNOPSIS: clear"
  : "DESCRIPTION: Clears the terminal screen."
  : "EXAMPLES: clear"
  : Nil
  )

manLs :: List String
manLs = (
    "NAME: ls"
  : "SYNOPSIS: ls"
  : "DESCRIPTION: Lists the contents of the current directory. Colors and bolds directories."
  : "EXAMPLES: ls"
  : Nil
  )

manCd :: List String
manCd = (
    "NAME: cd"
  : "SYNOPSIS: cd DIR | cd .."
  : "DESCRIPTION: Changes the current directory to the specified subdirectory, or if '..' is given then it moves up one directory."
  : "EXAMPLES: cd tmp | cd .."
  : Nil
  )

manCat :: List String
manCat = (
    "NAME: cat"
  : "SYNOPSIS: cat FILE"
  : "DESCRIPTION: Prints out the contents of the given file."
  : "EXAMPLES: cd hello.txt"
  : Nil
  )

manHelp :: List String
manHelp = (
    "NAME: help"
  : "SYNOPSIS: help"
  : "DESCRIPTION: Prints out usage information for this terminal emulator."
  : "EXAMPLES: help"
  : Nil
  )

manMan :: List String
manMan = (
    "NAME: man"
  : "SYNOPSIS: man COMMAND"
  : "DESCRIPTION: Prints out usage information for the given command."
  : "EXAMPLES: man ls | man cd | man man"
  : Nil
  )

manInvalid :: String -> List String
manInvalid prog = (
    ("Invalid program: " <> prog)
  : Nil
  )

-- | The files in the simulated file system.
defaultFiles :: File
defaultFiles = Dir "~/" $
    Dir "part1" (
        File "QUESTION" "Which of the following languages has pattern matching? | a) Java | b) C | c) Scala | d) Python |"
      : Dir "a" (
          File "DEADEND" "There is nothing here."
        : Nil
        )
      : Dir "b" (
          File "part1" "This isn't even a link."
        : Nil
        )
      : Dir "c" (
          File "QUESTION" "GNU is short for what? | a) GNU's Not Unix | b) General Neophyte Unix | c) Generative Network Utilities | d) GNU's New Utilities |"
        : Dir "a" (
            File "part1" "http://www.cs.oswego.edu/~cwells2/competitions/resumelee-2017/parts/4/resume.pdf.part1"
          : Nil
          )
        : Dir "b" (
            File "part1" "http://www.cs.oswego.edu/~cwells2/competitions/resumelee-2017/parts/3/resume.pdf.part1"
          : Nil
          )
        : Dir "c" (
            File "DEADEND" "404 Error"
          : Nil
          )
        : Dir "d" (
            File "part1" "http://www.cs.oswego.edu/~cwells2/competitions/resumelee-2017/parts/9/resume.pdf.part1"
          : Nil
          )
        : Nil
        )
      : Dir "d" (
          File "QUESTION" "Tabs or spaces, and how many? | a) 2 tabs | b) 14 spaces | c) 1 tabs + 3 spaces | d) Nothing |"
        : Dir "5" (
            File "This is probably not the correct directory" "null"
          : Nil
          )
        : Dir "24" (
            File "part1" "24 is just a really good number."
          : Nil
          )
        : Dir "37" (
            File "part1" "Most people choose this number, DL said so."
          : Nil
          )
        : Dir "-i" (
            File "part1" "This choice was irrational."
          : Nil
          )
        : Nil
        )
      : Nil
    )
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
