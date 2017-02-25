module Main where

import Prelude hiding (append)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, on, append, css, create, appendText, body, ready, setText, getValue, select, getProp, setAttr, attr, setProp, remove)
import Control.Monad.Eff.Ref (newRef, REF, readRef)
import Control.Monad.Except (runExcept)
import Data.Foreign.Class (read)
import Data.Foldable (for_)
import Data.String.Utils (words)
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

    let dir = "~/"
    curDir <- newRef dir
    setText (dir <> promptSym) prompt

    on "change" (handleCommand prompt input output curDir) cmdForm
  where
--    handleCommand :: JQuery
--      -> JQuery
--      -> JQueryEvent
--      -> JQuery
--      -> Eff ( dom :: DOM
--             , console :: CONSOLE
--             | eff
--             ) Unit
    handleCommand prompt input output curDir _ _ = unsafePartial do
      val <- getValue input
      for_ (runExcept (read val)) \command -> do
        outLine <- create "<p>"
        dir <- readRef curDir
        setText (dir <> promptSym <> command) outLine
        setText (dir <> promptSym) prompt
        append outLine output
        setProp "value" "" input
        runCommand command input output
        scrollVal <- getProp "scrollHeight" output
        for_ (runExcept (read scrollVal)) \(scrollH :: Int) -> do
          setProp "scrollTop" scrollH output
    runCommand comm input output = case words comm of
      ["clear"] -> do
        outLines <- select "#output p"
        remove outLines
      _ -> do
        outLine <- create "<p>"
        setText ("Invalid command: " <> comm) outLine
        append outLine output

promptSym :: String
promptSym = " $ "
