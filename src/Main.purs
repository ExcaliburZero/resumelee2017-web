module Main where

import Prelude hiding (append)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, on, append, css, create, appendText, body, ready, setText, getValue, select, getProp, setAttr, attr, setProp, remove)
import Control.Monad.Except (runExcept)
import Data.Foreign.Class (read)
import Data.Foldable (for_)
import DOM (DOM)
import Partial.Unsafe (unsafePartial)

main :: forall eff. Eff ( dom :: DOM
                        , console :: CONSOLE
                        | eff
                        ) Unit
main = ready $ do
    body <- body

    log "Script imported"

    cmdForm <- select "#commandForm"
    input  <- select "#command"
    output <- select "#output"

    on "change" (handleCommand input output) cmdForm
  where
    handleCommand :: JQuery
      -> JQuery
      -> JQueryEvent
      -> JQuery
      -> Eff ( dom :: DOM
             , console :: CONSOLE
             | eff
             ) Unit
    handleCommand input output _ _ = unsafePartial do
      val <- getValue input
      for_ (runExcept (read val)) \command -> do
        outLine <- create "<p>"
        setText ("~> " <> command) outLine
        append outLine output
        setProp "value" "" input
        scrollVal <- getProp "scrollHeight" output
        for_ (runExcept (read scrollVal)) \(scrollH :: Int) -> do
          setProp "scrollTop" scrollH output
        runCommand command input output
    runCommand comm input output = case comm of
      "clear" -> do
        outLines <- select "#output p"
        remove outLines
      _ -> do
        outLine <- create "<p>"
        setText ("Invalid command: " <> comm) outLine
        append outLine output
