module Main where

import Prelude hiding (append)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, on, append, css, create, appendText, body, ready, setText, getValue, select)
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

    input <- select "#command"
  
    output <- select "#output"
    --outLine <- create "<p>"
    --append outLine output

    --setText "Terminal opened ..." outLine
  

  --div <- create "<div>"
  --appendText "Your Name: " div
  --append div body
  --log "Hi"
    on "change" (handleCommand input output) input
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
