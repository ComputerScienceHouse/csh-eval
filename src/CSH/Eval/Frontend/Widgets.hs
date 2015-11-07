{-|
Module      : CSH.Eval.Frontend.Widget
Description : Reusable widgets for use in EvalFrontend pages
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

Defines the web application layer of Evals.
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend.Widgets (
    widgetPanel
  , widgetPanelOffset
  , widgetPanelText
  ) where

import qualified Data.Text as T

import CSH.Eval.Frontend.Data
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Text.Blaze  (preEscapedText)
import Yesod

-- | A helper function to convert integers to text, to make templates that use
-- Integer arguments more readable.
toText :: Integer -> T.Text
toText = T.pack . show

-- | A basic widget for a panel
widgetPanel :: Integer -- horizontal size of the panel
            -> Widget  -- widget for the title of the panel
            -> Widget  -- widget for the body of the panel
            -> Widget
widgetPanel mdsize = widgetPanelOffset mdsize 0

widgetPanelOffset :: Integer -> Integer -> Widget -> Widget -> Widget
widgetPanelOffset mdsize mdoffset title body = $(whamletFile "frontend/templates/widgets/panel.hamlet")

-- | A basic widget for a panel that takes text as arguments instead of widgets
widgetPanelText :: Integer -- horizontal size of the panel
                -> T.Text  -- html for the title of the panel
                -> T.Text  -- html for the body of the panel
                -> Widget
widgetPanelText mdsize title body = $(whamletFile "frontend/templates/widgets/panelText.hamlet")
