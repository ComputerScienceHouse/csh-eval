{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module CSH.Eval.Frontend where

import Yesod

data EvalFrontend = EvalFrontend

mkYesod "EvalFrontend" [parseRoutes|
/ HomeR GET
|]

instance Yesod EvalFrontend

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

