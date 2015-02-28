module Helpers.Common where

import Import
import Yesod.Form.Bootstrap3

glyphicon :: Text -> Widget
glyphicon name = toWidget [hamlet|<span class="glyphicon glyphicon-#{name}">|]

linkToMethod :: Text -> Route App -> [(Text,Text)] -> Text -> Widget
linkToMethod method url attributes content =
    toWidget [whamlet|
    $newline never
    <a href="@{url}" rel="nofollow" data-method="#{method}" *{attributes}>#{content}</a>
    |]

wlinkToMethod :: Text -> Route App -> [(Text,Text)] -> Widget -> Widget
wlinkToMethod method url attributes content =
    toWidget [whamlet|
    $newline never
    <a href="@{url}" rel="nofollow" data-method="#{method}" *{attributes}>^{content}</a>
    |]

postButton :: Text -> Route App -> Widget
postButton text route = linkToMethod "POST" route [("class", "btn btn-default")] text

type ConfirmText = Text
type MethodText = Text
wlinkToMethodConfirm :: MethodText -> ConfirmText -> Route App -> [(Text,Text)] -> Widget -> Widget
wlinkToMethodConfirm method confirmText route attrs
  = wlinkToMethod method route (("data-confirm", confirmText):attrs)

linkToMethodConfirm :: MethodText -> ConfirmText -> Route App -> [(Text,Text)] -> Text -> Widget
linkToMethodConfirm method confirmText route attrs
  = linkToMethod method route (("data-confirm", confirmText):attrs)

currentUser :: Handler (Maybe User)
currentUser = do
  maid <- maybeAuthId
  case maid of
    Just userId -> runDB $ get userId
    Nothing -> return Nothing


submitButton :: MonadHandler m => Text -> AForm m ()
submitButton text = bootstrapSubmit (BootstrapSubmit text "btn-default" [])

bs :: Text -> FieldSettings App
bs = bfs
