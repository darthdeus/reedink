module Helpers.Common where

import Import

glyphicon :: Text -> Widget
glyphicon name = toWidget [hamlet|<span class="glyphicon glyphicon-#{name}">|]

tlinkToMethod :: Text -> Route App -> [(Text,Text)] -> Text -> Widget
tlinkToMethod method url attributes content =
    toWidget [whamlet|
    $newline never
    <a href="@{url}" rel="nofollow" data-method="#{method}" *{attributes}>#{content}</a>
    |]

linkToMethod :: Text -> Route App -> [(Text,Text)] -> Widget -> Widget
linkToMethod method url attributes content =
    toWidget [whamlet|
    $newline never
    <a href="@{url}" rel="nofollow" data-method="#{method}" *{attributes}>^{content}</a>
    |]

type ConfirmText = Text
type MethodText = Text
linkToMethodConfirm :: MethodText -> ConfirmText -> Route App -> [(Text,Text)] -> Widget -> Widget
linkToMethodConfirm method confirmText route attrs
  = linkToMethod method route (("data-confirm", confirmText):attrs)

tlinkToMethodConfirm :: MethodText -> ConfirmText -> Route App -> [(Text,Text)] -> Text -> Widget
tlinkToMethodConfirm method confirmText route attrs
  = tlinkToMethod method route (("data-confirm", confirmText):attrs)

currentUser :: Handler (Maybe User)
currentUser = do
  maid <- maybeAuthId
  case maid of
    Just userId -> runDB $ get userId
    Nothing -> return Nothing
