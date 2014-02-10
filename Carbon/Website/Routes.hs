module Carbon.Website.Routes where

import Carbon.Website.Common
import qualified Carbon.Website.Composition as Composition
import qualified Carbon.Website.Files as Files
import qualified Carbon.Website.Item as Item
import qualified Carbon.Website.Login as Login
import qualified Carbon.Website.Paging as Paging
import qualified Carbon.Website.User as User

route :: OBW Response
route = msum [
    dir "item" $ msum [
      path $ \iid -> msum [
        dir "fitinstance" . crudCreate $ Item.fitInstance iid
      , dir "acs" . crudRead $ Item.acs iid
      , dir "evaluate" . crudRead $ Item.evaluate iid
      , crudRead $ Item.readItem iid
      , crudUpdate $ Item.updateItem iid
      , crudDelete $ Item.deleteItem iid
      ]
    , crudCreate Item.createItem
    , Paging.pageItems
    ]
  , dir "files" Files.serve
  , dir "login" $ msum [
      crudCreate Login.login
    , crudDelete Login.logout
    ]
  , dir "user" $ msum [
      path $ \uid -> msum [
        crudRead $ User.readUser uid
      , crudUpdate $ User.updateUser uid
      , crudDelete $ User.deleteUser uid
      ]
    , crudRead User.pageUsers
    , crudCreate User.createUser
    ]
  , Composition.serve
  ]
