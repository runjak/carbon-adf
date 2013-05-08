{-# LANGUAGE ScopedTypeVariables #-}
module OpenBrain.Website.Routes where

import OpenBrain.Website.Common

route :: OBW Response
route = msum [
    dir "article" $ msum [
      (path $ \(aid :: Integer) -> msum [
        crudRead $ undefined
      , crudUpdate $ undefined
      , crudDelete $ undefined
      ])
    , crudRead $ undefined
    , crudCreate $ undefined
    ]
  , dir "collection" $ msum [
      (path $ \(cid :: Integer) -> path $ \(aid :: Integer) -> msum [
        crudUpdate $ undefined
      , crudDelete $ undefined
      ])
    , (path $ \(cid :: Integer) -> crudRead $ undefined)
    , crudRead $ undefined
    ]
  , dir "description" $ msum [
      (path $ \(did :: Integer) -> msum [
        crudRead $ undefined
      , crudUpdate $ undefined
      , crudDelete $ undefined
      ])
    , crudRead $ undefined
    ]
  , dir "discussion" $ msum [
      (path $ \(did :: Integer) -> msum [
        dir "participate" $ msum [
          crudCreate $ undefined
        , crudDelete $ undefined
        ]
      , dir "weight" $ (path $ \(rid :: Integer) -> crudCreate $ undefined)
      , crudRead $ undefined
      ])
    , crudCreate $ undefined
    , crudRead $ undefined
    ]
  , dir "files" $ undefined
  , dir "login" $ msum [
      crudCreate $ undefined
    , crudUpdate $ undefined
    , crudDelete $ undefined
    ]
  , dir "relation" $ msum [
      (path $ \(rid :: Integer) -> msum [
        crudRead $ undefined
      , crudUpdate $ undefined
      , crudDelete $ undefined
      ])
    , crudRead $ undefined
    , crudCreate $ undefined
    ]
  , dir "result" $ msum [
      (path $ \(rid :: Integer) -> msum [
        dir "vote" $ crudCreate $ undefined
      , crudRead $ undefined
      ])
    , crudRead $ undefined
    ]
  , dir "user" $ msum [
      (path $ \(uid :: Integer) -> msum [
        crudRead $ undefined
      , crudUpdate $ undefined
      , crudDelete $ undefined
      ])
    , crudRead $ undefined
    , crudCreate $ undefined
    ]
--, liftM responseHTML $ S.serveFile return "files/index.html"
  ]
