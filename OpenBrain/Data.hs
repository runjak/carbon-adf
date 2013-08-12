{-|
  This is the Haskell implementation of the Datatypes
  described in the UML-class-diagram from storedData.pdf.
  It is also converted into a .pdf and included into storedData.pdf.
|-}
module OpenBrain.Data(
  module Article
, module Alias
, module CollectionArticle
, module Collection
, module Description
, module Discussion
, module Hash
, module Id
, module Logic
, module Relation
, module Result
, module Salt
, module User
)where

import OpenBrain.Data.Article           as Article
import OpenBrain.Data.Alias             as Alias
import OpenBrain.Data.CollectionArticle as CollectionArticle
import OpenBrain.Data.Collection        as Collection
import OpenBrain.Data.Description       as Description
import OpenBrain.Data.Discussion        as Discussion
import OpenBrain.Data.Hash              as Hash
import OpenBrain.Data.Id                as Id
import OpenBrain.Data.Logic             as Logic
import OpenBrain.Data.Relation          as Relation
import OpenBrain.Data.Result            as Result
import OpenBrain.Data.Salt              as Salt
import OpenBrain.Data.User              as User
