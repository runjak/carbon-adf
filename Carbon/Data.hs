{-|
  This is the Haskell implementation of the Datatypes
  described in the UML-class-diagram from storedData.pdf.
  It is also converted into a .pdf and included into storedData.pdf.
|-}
module Carbon.Data(
  module AcceptanceCondition
, module Alias
, module Article
, module Common
, module Description
, module Discussion
, module Hash
, module Id
, module Item
, module Json
, module Logic
, module Paging
, module Relation
, module Result
, module ResultSet
, module Salt
, module User
)where

import Carbon.Data.AcceptanceCondition as AcceptanceCondition
import Carbon.Data.Alias               as Alias
import Carbon.Data.Article             as Article
import Carbon.Data.Common              as Common
import Carbon.Data.Description         as Description
import Carbon.Data.Discussion          as Discussion
import Carbon.Data.Hash                as Hash
import Carbon.Data.Id                  as Id
import Carbon.Data.Item                as Item
import Carbon.Data.Json                as Json
import Carbon.Data.Logic               as Logic
import Carbon.Data.Paging              as Paging
import Carbon.Data.Relation            as Relation
import Carbon.Data.Result              as Result
import Carbon.Data.ResultSet           as ResultSet
import Carbon.Data.Salt                as Salt
import Carbon.Data.User                as User
