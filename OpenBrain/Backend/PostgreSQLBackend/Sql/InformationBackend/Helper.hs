module OpenBrain.Backend.PostgreSQLBackend.Sql.InformationBackend.Helper where

import OpenBrain.Backend.PostgreSQLBackend.Common hiding (clone)
import OpenBrain.Data

{-
  Produces a new Information with the same author, title, description and mediaid
  as the original. Source- and Targetrelations are also cloned.
  Clone is not a transaction on purpose.
-}
clone :: (IConnection conn) => conn -> InformationId -> IO InformationId
clone conn iid = do
  let target = toSql $ toId iid
  -- Creating clone for current target:
  mkChild <- prepare conn $ "INSERT INTO \"Information\" (author, title, description, mediaid) "
                         ++ "SELECT author , title , description , mediaid "
                         ++ "FROM \"Information\" WHERE informationid = ?"
  execute mkChild [target]
  [[clone]] <- quickQuery' conn "SELECT LASTVAL()" []
  -- Copy relations of target to clone
  cSourceRelations <- prepare conn $ "INSERT INTO \"Relations\" (comment, type, source, target) "
                                  ++ "SELECT comment, type, ?, target "
                                  ++ "FROM \"Relations\" WHERE source = ?"
  cTargetRelations <- prepare conn $ "INSERT INTO \"Relations\" (comment, type, source, target) "
                                  ++ "SELECT comment, type, source, ? "
                                  ++ "FROM \"Relations\" WHERE target = ?"
  execute cSourceRelations [clone, target]
  execute cTargetRelations [clone, target]
  -- Mark clone as child of parent:
  mkChild <- prepare conn "INSERT INTO \"Relations\" (comment, type, source, target) VALUES ('', ?, ?, ?)"
  execute mkChild [toSql Parent, target, clone]
  -- And it's done:
  return . fromId $ fromSql clone

