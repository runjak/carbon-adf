{-# LANGUAGE OverloadedStrings #-}
module OpenBrain.Website.Template (
    HTML, IsHTML(..), htmlToMu, htmlUnpack, htmlConcat
  , tmpl, gtmpl, emptyContext
) where
{-
  Basics for using Hastache in OpenBrain.
  Hastache provides a template mechanism
  to be used in the OpenBrain.Website.Html package,
  and later maybe also in others.
-}

import Data.Data
import Data.Generics
import Data.String
import Control.Monad
import Happstack.Server as S
import System.Time (CalendarTime)
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LZ
import qualified System.Time           as Time

tmpldir = "files/tmpl/"

newtype HTML = HTML LZ.ByteString

config = defaultConfig {muEscapeFunc = id}

tmpl :: FilePath -> MuContext IO -> IO HTML
tmpl f c = do
  t <- readFile $ tmpldir ++ f
  liftM HTML $ hastacheStr config (encodeStr t) c

gtmpl :: Data a => FilePath -> a -> IO HTML
gtmpl f c = do
  t <- readFile $ "tmpldir" ++ f
  liftM HTML $ hastacheStr config (encodeStr t) $ mkGenericContext c

emptyContext :: MuContext IO
emptyContext s = MuVariable $ B.concat ["Unexpected Variable:\t{{", s, "}}"]

htmlToMu :: HTML -> MuType m
htmlToMu (HTML s) = MuVariable s

htmlUnpack :: HTML -> LZ.ByteString
htmlUnpack (HTML s) = s

class IsHTML h where
 toHTML :: h -> HTML
instance IsHTML HTML where
  toHTML = id
instance IsHTML LZ.ByteString where
  toHTML = HTML

htmlConcat :: IsHTML h => [h] -> HTML
htmlConcat hs = do
  let hs' = map ((\(HTML h) -> h) . toHTML) hs
  HTML $ LZ.concat hs'

instance IsString HTML where
 fromString = HTML . fromString

instance ToMessage HTML where
  toContentType _    = BC.pack "text/html; charset=UTF-8"
  toMessage (HTML h) = h

instance MuVar CalendarTime where
  toLByteString t =
    let f   = fromString . show
        yea = f $ Time.ctYear t
        mon = f . (+1) . fromEnum $ Time.ctMonth t
        day = f $ Time.ctDay  t
        hou = f $ Time.ctHour t
        min = f $ Time.ctMin  t
    in LZ.concat [hou,":",min," ",day,".",mon,".",yea]
  isEmpty = const False
