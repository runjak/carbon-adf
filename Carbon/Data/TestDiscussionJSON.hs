{-# LANGUAGE OverloadedStrings #-}
module Carbon.Data.TestDiscussionJSON where

import Data.ByteString.Lazy.Internal (ByteString)
import Data.Monoid
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Internal as ByteString
import qualified Data.Maybe as Maybe

import Carbon.Data.Discussion

decode :: ByteString -> Maybe (Discussion String)
decode = Aeson.decode

test :: String -> ByteString -> IO ()
test n c = let success = putStrLn ("Decoding "++n++" worked :)")
               failed  = do
                  putStrLn ("Failed to decode "++n++"!")
                  putStr "Content was:" >> print c
           in maybe failed (const success) $ decode c

case1, case2 :: ByteString
case1 = Aeson.encode (mempty :: Discussion String)
case2 = "{\"arguments\":{\"Right\":[]},\"participants\":[]}"

main = mapM_ (uncurry test) [("case1", case1)
                            ,("case2", case2)]
