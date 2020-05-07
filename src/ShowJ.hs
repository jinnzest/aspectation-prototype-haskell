module ShowJ
  ( showJ
  ) where

import Data.Aeson (ToJSON, Value(String), encode)
import Data.Aeson.QQ ()
import Data.Aeson.Yaml as Aeson.Yaml
import Data.ByteString.Lazy.UTF8 (toString)

showJ :: ToJSON a => a -> String
showJ v =
  let
    a = toString $ Aeson.Yaml.encode v
    b = toString $ Data.Aeson.encode v
  in a   -- ++"\n" ++ b
