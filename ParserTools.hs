module ParserTools where

import Data.Ini (Ini, parseIni, lookupValue, keys)
import Data.Text (pack, unpack)

-- lookup value by string
strLookup :: String -> String -> Ini -> Either String String
strLookup sec key ini = 
    unpack <$> lookupValue (pack sec) (pack key) ini
