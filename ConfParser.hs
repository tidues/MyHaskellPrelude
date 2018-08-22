module ConfParser where

import CommonTools
import Data.Ini (Ini, parseIni)
import Data.Text (pack, unpack)

-- |parse config file
confParser :: Content -> Ini
confParser c = let res = parseIni (pack c)
               in  case res of
                    Left str -> error str
                    Right ini -> ini
