{-# LANGUAGE OverloadedStrings #-}
module Suli.Nyelv.Verslab where

import Data.Text (Text)
import qualified Data.Text as T

rovidMaganhangzok :: [Char]
rovidMaganhangzok =
    [ 'a'
    , 'i'
    , 'u', 'ü'
    , 'e'
    , 'o', 'ö'
    ]

hosszuMaganhangzok :: [Char]
hosszuMaganhangzok =
    [ 'á'
    , 'í'
    , 'ú', 'ű'
    , 'é'
    , 'ó', 'ő'
    ]

maganhangzok :: [Char]
maganhangzok = rovidMaganhangzok ++ hosszuMaganhangzok

idoSzotagol :: Text -> [Text]
idoSzotagol szo =
    let szokozNelkuli = T.filter nemSzokoz szo
    in tagol szokozNelkuli ""
  where
    nemSzokoz :: Char -> Bool
    nemSzokoz c = c /= ' '
    --
    tagol :: Text -> Text -> [Text]
    tagol m aktualis = case T.uncons m of
        Nothing -> [aktualis]
        Just (k, tobbi) ->
            if maganhangzo k && T.any maganhangzo aktualis
                then aktualis : tagol m ""
                else tagol tobbi (T.snoc aktualis k)
    --
    maganhangzo :: Char -> Bool
    maganhangzo c = elem c maganhangzok
