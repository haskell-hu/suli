{-
Modul verses szövegek verslábakra bontásához.
Lásd https://hu.wikipedia.org/wiki/Időmértékes_verselés.
-}
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

-- | Időmértékes szótagokra tagolja az adott szöveget.
--
-- Feltételezi, hagy a szöveg legfeljebb egy verssor (mivel sorok között nincs
-- értelme átvinni ezt a fajta szótagot).
--
-- Wikipédiáról:
--     "A szótag az időmértékes versritmusban a következő magánhangzóig terjed,
--      függetlenül attól, hogy a következő magánhangzó ugyanabban a szóban vagy
--      a rá következőben található.""
idoSzotagol :: Text -> [Text]
idoSzotagol szoveg =
    let szokozNelkuli = T.filter nemSzokoz szoveg
    in tagol szokozNelkuli ""
  where
    nemSzokoz :: Char -> Bool
    nemSzokoz c = c /= ' '
    -- | Időalapú szótagokra bontja az adott szóköz nélküli szöveget.
    -- Rekurzió során sorra veszi a szöveg karatereit, és vagy bővíti az
    -- aktuális szótagot, vagy lezárja azt, ha új szótag magánhangzójával
    -- találkozik.
    tagol
        :: Text  -- ^ A szöveg maradéka.
        -> Text  -- ^ Az aktuális szótag gyűjtője.
        -> [Text]
    tagol s aktualis = case T.uncons s of
        Nothing ->
            -- Elfogyott a szöveg, szótag ami a gyűjtőben van.
            [aktualis]
        Just (k, tobbi) ->
            -- Ha mhzóval találkozunk, és már van mhzó a szótagunkban, lezárjuk
            -- a szótagot és újat kezdünk. Különben bővítjük a gyűjtőt.
            if maganhangzo k && T.any maganhangzo aktualis
                then aktualis : tagol s ""
                else tagol tobbi (T.snoc aktualis k)
    -- | Igaz, ha az adott betű magánhangzó.
    maganhangzo :: Char -> Bool
    maganhangzo c = elem c maganhangzok
