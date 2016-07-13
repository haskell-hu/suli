# Haskell Suli

Haskell Suli - érdekességek az iskolapadból.

Vonatkozó cikkeket keresd a [haskell.hu](http://haskell.hu)-n.

## Futtatás

Tedd fel a [stack](http://haskellstack.org) tool-t.
Nyiss egy terminált / parancssort ott ahova klónoztad / letöltötted a repót (
ahol a suli.cabal file van).
Majd a terminálba írhatod:
 - `stack test` - futtatja a teszteket (lásd Spec.hs).
 - `stack build` - csak fordítja a programot, nem tesztel.
 - `stack exec suli-fut` - ha lefordítottad, így futtathatod.

## Link néhány függőség doksijára

Hackage
 - [Text](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html)
 - [Tasty HUnit](https://hackage.haskell.org/package/tasty-hunit-0.9.2/docs/Test-Tasty-HUnit.html)
