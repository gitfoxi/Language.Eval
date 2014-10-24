
Language.Eval
=============

Sometimes you just want to evaluate a string

    λ :set -XOverloadedStrings
    λ import Language.Eval
    λ eval "1+1" []
    2
    λ eval "a+b" [2,3]
    5
    λ eval "a+b * 100" [2,3]
    302
    λ eval "a+b * 100 & 0xf" [2,3]
    14

There's an example app included:

    Eval "1000 & 0xff"

People say that it's okay that Haskell has no `eval` function because it's
easy to write your own. But why bother? Use mine.
