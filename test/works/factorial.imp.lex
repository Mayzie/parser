Just
  [ Kfun
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "cond"
  , KComma
  , KId "tmp"
  , KBraceBra
  , KId "cond"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "=="
  , KNum 0
  , KRoundKet
  , KSemi
  , Kif
  , KId "cond"
  , Kthen
  , KBraceBra
  , KId "tmp"
  , KEquals
  , KNum 1
  , KSemi
  , Kreturn
  , KId "tmp"
  , KSemi
  , KBraceKet
  , Kelse
  , KBraceBra
  , KId "tmp"
  , KEquals
  , KRoundBra
  , KId "n"
  , KOp "-"
  , KNum 1
  , KRoundKet
  , KSemi
  , KId "tmp"
  , KEquals
  , KRoundBra
  , KId "factorial"
  , KRoundBra
  , KId "tmp"
  , KRoundKet
  , KOp "*"
  , KId "n"
  , KRoundKet
  , KSemi
  , Kreturn
  , KId "tmp"
  , KSemi
  , KBraceKet
  , KBraceKet
  , Kfun
  , KId "main"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , Kvars
  , KId "tmp"
  , KBraceBra
  , KId "tmp"
  , KEquals
  , KId "factorial"
  , KRoundBra
  , KId "n"
  , KRoundKet
  , KSemi
  , Kreturn
  , KId "tmp"
  , KSemi
  , KBraceKet
  ]
