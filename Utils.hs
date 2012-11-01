module Utils where


justIf c a = if c then Just a else Nothing
justIfNot c a = justIf (not c) a

dropIf c a = if c then Nothing else a

bothEmpty xs ys = null xs && null ys 