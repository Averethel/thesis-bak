module Utils.Iseq (
  Iseq,
  iNil,
  iStr,
  iAppend,
  iNewline,
  iIndent,
  iConcat,
  iInterleave ) where
  data Iseq = 
      INil
    | INewline
    | IStr String
    | IIndent Iseq
    | IAppend Iseq Iseq

  iNil :: Iseq
  iNil = INil

  iStr :: String -> Iseq
  iStr s = case lines s of
    [s] -> IStr s
    ss  -> iInterleave iNewline $ map IStr ss

  iAppend :: Iseq -> Iseq -> Iseq
  iAppend = IAppend

  iNewline :: Iseq
  iNewline = INewline

  iIndent :: Iseq -> Iseq
  iIndent = IIndent

  iConcat :: [Iseq] -> Iseq
  iConcat []     = iNil
  iConcat (s:ss) = s `iAppend` (iConcat ss)

  iInterleave :: Iseq -> [Iseq] -> Iseq
  iInterleave _   []     = iNil
  iInterleave _   [s]    = s
  iInterleave sep (s:ss) = s `iAppend` sep `iAppend` (iInterleave sep ss)

  spaces :: Int -> String
  spaces n = take n $ repeat ' '

  flatten :: Int -> [(Iseq, Int)] -> String
  flatten col []                               = 
    ""
  flatten col ((INil, indent) : seqs)          = 
    flatten col seqs
  flatten col ((INewline, indent) : seqs)      = 
    '\n':spaces indent ++ flatten indent seqs
  flatten col ((IStr s, indent) : seqs)        = 
    s ++ flatten (col + length s) seqs
  flatten col ((IIndent s, indent) : seqs)     = 
    flatten col ((s, col) : seqs)
  flatten col ((IAppend s1 s2, indent) : seqs) = 
    flatten col ((s1, indent):(s2, indent):seqs)

  iDisplay :: Iseq -> String
  iDisplay seq = flatten 0 [(seq, 0)]

  instance Show Iseq where
    show = iDisplay
