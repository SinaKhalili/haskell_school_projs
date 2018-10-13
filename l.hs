
data ME = Num Int
          | Var Char
          | Group ME
          | Sum [ME]
          | Product [ME]
          | Power ME Int
          | Neg ME
          deriving (Show, Ord, Eq)

parseNumberHelp :: [Char] -> Int
parseNumberHelp [] = 0
parseNumberHelp (c:s)
  | c <= '9' && c >= '0' = 1 + parseNumberHelp (s)
  | otherwise = 0

parseNumber :: [Char] -> (ME, [Char])
parseNumber xs = (Num (read $ take (parseNumberHelp xs) xs :: Int),
                              drop (parseNumberHelp xs) xs)
