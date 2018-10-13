



-- *OBJECTIVE : Create a symbolic derivative evaluator. This is done by accomplishing these tasks:
---                    (1) Parser for mathamatical expression
---                    (2) Symbolic Derivative Function (for the parsed expression)
---                    (3) Unparser for the algebraic expression
{-
                        IF wanted: Can create a Simplifier using the inside-out method
-}

data ME = Num Int
          | Var Char
          | Group ME
          | Sum [ME]
          | Product [ME]
          | Power ME Int
          | Neg ME
          deriving (Show, Ord, Eq)

--- * When parsing a+b-c+d, you should produce Sum[Var "a", Var "b", Neg (Var "c"), Var "d"]

-- This parse function only calls on another function called "parse main"
-- the second element of tuple must be empty to show that we parsed all elements
parseME :: [Char] -> Maybe ME
parseElement :: [Char] -> Maybe (ME, [Char])

parseNumberHelp :: [Char] -> [Char]
parseNumberHelp [] = []
parseNumberHelp (c:s)
  | c <= '9' && c >= '0' = 1 + parseNumberHelp (s)
  | otherwise = []

parseNumber :: [Char] -> (ME, [Char])
parseNumber xs = (Num (read $ take (parseNumberHelp xs) xs :: Int),
                              drop (parseNumberHelp xs) xs)

-- This is the absolute base case - it can take in a variable (only one) and len 1 or any len nums
-- TODO: Get this method working properly (variables of len 1 and numbers only) <- how to check?
parseChar :: [Char] -> Maybe (ME, [Char])
parseChar [] = Nothing
parseChar (c:s)
  | c == '+' || c == '*' || c == '(' || c == ')' || c == '-'   = Nothing
  | c <= '9' && c >= '0'    = Just parseNumber  c:s  --- Looks like its all symbolic, so no need nums
  | otherwise               = Just ((Var c ), s)


-- This function parses until it stops at a unknown (to it) character. Its Nothing cond
-- only reached if first not match - essentially "parseTillStop" ing on the first element -> Nothing
parseTillStop :: [Char] -> Maybe (ME, [Char])



parseToGroup :: [Char] -> Maybe (ME, [Char])
keepParsing :: [Char] -> Maybe (ME, [Char])

-- * Notice that it only accepts an empty array in the tuple - this catches nothings propped
parseME s = case parseTillStop s of
    Just (me, []) -> Just me
    _ -> Nothing

parseTillStop s =
    case parseToGroup(s) of
        Just (me, more_chars) -> extendTheGroup(me, more_chars)
        _ -> Nothing

extendTheGroup (e1, after1) =
    case parseItem(after1) of
        Just(e2, more) -> extendTheGroup(Group e1 e2, more)
        _ -> Just(e1, after1)


keepParsing (e1, []) = Just (e1, [])
keepParsing (e1, '|' : after_bar) =
    case parseAnother(after_bar) of
        Just(e2, more) -> keepParsing(Alt e1 e2, more)
        _ -> Nothing
keepParsing(e1, c:more) = Just (e1, c:more)

parseElement ('(':more) =
    case parseME(more) of
        Just (me, ')':yet_more) -> Just(Group me, yet_more)
        _ -> Nothing
parseElement s = parseChar s





unparseME :: ME -> [Char]




deriv :: ME -> Char -> ME

simplifyME :: ME -> ME
simplifyME e = e


-- Main Program

parse_deriv_simp_unparse v s =
  case parseME s of
    Just e -> Just (unparseME (simplifyME (deriv e v)))
    _ -> Nothing

main = do
  [[v], expr] <- getArgs
  case parse_deriv_simp_unparse v expr of
    Just s -> hPutStrLn stdout s
    _ -> hPutStrLn stdout "Parse failure"
