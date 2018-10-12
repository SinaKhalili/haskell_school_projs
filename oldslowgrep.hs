import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)

--
-- Glushkov representation of Regular expression using Haskell
-- 
-- Autho : Parsa Habibi, Sept 2018


{-TODO:

Part1. 

[]GLushkov: 
	
[x] 1. Linearize the regular expression, by adding a sequential subscript to every symbol 
in the expression. 
	-- we definition of our RE instead of having Ch char, to have Ch char int
	-- I will then recursively break down the RE into a tree 
	-- I will start by doing a post order triversal untill I reach a base case (Ch char int)
	-- Once I have incremented the counter for it, I will continue on with the triversal untill every Ch char int has a number from [0,n]

[x] 2. the States

	-- Start Symbols: The set of Start symbols is just the set of symbols that can occur first in a string matched by the expression.
	-- Final Symbols: The set of Final symbols is just the set of symbols that can occur last in a string matched by the expression.
	-- Pair  Symbols: The set of symbol pairs are the symbols that can appear consecutively in a string matched by the expression.

[] 3. Construct the NFA 
-}

--Regular expression definition 

data RE = Epsilon | Ch Char Int | Seq RE RE | Alt RE RE | Star RE | Group RE deriving Show

--State nodes is the generic type for the list of of states	
data StateNode = StateNode { ch :: Char, num :: Int } deriving Show

--States includes all the 3 states for glushkov, Start, Final and Pairs(which contains a tuple list of state nodes)
data States = States { start :: [StateNode], final :: [StateNode], pairs :: [(StateNode, StateNode)] } deriving Show
 
-- Linearize defintions

linearize :: RE -> Int -> (RE , Int)

--Base case for when we hit a Ch a _ 

linearize (Ch a _) c  = let counter = c + 1
						in ((Ch a counter), counter);

linearize (Seq r1 r2) c = let (left, c1) = linearize r1 c
                              (right, c2) = linearize r2 c1
                             in (Seq left right, c2)

linearize (Alt r1 r2) c = let (left, c1) = linearize r1 c
                              (right, c2) = linearize r2 c1
                             in (Alt left right, c2)

linearize (Star r1) c = let (left, c1) = linearize r1 c
                             in (Star left , c1)

linearize (Group r1) c = let (left, c1) = linearize r1 c
                             in (Group left , c1)

-- End linearize 

-- 2. The States


--Helpers

--merges two arrays together 
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


--makes pairs 
makepairs :: [a] -> [a] -> [(a, a)]
makepairs x y = [(a, b) | a <- x, b <- y]


--Start states
find_start_states :: RE -> [StateNode]

find_start_states (Ch a c) = [StateNode {ch = a, num = c}]
-- find_start_states (Alt r1 r2) = [r1, r2] >>= find_start_states
find_start_states (Alt r1 r2) = let (left) = find_start_states r1
                                    (right) = find_start_states r2
                                   in merge left right
find_start_states (Seq (Star r1) r2) = let (left) = find_start_states r1
                                           (right) = find_start_states r2
                                          in merge left right
find_start_states (Seq Epsilon r2) = find_start_states r2
find_start_states (Seq r1 _) = find_start_states r1
find_start_states (Star r1) = find_start_states r1
find_start_states (Group r1) = find_start_states r1

-- Final States 
find_final_states :: RE -> [StateNode]

find_final_states (Ch a c) = [StateNode {ch = a, num = c}]

find_final_states (Alt r1 r2) = let (left) = find_start_states r1
                                    (right) = find_start_states r2
                                   in merge left right
find_final_states (Seq r1 (Star r2)) = let (left) = find_start_states r1
                                           (right) = find_start_states r2
                                          in merge left right
find_final_states (Seq Epsilon r2) = find_start_states r2
find_final_states (Seq _ r2) = find_start_states r2
find_final_states (Star r1) = find_start_states r1
find_final_states (Group r1) = find_start_states r1

--Pair states

--Merges two RE and a pair , it strictly mergess the first RE since in Alt we only want the pair r1,r2 we cannot have r2,r1
merge_exclusive_pairs :: RE -> RE -> [(StateNode, StateNode)] -> [(StateNode, StateNode)]
merge_exclusive_pairs r1 r2 nodes = let (left) = find_pair_states r1 nodes
                                        (right) = find_pair_states r2 []
                                       in merge left right

-- REcursivley finds the pairs
find_pair_states :: RE -> [(StateNode, StateNode)] -> [(StateNode, StateNode)]
find_pair_states (Ch _ _) nodes = nodes
find_pair_states (Alt r1 r2) nodes = merge_exclusive_pairs r1 r2 nodes
find_pair_states (Seq r1 r2) nodes = let (left) = find_start_states r1
                                         (right) = find_final_states r2
                                         (pairs) = makepairs left right
                                         (exclusive_pairs) = merge_exclusive_pairs r1 r2 nodes
                                       in merge exclusive_pairs pairs
find_pair_states (Star r1) nodes = let (left) = find_start_states r1
                                       (right) = find_final_states r1
                                       (pairs) = makepairs left right
                                      in (find_pair_states r1 pairs)
find_pair_states (Group r1) nodes = find_pair_states r1 nodes



match :: RE -> [Char] -> Bool
splits :: [Char] -> [([Char], [Char])]
combine_all :: Char -> [([Char], [Char])] -> [([Char], [Char])]
match_any_split :: RE -> RE -> [([Char], [Char])] -> Bool
match_any_nonempty_split :: RE -> RE -> [([Char], [Char])] -> Bool

match Epsilon s = s == ""
match (Ch a _) "" = False
match (Ch a _) (c : more_chars) = a == c && more_chars == []
match (Alt r1 r2) string = match r1 string || match r2 string
match (Seq r1 r2) string = match_any_split r1 r2 (splits string)
match (Star r1) "" = True
match (Star r1) s = match_any_nonempty_split r1 (Star r1) (splits s)
match (Group r1) s = match r1 s

splits "" = [("", "")]
splits (c : chars) = combine_all c (splits chars)



combine_all c [] = []
combine_all c (("", s): more) = ([c], s) : ("", c:s) : (combine_all c more)
combine_all c ((a:as, s): more) = (c:a:as, s) : (combine_all c more)

match_any_split r1 r2 [] = False
match_any_split r1 r2 ((s1, s2) : more_splits) 
   | match r1 s1 && match r2 s2     = True
   | otherwise                      = match_any_split r1 r2 more_splits 

match_any_nonempty_split r1 r2 [] = False
match_any_nonempty_split r1 r2 ((s1, s2) : more) 
   | s1 /= "" && match r1 s1 && match r2 s2     = True
   | otherwise                                  = match_any_nonempty_split r1 r2 more 

-- 3.  A parser to convert text into regular expressions

-- * THE way that this search works is like so: 
-- ? The different reserved symbols are only 'known' by their own functions
-- ? The only function that knows all the symbols is the very last, deepest layer, parseChar
-- ! which only knows to *not* use those symbols - nothing else
-- ? so there are three ways of catching the symbols: 
-- ? on the way UP  - OR - on the way DOWN - or - after a NOTHING gets propped to you (still on way up)
-- ? TO catch on the way DOWN you want to grab it before it gets a nothing propogated
-- ? >> This is how parse element catches '(' 
-- ? >> This is also how the pipe '|' is caught by extendRE
-- ? TO catch on the way UP there are two ways: 
-- ? 1. You are anticipating your symbol isn't called and you allow to proceed catch on up 
-- ? Here, parseItem basically does that - it catches stars (*) on their way up anticipating its not first elem
-- ? 2. OR you anticipate a NOTHING getting all the way back to you with the intention of reading chars again
-- ? This is how the right brace ')' or the group catches it by calling parseRE and getting it propped back
-- ? by way of extendRE (extendRE and extendSeq actually is a critical part of this operation because
-- ? they're the functions with *history* i.e. they can take previous inputs and put them in their output)
-- ? conversly the other functions only take [chars] and return (RE, [char]) 
-- ? 

{- ? a-}
parseRE :: [Char] -> Maybe (RE, [Char])         {- Parse till stop-}
extendRE :: (RE, [Char]) -> Maybe (RE, [Char]) {- This is called from parseRE as the thing to call after
                                                successful call to parse seq. Idea here is to catch any 
                                                pipes '|' since this is the only function that knows 
                                                what those are.-}

extendSeq :: (RE, [Char]) -> Maybe (RE, [Char]) {- This is called to chain loop sequences until done
                                                    done by calling parseItem on elements. Done until a 
                                                    Nothing is propped or done. If nothing is propped it
                                                    just returns up to the point propped as re and re
                                                    rest as chars-}
parseSeq :: [Char] -> Maybe (RE, [Char])    {-  parseItem is called  on elem, giving us another single
                                        parse - remaining letters combo but now we call extendSeq to see if 
                                            sequence can be made rest is -}
parseItem :: [Char] -> Maybe (RE, [Char])    {- Checks for star, basically calls below and *then* sees if 
                                                next char is a star, if it is it adds the star in without
                                                pushing it all the way down   -}
parseElement :: [Char] -> Maybe (RE, [Char]) -- Checks for groups, if none, passes it to parse char
                                                -- if found, it passes rest of expr to parseRE and groups
                                                -- *gives nothing on missing right bracket, propogates
parseChar :: [Char] -> Maybe (RE, [Char]) -- Parses for allowed character

parseChar [] = Nothing
parseChar ('\\':c:s) = Just ((Ch c 0), s)
parseChar (c:s)
  | c == '|' || c == '*' || c == '(' || c == ')'   = Nothing
  | otherwise                                      = Just ((Ch c 0), s)

parseElement ('(':more) =
    case parseRE(more) of
        Just (re, ')':yet_more) -> Just(Group re, yet_more)
        _ -> Nothing
parseElement s = parseChar s

parseItem s =
   case parseElement(s) of
        -- Just (re, '\\':c:s) -> Just ((Ch c), s)  
        Just (re, '*':more) -> Just (Star re, more)
        Just (re, more) -> Just (re, more)
        _ -> Nothing

extendSeq :: (RE, [Char]) -> Maybe (RE, [Char])

parseSeq s =
    case parseItem(s) of
        Just (r, more_chars) -> extendSeq(r, more_chars)
        _ -> Nothing

extendSeq (e1, after1) =
    case parseItem(after1) of 
        Just(e2, more) -> extendSeq(Seq e1 e2, more)
        _ -> Just(e1, after1)

parseRE s =
    case parseSeq(s) of
        Just (r, more_chars) -> extendRE(r, more_chars)
        _ -> Nothing

extendRE (e1, []) = Just (e1, [])
extendRE (e1, '|' : after_bar) =
    case parseSeq(after_bar) of 
        Just(e2, more) -> extendRE(Alt e1 e2, more)
        _ -> Nothing
extendRE(e1, c:more) = Just (e1, c:more)

parseMain :: [Char] -> Maybe RE

parseMain s = case parseRE s of 
    Just (e, []) -> Just e
    _ -> Nothing

-- 4.  Searching for matching lines in a file

matches :: RE -> [[Char]] -> [[Char]]
matches re lines = filter (match re) lines

matching :: [Char] -> [[Char]] -> [[Char]]
matching regexp lines = case parseMain regexp of
                            Just r -> matches r lines
                            _ -> []

-- 5.  Command line interface

main = do
  [regExp, fileName] <- getArgs
  srcText <- readFile fileName
  hPutStr stdout (unlines (matching regExp (lines srcText)))





