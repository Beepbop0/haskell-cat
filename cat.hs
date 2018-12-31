-- simple UNIX cat program
-- optional second argument can be used to tranform output
-- u = uppercase
-- l = lowercase
-- f = filter whitespace
-- these features are actually from unix cat
-- n = line numbers
-- b = line numbers where blank lines aren't numbered
-- e = show "ends" (newlines) with "$"
-- T = show tabs with "^I"
-- TODO: make use of Data.List.Stream for better effeciency

import System.Environment ( getArgs )
import System.IO
import Data.String ( lines, unlines )
import Data.Char ( toUpper, toLower )
import Data.List ( isPrefixOf, genericLength )

data NumMode = B | N deriving (Eq)
type Arg = Char
type Args = String

whitespace = "\t\r "

main = do
    cmdargs <- getArgs 
    let f = funcGen dispArgs
        dispArgs = filter isArg cmdargs 
        paths = filter (not . isArg) cmdargs 
    cat <- foldMap readFile paths -- concatenate all of the contents of the files together
    putStr $ f cat

isArg = isPrefixOf "-"

-- given a list of options, compose a function that uses all of those functions combined
funcGen :: [Args] -> (String -> String)
funcGen x 
    | hasMeArgs x = error "Error: mutually exclusive arguments provided"
    | otherwise = unlines . foldr (\s f -> f . scan s) id x . lines
  where 
    scan :: Args -> ([String] -> [String])
    scan s = case s of
            ('-':o) -> foldr (\c f -> f . func c) id o
            _ -> error "Error: provide arguments in a POSIX manner"
    func :: Arg -> ([String] -> [String]) 
    func c = case c of 
            'u' -> map $ map toUpper
            'l' -> map $ map toLower
            'b' -> lineNum B
            'n' -> lineNum N 
            'e' -> lineEnds 
            'T' -> map showTabs 
            'f' -> map $ fmt whitespace
            _ -> error "Error: invalid mode. read documentation"

hasMeArgs :: [Args] -> Bool
hasMeArgs l = let f :: Char -> Maybe String -> Maybe String
                  f _ Nothing = Nothing
                  f c m@(Just l) 
                    | c `elem` meArgs = if opp c `elem` l
                                           then Nothing
                                           else Just (c:l)
                    | otherwise = m
                  opp 'l' = 'u'
                  opp 'u' = 'l'
                  opp 'b' = 'n'
                  opp 'n' = 'b'
                  meArgs = "ulbn"
               in case foldr f (Just []) (concat l) of
                    Nothing -> True
                    _ -> False

showTabs :: String -> String
showTabs = foldr (\c a -> case c of
                            '\t' -> '^' : 'I' : a
                            _ -> c : a)
                 []

-- (this function assumes the string has lines applied to it)
lineEnds :: [String] -> [String]
lineEnds = map (++"$") 

-- if B is enabled, a blank line doesn't include a line number
lineNum :: NumMode -> ([String] -> [String])
lineNum N = zipWith lineNumFmt [1..]                
lineNum B = f 1
  where f _ [] = []
        f n ([]:xs) =  [] : f n xs
        f n (x:xs) = lineNumFmt n x : f (n+1) xs
                
-- format of each line
lineNumFmt :: Int -> String -> String
lineNumFmt n l = pad ++ ns ++ "  " ++ l
  where pad = replicate (6 - nl) ' '
        nl = length ns
        ns = show n

-- filter a given list of characters
fmt :: String -> (String -> String)
fmt ch = filter (`notElem` ch)

