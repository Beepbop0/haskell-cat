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
-- TODO: make use of Data.List.Stream for better efficiency

import System.Environment ( getArgs )
import System.IO
import Data.String ( lines, unlines )
import Data.Char ( toUpper, toLower )
import Data.List ( isPrefixOf, genericLength )

data Result = OK | Error String deriving (Eq)
data NumMode = B | N deriving (Eq)
type Arg = Char
type Args = String

whitespace = "\t\r "

main = do
    cmdargs <- getArgs 
    let f = funcGen dispArgs
        (dispArgs, paths) = filter' ("-" `isPrefixOf`) cmdargs 
    cat <- foldMap readFile paths                   -- concatenate all of the contents of the files together 
    putStr $ f cat

-- fst = a results in true from f
-- snd = a results in false from f
filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' f = foldr g ([], [])
  where g e (x,y) = if f e
                       then (e:x, y)
                       else (x, e:y)

-- given a list of options, compose a function that uses all of those functions combined
funcGen :: [Args] -> (String -> String)
funcGen x 
    | Error x <- hasMeArgs x = error x
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

hasMeArgs :: [Args] -> Result 
hasMeArgs l = case foldr f (Right []) (concat l) of
                Left x -> Error x
                Right x -> OK
    where f :: Char -> Either String String -> Either String String
          f _ (Left x) = Left x
          f c e@(Right l) 
            | c `elem` meArgs = if opp c `elem` l
                                   then Left $ "Error: mutually exclusive args " ++ show c ++ " and " 
                                                ++ (show . opp) c ++ " applied, remove one"
                                   else Right $ c:l
            | otherwise = e
          opp c = case c of
                    'l' -> 'u'
                    'u' -> 'l'
                    'b' -> 'n'
                    'n' -> 'b'
          meArgs = "ulbn"

showTabs :: String -> String
showTabs = foldr (\c a -> case c of
                            '\t' -> '^' : 'I' : a
                            _ -> c : a)
                 []

lineEnds :: [String] -> [String]
lineEnds = map (++"$") 

-- if B is enabled, a blank line doesn't include a line number
lineNum :: NumMode -> ([String] -> [String])
lineNum N = zipWith lineNumFmt [1..]                
lineNum B = f 1
  where f _ [] = []
        f n ([]:xs) =  [] : f n xs
        f n (x:xs) = lineNumFmt n x : f (n+1) xs
                
lineNumFmt :: Int -> String -> String
lineNumFmt n l = pad ++ ns ++ "  " ++ l
  where pad = replicate (6 - nl) ' '
        nl = length ns
        ns = show n

fmt :: String -> (String -> String)
fmt ch = filter (`notElem` ch)

