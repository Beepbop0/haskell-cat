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
import System.Environment ( getArgs )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TIO 
import Data.Char ( toUpper, toLower )
import Data.List ( isPrefixOf, genericLength, uncons )
import Control.Monad ( foldM )
import Data.Monoid ( (<>) )

data Result = OK | Error String deriving (Eq, Show)
data NumMode = B | N deriving (Eq)
type Args = String
type Argument = Char

main = do
    cmdargs <- getArgs 
    let f = funcGen dispArgs
        (dispArgs, paths) = filter' ("-" `isPrefixOf`) cmdargs 
    cat <- foldMap TIO.readFile paths                   -- concatenate all of the contents of the files together 
    TIO.putStr $ f cat

-- fst = a results in true from f
-- snd = a results in false from f
filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' f = foldr g ([], [])
  where g e (x,y) = if f e 
                       then (e:x, y) 
                       else (x, e:y)

-- given a list of options, compose a function that uses all of those functions combined
funcGen :: [Args] -> (T.Text -> T.Text)
funcGen x 
    | Error x <- hasMeArgs x = error x
    | otherwise = T.unlines . foldr (\s f -> f . func s) id (filter (/= '-') (concat x)) . T.lines
  where 
    func :: Argument -> ([T.Text] -> [T.Text]) 
    func c = case c of 
            'u' -> map $ T.map toUpper
            'l' -> map $ T.map toLower
            'b' -> lineNum B
            'n' -> lineNum N 
            'e' -> lineEnds 
            'T' -> map showTabs 
            'f' -> map $ fmt "\t\r "
            _ -> error "Error: invalid mode. read documentation"

hasMeArgs :: [Args] -> Result 
hasMeArgs l = case findMeArgs (concat l) of
                Left x -> Error x
                _ -> OK

findMeArgs :: String -> Either String String
findMeArgs = foldM evalStack [] 

evalStack :: String -> Char -> Either String String
evalStack l c
    | c `elem` meArgs = if opp c `elem` l
                           then Left $ "Error: mutually exclusive args " ++ show c ++ " and " 
                                                ++ (show . opp) c ++ " applied, remove one"
                           else Right $ c:l
    | otherwise = return l
    where opp c = case c of
            'u' -> 'l'
            'l' -> 'u'
            'b' -> 'n'
            'n' -> 'b'
          meArgs = "ulbn"

showTabs :: T.Text -> T.Text
showTabs = T.replace (T.singleton '\t') (T.pack "^I")

lineEnds :: [T.Text] -> [T.Text]
lineEnds = map $ flip T.snoc '$'

-- if B is enabled, a blank line doesn't include a line number
lineNum :: NumMode -> ([T.Text] -> [T.Text])
lineNum N = zipWith lineNumFmt [1..]                
lineNum B = f 1
  where f :: Int -> [T.Text] -> [T.Text]
        f _ [] = []
        f n s = if T.null h 
                   then h : f n t 
                   else lineNumFmt n h : f (n+1) t
          where Just (h, t) = uncons s
                
lineNumFmt :: Int -> (T.Text -> T.Text)
lineNumFmt n l = TB.toLazyText app <> l
  where app = TB.fromString pad <> TB.fromString ns <> TB.fromString "  " :: TB.Builder
        pad = replicate (6 - genericLength ns) ' '
        ns = show n 

fmt :: String -> (T.Text -> T.Text)
fmt ch = T.filter (`notElem` ch)

