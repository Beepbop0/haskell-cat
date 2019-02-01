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
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C ( 
    pack, 
    lines, 
    unlines, 
    snoc, 
    cons, 
    foldr, 
    filter, 
    map, 
    replicate )
import Data.Char ( toUpper, toLower )
import Data.List ( isPrefixOf, genericLength )

data Result = OK | Error String deriving (Eq)
data NumMode = B | N deriving (Eq)
type Args = String
type Argument = Char

main = do
    cmdargs <- getArgs 
    let f = funcGen dispArgs
        (dispArgs, paths) = filter' ("-" `isPrefixOf`) cmdargs 
    cat <- foldMap BS.readFile paths                   -- concatenate all of the contents of the files together 
    BS.putStr $ f cat

-- fst = a results in true from f
-- snd = a results in false from f
filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' f = foldr g ([], [])
  where g e (x,y) = if f e
                       then (e:x, y)
                       else (x, e:y)

-- given a list of options, compose a function that uses all of those functions combined
funcGen :: [Args] -> (BS.ByteString -> BS.ByteString)
funcGen x 
    | Error x <- hasMeArgs x = error x
    | otherwise = C.unlines . foldr (\s f -> f . scan s) id x . C.lines
  where 
    scan :: Args -> ([BS.ByteString] -> [BS.ByteString])
    scan s = case s of
            ('-':o) -> foldr (\c f -> f . func c) id o
            _ -> error "Error: provide arguments in a POSIX manner"
    func :: Argument -> ([BS.ByteString] -> [BS.ByteString]) 
    func c = case c of 
            'u' -> map $ C.map toUpper
            'l' -> map $ C.map toLower 
            'b' -> lineNum B
            'n' -> lineNum N 
            'e' -> lineEnds 
            'T' -> map showTabs 
            'f' -> map $ fmt "\t\r "
            _ -> error "Error: invalid mode. read documentation"

hasMeArgs :: [Args] -> Result 
hasMeArgs l = case foldr f (Right []) (concat l) of
                Left x -> Error x
                _ -> OK
    where f :: Char -> Either String String -> Either String String
          f c (Right l) 
            | c `elem` meArgs = if opp c `elem` l
                                   then Left $ "Error: mutually exclusive args " ++ show c ++ " and " 
                                                ++ (show . opp) c ++ " applied, remove one"
                                   else Right $ c:l
          f _ e = e
          opp c = case c of
                    'u' -> 'l'
                    'l' -> 'u'
                    'b' -> 'n'
                    'n' -> 'b'
          meArgs = "ulbn"

showTabs :: BS.ByteString -> BS.ByteString
showTabs = C.foldr (\c a -> case c of
                            '\t' -> '^' `C.cons` 'I' `C.cons` a
                            _ -> c `C.cons` a)
                 BS.empty

lineEnds :: [BS.ByteString] -> [BS.ByteString]
lineEnds = map $ flip C.snoc '$'

-- if B is enabled, a blank line doesn't include a line number
lineNum :: NumMode -> ([BS.ByteString] -> [BS.ByteString])
lineNum N = zipWith lineNumFmt [1..]                
lineNum B = f 1
  where f :: Int -> [BS.ByteString] -> [BS.ByteString]
        f _ [] = []
        f n s = if h == BS.empty
                   then BS.empty : f n t
                   else lineNumFmt n h : f (n+1) t
            where h = head s
                  t = tail s
                
lineNumFmt :: Int -> (BS.ByteString -> BS.ByteString)
lineNumFmt n l = pad `BS.append` C.pack ns `BS.append` C.pack "  " `BS.append` l
  where pad = C.replicate (6 - genericLength ns) ' '
        ns = show n

fmt :: String -> (BS.ByteString -> BS.ByteString)
fmt ch = C.filter (`notElem` ch)

