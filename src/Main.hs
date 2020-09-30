{-# OPTIONS_GHC -Wall #-}

import Data.List ( delete, sort, sortOn, uncons, transpose, delete, groupBy )
import Data.Function ( on )
import Data.Maybe ( isNothing, fromJust )
import qualified Data.Bifunctor as BF ( first )
import System.IO.Error ( tryIOError )


import qualified Data.Map.Strict as Map
import Data.Text ( splitOn, pack, unpack, strip )
import System.Random.Shuffle ( shuffle' )
import System.Random ( RandomGen, split, getStdGen )

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

replace :: (Ord a, Eq a) => a -> a -> [a] -> [a]
replace old new = delete old .> (:) new .> sort

shuffle'' :: RandomGen gen => [a] -> gen -> [a]
shuffle'' lst = shuffle' lst (length lst)

tableSize :: Int
tableSize = 10

data Name = Name {firstN :: String, lastN :: String} deriving (Eq, Ord)
instance Show Name where
    show n = firstN n ++ " " ++ lastN n

data Person = 
      Counselor {name :: Name, 
                 t :: Int, 
                 conflicts :: [Name]}
    | Camper {name :: Name, 
              age :: Int, 
              t :: Int, 
              conflicts :: [Name]}
    | Aide {name :: Name, 
            t :: Int, 
            conflicts :: [Name]}
    deriving (Eq)
instance Show Person where
    show (Camper n _ tNum _) = show n ++ ": " ++ show tNum  
    show (Counselor n tNum _) = show n ++ " at " ++ show tNum
    show (Aide n tNum _) = show n ++ "*: " ++ show tNum

data Table = Table {num :: Int, 
                    counselors :: [Person], 
                    aides :: [Person], 
                    aSlots :: Int, 
                    campers :: [Person], 
                    cSlots :: Int} 
                    deriving (Eq)
instance Show Table where
    show (Table n counselorList aideList _ cmprs _ ) = unlines $ ("Table #" ++ show n): concatMap (\(f,s) -> ("\t"++f): map ("\t\t"++) s) [("Counselors:", map (name.>show) counselorList), ("Aides:", map (name.>show) aideList), ("Campers:", map (name.>show) cmprs)]
instance Ord Table where
    compare = compare `on` num
    (<=) = (<=) `on` num

nameFromString :: String -> Name
nameFromString s = let s' = words s in Name (head s') (last s')

allPeople :: Table -> [Person]
allPeople tbl = counselors tbl ++ aides tbl ++ campers tbl

getAllCampers :: [Table] -> [Person]
getAllCampers = concatMap campers

getAllAides :: [Table] -> [Person]
getAllAides = concatMap aides

generateConflicts :: [Name] -> Person -> Person
generateConflicts ppl p = 
    let newConflicts = ppl |> filter (lastN .> (==) (p |> name |> lastN )) |> delete (name p)
    in  p {conflicts = newConflicts} 

generateAllConflicts :: [Person] -> Map.Map Name [Name]
generateAllConflicts ppl = ppl |> map (generateConflicts (map name ppl)) |> filter (conflicts .> null .> not)  |> foldl insertToMap Map.empty
    where
        insertToMap :: Map.Map Name [Name] -> Person -> Map.Map Name [Name]
        insertToMap mp p = if name p `elem` (Map.elems mp |> concat) then mp else Map.insert (name p) (conflicts p) mp

loadConflicts :: Map.Map Name [Name] -> [Person] -> [Person]
loadConflicts cflcts = map helper
    where
        helper p = let mbCf = cflcts |> Map.lookup (name p) in if isNothing mbCf then p else p {conflicts=fromJust mbCf}

createCounselorTables :: [Person] -> [Table]
createCounselorTables = foldl insertCounselor []
    where
        insertCounselor :: [Table] -> Person -> [Table]
        insertCounselor tbls cnslr = 
            case filter (num .> (==) (t cnslr) ) tbls of
                [] -> Table {num= t cnslr, counselors= [cnslr], aides= [], aSlots=0, campers=[], cSlots=0}:tbls
                [tbl] -> replace tbl (tbl {counselors=cnslr:counselors tbl}) tbls
                _ -> error $ "There are two tables with the same number (" ++ show (t cnslr) ++ ")"

addAideSlots :: [Int] -> [Table] -> [Table]
addAideSlots slotsCounts tbls = zip slotsCounts (sort tbls) |> map updateAideSlots
    where
        updateAideSlots (aSlots', tbl) = tbl {aSlots=aSlots'}

addCamperSlots :: [Table] -> [Table]
addCamperSlots = map (\tbl -> tbl {cSlots = tableSize - (tbl |> counselors |> length) - aSlots tbl})

setupTables :: Map.Map Name [Name] -> [Person] -> [Int] -> [Table]
setupTables cflcts cs aSlotLocs = cs |> loadConflicts cflcts |> createCounselorTables |> addAideSlots aSlotLocs |> addCamperSlots

trySeed :: Person -> Table -> Maybe Table
trySeed a@(Aide _ _ cflcts) tbl = 
    if aSlots tbl > 0 && filter (`elem` cflcts) (allPeople tbl |> map name) |> length |> (==0)
        then Just tbl{aides= a{t= num tbl} : aides tbl, aSlots= aSlots tbl - 1}
        else Nothing
    
trySeed c@(Camper _ _ _ cflcts) tbl = 
    if cSlots tbl > 0 && filter (`elem` cflcts) (allPeople tbl |> map name) |> length |> (==0)
        then Just tbl{campers= c{t= num tbl} : campers tbl, cSlots= cSlots tbl - 1}
        else Nothing

trySeed (Counselor n _ _) _ = error $ show n ++ " was attempted to be seeded"

seedPerson :: [Table] -> Person -> Maybe [Table]
seedPerson tbls p = 
    let f = case p of Camper {} -> cSlots; Aide {} -> aSlots; Counselor n _ _ -> error $ show n ++ " was attempted to be seeded"
        attempts = tbls |> sortOn (f) |> reverse |> map (trySeed p) |> dropWhile isNothing
    in
        if null attempts 
            then Nothing 
            else 
                let tbl = attempts |> head |> fromJust
                    oldTbl = tbls |> filter (num .> (==) (num tbl) ) |> head
                in Just $ replace oldTbl tbl tbls

shuffleFoldl :: RandomGen gen => gen -> (b -> a -> b) -> b -> [a] -> b
shuffleFoldl _ _ b [] = b
shuffleFoldl g f b xs =
    let (g1, g2) = split g
        x:rest = shuffle'' xs g1 
    in shuffleFoldl g2 f (f b x) rest

seedTables :: RandomGen gen => gen -> [Person] -> [Person] -> [Table] -> [Table]
seedTables seed aideList cmprs tbls = 
    let 
        (g1, g2) = seed |> split
        start = shuffleFoldl g2 (fromJust .> seedPerson) (Just tbls) aideList
        cmprGroups = cmprs |> sortOn age |> groupBy ((==) `on` age) |> flip shuffle'' g1
    in
        foldl (shuffleFoldl g1 (fromJust .> seedPerson)) start cmprGroups |> fromJust
       

importAides :: FilePath -> IO [Person]
importAides fp = do
    raw <- readFile fp
    raw |> lines |> map importAideLine |> return
    where
        importAideLine = takeWhile (/= ',') .> nameFromString .> flip (`Aide` 0) []

importCounselorTables :: FilePath -> IO ([Person], [Int])
importCounselorTables fp = do
    raw <- readFile fp
    raw |> lines |> zip [1..] |> map importCounselorLine |> unzip |> BF.first concat |> return
    where
        importCounselorLine :: (Int, String) -> ([Person], Int)
        importCounselorLine (tNo, l) = 
            let 
                fields = l |> pack |> splitOn (pack ",") |> map (strip .> unpack)
                allCounselors = fields |> filter (/= "Aide") |> map (\n -> Counselor (nameFromString n) tNo [])
                aslots = fields |> filter (== "Aide") |> length
            in 
                (allCounselors, aslots)

importCampers :: FilePath -> Int -> IO [Person]
importCampers fp numTables = do
    raw <- readFile fp
    raw |> lines |> zip [0..] |> map importCamperLine |> return
    where
        importCamperLine :: (Int, String) -> Person
        importCamperLine (a, l) = l |> takeWhile (/= ',') |> nameFromString |> flip (flip (`Camper` (a `div` numTables) ) 0) []

importConflicts :: FilePath -> IO (Maybe (Map.Map Name [Name]))
importConflicts fp = do
    raw <- tryIOError $ readFile fp
    case raw of
        Left _ -> return Nothing
        Right str -> 
            let
                importConflictLine :: String -> (Name, [Name])
                importConflictLine l = l |> pack |> splitOn (pack ",") |> map (unpack .> nameFromString) |> uncons |> fromJust
            in
                str |> lines |> map importConflictLine |> Map.fromList |> Just |> return 
    

exportConflicts :: FilePath -> Map.Map Name [Name] -> IO ()
exportConflicts fp cflcts = do cflcts |> Map.toList |> map (uncurry (:)) |> map (concatMap (show .> (++ ",") ) .> init) |> unlines |> writeFile fp 

createLookup :: [Table] -> [String]
createLookup tbls = pplLookup (getAllCampers tbls) |> (++ ("" : pplLookup (getAllAides tbls)))
    where
        pplLookup = sortOn (name .> lastN) .> map (\c -> lastN (name c) ++ "," ++ firstN (name c) ++ "," ++ show (t c)) 

createTables :: [Table] -> [String]
createTables tbls = headers : names
    where
        maxLength = tbls |> map (campers .> length) |> maximum
        emptyFill tbl = tbl{ campers =tbl |> campers |> (++ replicate (tbl |> campers |> length |> (maxLength-)) (Camper (Name "" "") 1000 0 []))}

        tbls' = tbls |> sort |> map (\tbl -> tbl {campers= campers tbl |> sortOn age}) |> map emptyFill
        headers = tbls' |> map (num .> show) |> map ("Table " ++) |> concatMap (++ ",") |> init
        names = tbls' |> map (campers .> map name) |> transpose |> map (concatMap (show .> (++ ",")) .> init)

exportTablesAndLookup :: FilePath -> [Table] -> IO ()
exportTablesAndLookup fp tbls = do
    tbls |> createLookup |> unlines |> writeFile ("Lookup-" ++ fp) 
    tbls |> createTables |> unlines |> writeFile ("Tables-" ++ fp)

run :: RandomGen gen => gen -> IO ()
run seedNumber = do
    mbCflcts <- importConflicts "Conflicts.csv"
    (simple_counselors, aSlotLocs) <- importCounselorTables "Counselor Tables.csv"
    simple_aides <- importAides "Aides.csv"
    simple_campers <- importCampers  "Camper List.csv" (length aSlotLocs)
    if isNothing mbCflcts
        then (simple_counselors ++ simple_aides ++ simple_campers) |> generateAllConflicts |> exportConflicts "Conflicts.csv" 
        else 
        let cflcts = fromJust mbCflcts
            ads = loadConflicts cflcts simple_aides
            cmprs = loadConflicts cflcts simple_campers
            in
            setupTables cflcts simple_counselors aSlotLocs |> seedTables seedNumber ads cmprs |> exportTablesAndLookup ".csv"

main :: IO ()
main = do
    seedNumber <- getStdGen
    run seedNumber