import Data.List ( foldl, delete, sort, group, sortOn, uncons, transpose )
import Data.Function ( on )
import Data.Maybe ( isNothing, fromJust, catMaybes )
import System.Environment ( getArgs )
import qualified Data.Bifunctor as BF ( first )
import System.IO.Error ( tryIOError )
import Data.Foldable ( Foldable(toList) ) 


import qualified Data.Map.Strict as Map
import Data.Sequence ( chunksOf, fromList )
import Data.Text ( splitOn, pack, unpack, strip )
import System.Random.Shuffle ( shuffle' )
import System.Random ( RandomGen, split, mkStdGen )

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

replace :: (Ord a, Eq a) => a -> a -> [a] -> [a]
replace old new = delete old .> (:) new .> sort

shuffle'' :: RandomGen gen => [a] -> gen -> [a]
shuffle'' lst = shuffle' lst (length lst)

tableSize :: Int
tableSize = 20

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
    show (Camper n _ t _) = show n ++ ": " ++ show t  
    show (Counselor n t _) = show n ++ " at " ++ show t
    show (Aide n t _) = show n ++ "*: " ++ show t

data Table = Table {num :: Int, 
                    counselors :: [Person], 
                    aides :: [Person], 
                    aSlots :: Int, 
                    campers :: [Person], 
                    cSlots :: Int} 
                    deriving (Eq)
instance Show Table where
    show (Table n cnslrs aideList _ cmprs _ ) = unlines $ ("Table #" ++ show n): concatMap (\(f,s) -> ("\t"++f): map ("\t\t"++) s) [("Counselors:", map (name.>show) cnslrs), ("Aides:", map (name.>show) aideList), ("Campers:", map (name.>show) cmprs)]
instance Ord Table where
    compare = compare `on` num
    (<=) = (<=) `on` num

data SeedResult g a = Success g a | Failure g a 

failed :: SeedResult g a -> Bool
failed (Success _ _) = False
failed (Failure _ _) = True

getGen :: SeedResult g a -> g
getGen (Success g _) = g
getGen (Failure g _) = g

getResultVal :: SeedResult g a -> a
getResultVal (Success _ a) = a
getResultVal (Failure _ a) = a

nameFromString :: String -> Name
nameFromString s = let s' = words s in Name (head s') (last s')

allPeople :: Table -> [Person]
allPeople tbl = counselors tbl ++ aides tbl ++ campers tbl

getAllCampers :: [Table] -> [Person]
getAllCampers = concatMap campers

getAllCounselors :: [Table] -> [Person]
getAllCounselors = concatMap counselors

getAllAides :: [Table] -> [Person]
getAllAides = concatMap aides

getAllPeople :: [Table] -> [Person]
getAllPeople = concatMap allPeople

generateConflicts :: [Name] -> Person -> Person
generateConflicts ppl p = 
    let newConflicts = filter (lastN .> (==) (p |> name |> lastN ) ) ppl
    in  p {conflicts = newConflicts} 

generateAllConflicts :: [Person] -> Map.Map Name [Name]
generateAllConflicts ppl = map (generateConflicts (map name ppl)) ppl |> foldl insertToMap Map.empty
    where
        insertToMap :: Map.Map Name [Name] -> Person -> Map.Map Name [Name]
        insertToMap mp p = if (name p) `elem` (Map.elems mp |> concat) then mp else Map.insert (name p) (conflicts p) mp

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

trySeed :: RandomGen gen => Person -> SeedResult gen [Table] -> SeedResult gen [Table] 
trySeed a@(Aide _ _ cflcts) sr = 
    let (g1, g2) = split $ getGen sr
        tbl:rest = shuffle'' (getResultVal sr) g1 
    in
        if filter (flip elem cflcts .> not) (allPeople tbl |> map name) |> length |> (==0)
        then Success g2 $ tbl{aides= a{t= num tbl} : aides tbl, aSlots= aSlots tbl - 1}:rest 
        else Failure g2 $ tbl:rest
    
trySeed c@(Camper _ _ _ cflcts) sr = 
    let (g1, g2) = split $ getGen sr
        tbl:rest = shuffle'' (getResultVal sr) g1 
    in
        if filter (flip elem cflcts .> not) (allPeople tbl |> map name) |> length |> (==0)
        then Success g2 $ tbl{campers= c{t= num tbl} : campers tbl, cSlots= cSlots tbl - 1}:rest 
        else Failure g2 $ tbl:rest

seed :: RandomGen gen => SeedResult gen [Table] -> Person -> SeedResult gen [Table]
seed sr p = sr |> iterate (trySeed p) |> tail |> dropWhile failed |> head        

setupTables :: Map.Map Name [Name] -> [Person] -> [Int] -> [Table]
setupTables cflcts cnslrs aSlotLocs = cnslrs |> loadConflicts cflcts |> createCounselorTables |> addAideSlots aSlotLocs |> addCamperSlots

shuffleFoldl :: RandomGen gen => gen -> (b -> a -> b) -> b -> [a] -> b
shuffleFoldl g f b [] = b
shuffleFoldl g f b xs =
    let (g1, g2) = split g
        x:rest = shuffle'' xs g1 
    in shuffleFoldl g2 f (f b x) rest

seedTables :: Int -> [Person] -> [Person] -> [Table] -> [Table]
seedTables seedNumber aideList cmprs tbls = 
    let 
        gs = mkStdGen seedNumber |> iterate (split .> fst) 
        cmprGroups = cmprs |> sortOn age |> fromList |> chunksOf (length tbls) |> toList |> map toList |> flip shuffle'' (head gs)
        sr = shuffleFoldl (gs!!2) seed (Success (gs!!1) tbls) aideList 
    in shuffleFoldl (gs!!4) (shuffleFoldl (gs!!3) seed) sr cmprGroups |> getResultVal

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
        importCounselorLine (t, l) = 
            let 
                fields = l |> pack |> splitOn (pack ",") |> map (strip .> unpack)
                cnslrs = fields |> filter (/= "Aide") |> map (\n -> Counselor (nameFromString n) t [])
                aslots = fields |> filter (== "Aide") |> length
            in 
                (cnslrs, aslots)

importCampers :: FilePath -> IO [Person]
importCampers fp = do
    raw <- readFile fp
    raw |> lines |> zip [1..] |> map importCamperLine |> return
    where
        importCamperLine :: (Int, String) -> Person
        importCamperLine (a, l) = l |> takeWhile (/= ',') |> nameFromString |> flip (flip (`Camper` a) 0) []

importConflicts :: FilePath -> IO (Maybe (Map.Map Name [Name]))
importConflicts fp = do
    raw <- tryIOError $ readFile fp
    case raw of
        Left e -> return Nothing
        Right str -> 
            let
                importConflictLine :: String -> (Name, [Name])
                importConflictLine l = l |> pack |> splitOn (pack ",") |> map (unpack .> nameFromString) |> uncons |> fromJust
            in
                str |> lines |> map importConflictLine |> Map.fromList |> Just |> return 
    

exportConflicts :: FilePath -> Map.Map Name [Name] -> IO ()
exportConflicts fp cflcts = do cflcts |> Map.toList |> map (\(f,s) -> f:s) |> map (concatMap (show .> (++) ",")) |> unlines |> writeFile fp 

createLookup :: [Table] -> [String]
createLookup tbls = pplLookup (getAllCampers tbls) |> (++) ("" : pplLookup (getAllAides tbls))
    where
        pplLookup = sortOn (name .> lastN) .> map (\c -> lastN (name c) ++ "," ++ firstN (name c) ++ "," ++ show (t c)) 

createTables :: [Table] -> [String]
createTables tbls = headers ++ names
    where
        headers = tbls |> map (num .> show) |> map ("Table " ++) |> map (++ ",")
        names = tbls |> map (campers .> map name) |> transpose |> map (show .> (++) ",")

exportTablesAndLookup :: FilePath -> [Table] -> IO ()
exportTablesAndLookup fp tbls = do
    tbls |> createLookup |> unlines |> writeFile ("Lookup-" ++ fp) 
    tbls |> createTables |> unlines |> writeFile ("Tables-" ++ fp)


main :: IO ()
main = do
    weekNumber <- getArgs
    seed <- getLine
    mbCflcts <- importConflicts "Conflicts.csv"
    (simple_counselors, aSlotLocs) <- importCounselorTables "Counselor Tables.csv"
    simple_aides <- importAides "Aides.csv"
    simple_campers <- importCampers "Campers.csv"
    if isNothing mbCflcts
        then (simple_counselors ++ simple_aides ++ simple_campers) |> generateAllConflicts |> exportConflicts "Conflicts.csv" 
        else 
        let cflcts = fromJust mbCflcts
            ads = loadConflicts cflcts simple_aides
            cmprs = loadConflicts cflcts simple_campers
            in
            setupTables cflcts simple_counselors aSlotLocs |> seedTables (read seed) ads cmprs |> exportTablesAndLookup ("Week" ++ show (head weekNumber) ++ ".csv")

cnslrs :: [Person]
cnslrs = zip ['a'..'z'] [1..] |> map (\(n,t) -> Counselor (Name "Counselor" (n:"")) t [])