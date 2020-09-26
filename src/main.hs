import Data.List (foldl, delete, sort, group, sortOn)
import Data.Function (on)
import Data.Maybe (isNothing, fromJust)

import System.Random.Shuffle (shuffle')
import System.Random (RandomGen, split)

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

data Name = Name {firstN :: String, lastN :: String} deriving (Eq)
instance Show Name where
    show n = firstN n ++ " " ++ lastN n

data Person = 
      Counselor {name :: Name, 
                 t :: Int, 
                 conflicts :: [Person]}
    | Camper {name :: Name, 
              age :: Int, 
              t :: Int, 
              conflicts :: [Person]}
    | Aide {name :: Name, 
            t :: Int, 
            conflicts :: [Person]}
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
    show (Table n cnslrs aideList _ cmprs _ ) = output
        where
            rest =  map (\(f,s) -> ("\t"++f):map ("\t\t"++) s) [("Counselors:", map (name.>show) cnslrs), ("Aides:", map (name.>show) aideList), ("Campers:", map (name.>show) cmprs)]
            output = ("Table #" ++ show n): rest
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

generateConflicts :: [Person] -> Person -> Person
generateConflicts ppl p = 
    let newConflicts = filter (name .> lastN .> (==) (p |> name |> lastN ) ) ppl
    in  p {conflicts = newConflicts} 

generateAllConflicts :: [Person] -> [Person]
generateAllConflicts ppl = map (generateConflicts ppl) ppl

createCounselorTables :: [Person] -> [Table]
createCounselorTables = foldl insertCounselor []
    where
        insertCounselor :: [Table] -> Person -> [Table]
        insertCounselor tbls cnslr = 
            case filter (num .> (==) (t cnslr) ) tbls of
                [] -> Table {num= t cnslr, counselors= [cnslr], aides= [], aSlots=0, campers=[], cSlots=0}:tbls
                [tbl] -> replace tbl (tbl {counselors=cnslr:counselors tbl}) tbls
                _ -> error $ "There are two tables with the same number (" ++ show (t cnslr) ++ ")"

-- addAideSlots :: [Table] -> [(Int, Int)] -> [Table]
-- addAideSlots tbls = foldl (\ tbls' tup -> let tbl = tbls' !! fst tup in replace tbl (tbl {aSlots= snd tup} ) tbls') (sort tbls)

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
        if filter (flip elem cflcts .> not) (allPeople tbl) |> length |> (==0)
        then Success g2 $ tbl{aides= a{t= num tbl} : aides tbl, aSlots= aSlots tbl - 1}:rest 
        else Failure g2 $ tbl:rest
    
trySeed c@(Camper _ _ _ cflcts) sr = 
    let (g1, g2) = split $ getGen sr
        tbl:rest = shuffle'' (getResultVal sr) g1 
    in
        if filter (flip elem cflcts .> not) (allPeople tbl) |> length |> (==0)
        then Success g2 $ tbl{campers= c{t= num tbl} : campers tbl, cSlots= cSlots tbl - 1}:rest 
        else Failure g2 $ tbl:rest

seed :: RandomGen gen => SeedResult gen [Table] -> Person -> SeedResult gen [Table]
seed sr p = sr |> iterate (trySeed p) |> tail |> dropWhile failed |> head        

setupTables :: [Person] -> [Int] -> [Table]
setupTables cnslrs aSlotLocs = cnslrs |> createCounselorTables |> addAideSlots aSlotLocs |> addCamperSlots

shuffleFoldl :: RandomGen gen => gen -> (b -> a -> b) -> b -> [a] -> b
shuffleFoldl g f b [] = b
shuffleFoldl g f b xs = 
    let (g1, g2) = split g 
        x:rest = shuffle'' xs g1 
    in shuffleFoldl g2 f (f b x) rest

seedTables :: RandomGen gen => gen -> [Person] -> [Person] -> [Table] -> [Table]
seedTables g aideList cmprs tbls = 
    let gs = iterate (split .> fst) g
        cmprGroups = cmprs |> sortOn age |> group |> flip shuffle'' (head gs)
        sr = shuffleFoldl (gs!!2) seed (Success (gs!!1) tbls) aideList 
    in shuffleFoldl (gs!!4) (shuffleFoldl (gs!!3) seed) sr cmprGroups |> getResultVal

cnslrs :: [Person]
cnslrs = zip ['a'..'z'] [1..] |> map (\(n,t) -> Counselor (Name "Counselor" (n:"")) t []) 
