--show :: a -> String
--show x = 


data Pair a = P a a

instance Show a => Show (Pair a) where
	show (P x y) = "<" ++ show x ++ ";" ++ show y ++ ">"

-- data NestedList a = Atom a {fromAtom :: a}
--				| List { fromList :: [NestedList a] }
--			deriving Show

data NestedList a = Atom a 
				| List [NestedList a]
			deriving Show

-- (1 2 (3 4) 5)
-- List [Atom 1,  Atom 2,   List [Atom 3,  Atom 4],    Atom 5]


{--
class <numele-clasei> <variabila-de-tip-folosită-în-definiție> where
		<tipuri de funcții care există pentru clasă>

							| expresie de tip
							v	
instance [<context>] <numele-clasei> <definiția-tipului> where
		<implementarea funcțiilor din clasă>
--}


--EXERCITIU 1 POINT-FREE
class Invertible a where -- "a aderă la clasa Invertible"
	invert :: a -> a
	--point-free
	invert = id --	versiunea normala: invert x = id x


--EXERCITIU 2 POINT-FREE
instance Invertible a => Invertible [a] where
	--invert lista = reverse $ map invert lista
	-- pas I : invert lista = reverse (map invert lista)
	-- pas II: invert lista = (reverse ((map invert) lista))
	--                            f     ------g-----        
	-- pas III : invert = reverse . (map invert)
	invert = reverse . map invert

instance Invertible a => Invertible (Pair a) where
	invert (P x y) = P y x


-- EXERCITIU 3 POINT-FREE	
instance Invertible a => Invertible (NestedList a) where
--	invert (Atom x) = Atom x
--	pas I : invert (Atom x) = Atom $ invert x
-- 	pas II : incert (Atom x) = (Atom . invert) x -> nu pot point-free
--  alternativa: invert a = (Atom . invert. fromAtom) a
--	solutie: invert = Atom . invert . fromAtom -- nu pot sa deosebesc daca lui Atom ii dau atom sau lista
	invert (Atom x) = Atom $ invert x
	invert (List nl) = List $ invert nl


	
instance Invertible Bool
instance Invertible Integer


-- variabila de tip din definiția clasei nu este neapărat un tip
-- ci poate fi un constructor de tip
class Container t where
	-- "dacă construiesc un tip aplicând constructorul de tip t peste un tip a"
	contents :: t a -> [a]

instance Container Pair where
	contents (P x y) = [x, y]

instance Container NestedList where
	contents (Atom x) = [x]
	-- lista este listă Haskell
	contents (List lista) = concat $ map contents lista

--					constructorul de *tip* pentru tipul listă
--					v
instance Container [] where
	contents lista = lista


{-
f4 :: a -> b -> c -> d
din x == y :
	a = b și Eq a
din x > y:
	a = b și Ord a
din valoarea întoarsă:
	a = d = b = c
f4 :: (Eq a, Ord a) => a -> a -> a -> a
f4 :: Ord a => a -> a -> a -> a
-}
f4 x y z	=	if x == y then z else 
					if x > y then x else y
					
{-
f5 :: a -> b -> c
din contents x   (-> [m]):
	Container t și a = t m
din (==):
	Eq m
din invert y, invert x:
	Invertible b, Invertible a
din valoarea întoarsă de funcție:
	a = b = c = t m
f5 :: (Container t, Eq m, Invertible (t m)) => t m -> t m -> t m
-}			
f5 x y = if contents x == [] then invert y else invert x			
					
			
{-
f2 :: a -> b -> c
din invert x, invert y:
	Invertible a. Invertible b, 
din ==:
	a = b, Eq a
din contents x, contents y:
	a = b = t m
	Container t
din valoarea întoarsă:
	c = [m]
f2 :: (Invertible (t m), Container t, Eq (t m)) => t m -> t m -> [m]
-}		
f2 x y		=	if (invert x) == (invert y)
					then contents x
					else contents y


{-
f3 :: a -> b -> c
din head x, head y, >: a = b = [m], Ord m
din invert x:
	Invertible [m]
din (++): a = [m]
din (-): Num m
tipul întors: c = [m]
f3 :: (Invertible [m], Ord m, Num m) => [m] -> [m] -> [m]
f3 :: (Invertible m, Ord m, Num m) => [m] -> [m] -> [m]
-}
f3 x y		=	if head x > head y
					then (invert x) ++ (invert y)
					else [head y - head x]








