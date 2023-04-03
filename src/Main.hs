module Main (main) where

import Data.List (sortOn)

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import System.Random (newStdGen, randomR, StdGen)

import qualified Data.Vector as V

-------------
-- VECTORS --
-------------

data Vector2 = Vector2 Float Float deriving Show

vzero :: Vector2
vzero = Vector2 0 0

instance Num Vector2 where
    Vector2 x1 y1 + Vector2 x2 y2 = Vector2 (x1 + x2) (y1 + y2)
    Vector2 x1 y1 - Vector2 x2 y2 = Vector2 (x1 - x2) (y1 - y2)
    Vector2 x1 y1 * Vector2 x2 y2 = Vector2 (x1 * x2) (y1 * y2)

    -- Normalize vector
    abs (Vector2 x y) = Vector2 (x / mag) (y / mag)
        where mag = sqrt (x * x + y * y)

    signum (Vector2 x y) = Vector2 (signum x) (signum y)
    fromInteger x = Vector2 (fromInteger x) 0

setMag :: Vector2 -> Float -> Vector2
setMag vec mag = Vector2 (x' * mag) (y' * mag)
    where (Vector2 x' y') = abs vec

vecDist :: Vector2 -> Vector2 -> Float
vecDist (Vector2 x1 y1) (Vector2 x2 y2) = sqrt $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

-----------
-- TYPES --
-----------

data Rocket = Rocket {
    position :: Vector2,
    velocity :: Vector2,
    rocketDNA :: RocketDNA,
    isAlive :: Bool
} deriving Show

type RocketDNA = V.Vector Vector2

data Target = Target Vector2 Picture
data Obstacle = Obstacle Vector2 Float Float

type Population = [Rocket]
type ScoredPopulation = [(Rocket, Float)]

data World = World Target Obstacle Population Int StdGen

---------------------------
-- SIMULATION PARAMETERS --
---------------------------

windowSize :: Int
windowSize = 500

populationSize :: Int
populationSize = 200

lifespan :: Int
lifespan = 500

crossoverRate :: Float
crossoverRate = 0.8

mutationRate :: Float
mutationRate = 0.01

elitism :: Int
elitism = 1

---------
-- DNA --
---------

randomVector :: StdGen -> (Vector2, StdGen)
randomVector gen = (Vector2 x y `setMag` 0.1, gen'')
    where (x, gen') = randomR (-1, 1) gen
          (y, gen'') = randomR (-1, 1) gen'

randomDNA :: StdGen -> (RocketDNA, StdGen)
randomDNA gen = go lifespan gen V.empty
    where go 0 g dna = (dna, g)
          go n g dna = go (n - 1) g' dna'
            where (vec, g') = randomVector g
                  dna' = dna V.++ V.singleton vec
-------------
-- ROCKETS --
-------------

rocketPos :: Vector2
rocketPos = Vector2 0 ((fromIntegral (-windowSize) / 2) + 50)

rocketSize :: Float 
rocketSize = 10

updateRocket :: Rocket -> Int -> Target -> Obstacle -> Rocket 
updateRocket (Rocket pos vel dna False) _ _ _ = Rocket pos vel dna False
updateRocket rocket@(Rocket pos vel dna _) time _ obstacle = if touchingObstacle rocket obstacle 
        then Rocket pos vel dna False else Rocket pos' vel' dna True
    where vel' = vel + (dna V.! time)
          pos' = pos + vel

drawRocket :: Rocket -> Picture 
drawRocket (Rocket (Vector2 x y) (Vector2 vx vy) _ False) = translate x y 
                                                          $ rotate ((180/pi) * atan2 vy vx)
                                                          $ color (makeColor 1 0 0 0.5)
                                                          $ rectangleSolid rocketSize rocketSize
drawRocket (Rocket (Vector2 x y) (Vector2 vx vy) _ _) = translate x y 
                                                      $ rotate ((180/pi) * atan2 vy vx)
                                                      $ color (makeColor 1 1 1 0.5)
                                                      $ rectangleSolid rocketSize rocketSize

newRocket :: StdGen -> (Rocket, StdGen)
newRocket gen = (Rocket rocketPos vzero dna True, gen')
    where (dna, gen') = randomDNA gen

touchingObstacle :: Rocket -> Obstacle -> Bool 
touchingObstacle (Rocket _ _ _ False) _ = True
touchingObstacle (Rocket (Vector2 rx ry) _ _ _) (Obstacle (Vector2 ox oy) ow oh)
    | rx > (ow / 2 + ox) = False 
    | rx < (-ow / 2 + ox) = False 
    | ry > (oh / 2 + oy) = False 
    | ry < (-oh / 2 + oy) = False 
    | otherwise = True

fitness :: Rocket -> Target -> Float
fitness (Rocket _ _ _ False) _ = 0
fitness (Rocket pos _ _ _) (Target targetPos _) = 1 / vecDist pos targetPos

----------------
-- POPULATION --
----------------

initPopulation :: StdGen -> (Population, StdGen)
initPopulation g = go populationSize g []
    where go 0 gen pop = (pop, gen)
          go n gen pop = let (rocket, gen') = newRocket gen
                         in go (n - 1) gen' (rocket : pop)

crossover :: RocketDNA -> RocketDNA -> StdGen -> (RocketDNA, StdGen)
crossover dna1 dna2 gen = do
    let (crossChance, gen') = randomR (0, 1) gen
        (crossPoint, gen'') = randomR (1, lifespan - 1) gen'

    if crossChance > crossoverRate then (dna1, gen'')
        else (V.take crossPoint dna1 V.++ V.drop crossPoint dna2, gen'')

mutate :: RocketDNA -> StdGen -> (RocketDNA, StdGen)
mutate dna gen = let (mutChance, gen') = randomR (0, 1) gen
                     (mutPoint, gen'') = randomR (0, lifespan - 1) gen'
                     (mutAmountX, gen''') = randomR (-0.1, 0.1) gen''
                     (mutAmountY, newGen) = randomR (-0.1, 0.1) gen'''
                 in if mutChance > mutationRate then (dna, gen'') -- gen'' instead of newGen because lazy evaluation
                        else (dna V.// [(mutPoint, (dna V.! mutPoint) + Vector2 mutAmountX mutAmountY)], newGen)

selectParent :: ScoredPopulation -> Float -> StdGen -> (Rocket, StdGen)
selectParent pop totalFitness gen = let (dart, gen') = randomR (0, totalFitness) gen
    in (go pop dart, gen')
        where go [] _ = error "No Population"
              go ((rocket, prob):rs) dart
                | dart <= prob = rocket
                | otherwise = go rs dart

reproduceNonElite :: Population -> Target -> StdGen -> (Population, StdGen)
reproduceNonElite pop target gen = go (drop elitism pop) gen []
    where go [] g acc = (acc, g)
          go p g acc = let (parent1, g') = selectParent scoredPop totalFitness g
                           (parent2, g'') = selectParent scoredPop totalFitness g'
                           (childDNA, g''') = crossover (rocketDNA parent1) (rocketDNA parent2) g''
                           (mutateDNA, g4) = mutate childDNA g'''
                       in go (tail p) g4 (Rocket rocketPos vzero mutateDNA True : acc)
                         where summed = scanl1 (+) $ map (`fitness` target) pop :: [Float]
                               scoredPop = zip pop summed 
                               totalFitness = last summed

reproduce :: Population -> Target -> StdGen -> (Population, StdGen)
reproduce pop target gen = (take elitism sorted ++ nonElite, gen')
    where sorted = sortOn (`fitness` target) $ filter isAlive pop  
          (nonElite, gen') = reproduceNonElite sorted target gen
    
-----------
-- WORLD --
-----------

updateWorld :: ViewPort -> Float -> World -> World
updateWorld _ _ (World target obstacle pop frames gen)
    | frames >= lifespan = let (newP, gen') = reproduce pop target gen in World target obstacle newP 0 gen'
    | otherwise = World target obstacle pop' (succ frames) gen
        where pop' = map (\r -> updateRocket r frames target obstacle) pop

drawTarget :: Target -> Picture 
drawTarget (Target (Vector2 x y) pic) = translate x y pic

drawObstacle :: Obstacle -> Picture 
drawObstacle (Obstacle (Vector2 x y) width height) = translate x y . color white $ rectangleSolid width height

drawWorld :: World -> Picture
drawWorld (World target obstacle pop _ _) = pictures $ targ : ob : map drawRocket pop
    where targ = drawTarget target
          ob = drawObstacle obstacle

window :: Display
window = InWindow "Haskell SmartRockets" (windowSize, windowSize) (10, 10)

main :: IO ()
main = do
    gen <- newStdGen
    let target = Target (Vector2 0 (fromIntegral windowSize / 2 - 100)) (color red $ circleSolid 10)
        obstacle = Obstacle vzero 200 20
        (pop, gen') = initPopulation gen 
        world = World target obstacle pop 0 gen'

    simulate window black 60 world drawWorld updateWorld
