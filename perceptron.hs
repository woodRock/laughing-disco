import System.Random (randomR, getStdRandom) 
import Control.Monad (replicateM)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)

type Neuron = [Double]
type Layer = [Neuron]
type Input = [[Double]]
type Output = [Double]

initNeuron :: Int -> IO Neuron 
initNeuron n = do
  let interval = randomR (-0.5, 0.5)
  (replicateM n (getStdRandom interval)) 

initLayer :: Int -> Int -> IO Layer 
initLayer n m = do 
  (replicateM m (initNeuron n))

recall :: Neuron -> [Double] -> Double 
recall n = threshold . neuronSum n

recallLayer :: Layer -> [Double] -> [Double]
recallLayer [] _ = [] 
recallLayer (l:ls) x = recall l x : recallLayer ls x 

threshold :: Double -> Double 
threshold x 
  | x < 0  = 0 
  | x >= 0 = 1 

neuronSum :: Neuron -> [Double] -> Double
neuronSum n is = sum $ zipWith (*) n ((-1):is)

weightUpdate :: Double -> Double -> Double -> Double -> Double 
weightUpdate eta diff w x = w - eta * diff * x

trainOne :: Double -> [Double] -> Double -> Neuron -> Neuron 
trainOne eta xs t ws = zipWith (weightUpdate eta diff) ws ((-1):xs)
  where diff = recall ws xs - t 

trainOneLayer :: Double -> [Double] -> Double -> Layer -> Layer 
trainOneLayer _ _ _ [] = []
trainOneLayer eta xs ts (l:ls) = trainOne eta xs ts l : trainOneLayer eta xs ts ls 

trainSet :: Double -> Input -> Output -> Layer -> Layer 
trainSet _ [] _ n = n 
trainSet eta (v:vs) (t:ts) l = trainSet eta vs ts $ trainOneLayer eta v t l

train :: Int -> Double -> Input -> Output -> Layer -> Layer
train 0 _ _ _ n = n 
train epoch eta vs ts n = train (epoch - 1) eta vs ts $ trainSet eta vs ts n 

x :: Input
x = [[x,y,z] | x <- [0..n], y <- [0..n], z <- [0..n]]
  where n = 5

y :: Output
y = map objective x 

objective :: [Double] -> Double 
objective [x,y,z] = threshold ((x - 1.0)^4 + (y - 1.0)^5 - z - 1.0)

mode :: [Double] -> Double 
mode = head . head . sortBy (flip $ comparing length) . group . sort 

predict :: Layer -> Input -> Output 
predict model = map mode . map (recallLayer model) 

accuracy :: Output -> Output -> Double 
accuracy y y' = (n - error) / n
  where 
    error = sum $ zipWith (\y y' -> abs (y - y')) y y'
    n = fromIntegral $ length y 

main :: IO () 
main = do  
  let eta = 0.01  
  let epochs = 50
  layer <- initLayer 3 3 
  let model = train epochs eta x y layer
  let y_pred = predict model x
  let acc = "Acc: " ++ show (accuracy y y_pred)
  print acc
