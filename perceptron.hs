-- Represents a single neuron, recording all of its weights.
type Neuron = [Double]

-- The number of weights is n+1
initNeuron :: Int -> Neuron 
initNeuron n = [ initial | _ <- [0..n] ] 
  where initial = 0.01 

-- The function takes the neuron and the input list, returns a scalar output.
recall :: Neuron -> [Double] -> Double 
recall n = threshold . neuronSum n 

-- Defines when a neuron fires. 
threshold :: Double -> Double 
threshold x 
  | x < 0  = 0 
  | x >= 0 = 1 

-- Sums the weights
neuronSum :: Neuron -> [Double] -> Double
neuronSum n is = sum $ zipWith (*) n ((-1):is)

-- Update neuron weights based on a single input/output pair.
trainOne :: Double -> [Double] -> Double -> Neuron -> Neuron 
trainOne eta xs t ws = zipWith (weightUpdate eta diff) ws ((-1):xs)
  where diff = recall ws xs - t 

-- The weight is adjusted proportionally to the difference. 
weightUpdate :: Double -> Double -> Double -> Double -> Double 
weightUpdate eta diff w x = w - eta * diff * x

-- Trains the entire network of neurons. 
trainSet :: Double -> [[Double]] -> [Double] -> Neuron -> Neuron 
trainSet _ [] _ n = n 
trainSet eta (v:vs) (t:ts) n = trainSet eta vs ts $ trainOne eta v t n

-- Train the network for n epochs. 
train:: Int -> Double -> [[Double]] -> [Double] -> Neuron -> Neuron 
train 0 _ _ _ n = n 
train epoch eta vs ts n = train (epoch - 1) eta vs ts $ trainSet eta vs ts n 

-- The input features, a set of 3d vectors.
x :: [[Double]]
x = [[x,y,z] | x <- [0..n], y <- [0..n], z <- [0..n]]
  where n = 5

-- The class labels, where y in [0,1]. 
y :: [Double]
y = map objective x 

-- The objective function the model is trying to learn. 
objective :: [Double] -> Double 
objective [x,y,z] = threshold ((x - 1.0)^4 + (y - 1.0)^5 - z - 1.0)

-- Run the model on a the training set, and report accuracy.
main :: IO () 
main = putStrLn $ show $ acc
  where
    acc = (n - error) / n
    n = fromIntegral $ length x
    error = sum $ zipWith (\y y' -> abs (y - y')) y test
    test = map (recall model) x  
    model = train epochs eta x y (initNeuron 3) 
    eta = 0.01
    epochs = 100
