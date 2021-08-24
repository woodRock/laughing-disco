type Neuron = [Double]
type Layer = [Neuron]

initNeuron :: Int -> Neuron 
initNeuron n = [ 0.01 | _ <- [0..n] ] 

initLayer :: Int -> Int -> Layer 
initLayer n m = [ initNeuron n | _ <- [0..n]]

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

trainOne :: Double -> [Double] -> Double -> Neuron -> Neuron 
trainOne eta xs t ws = zipWith (weightUpdate eta diff) ws ((-1):xs)
  where diff = recall ws xs - t 

weightUpdate :: Double -> Double -> Double -> Double -> Double 
weightUpdate eta diff w x = w - eta * diff * x

trainOneLayer :: Double -> [Double] -> Double -> Layer -> Layer 
trainOneLayer _ _ _ [] = []
trainOneLayer eta xs ts (l:ls) = trainOne eta xs ts l : trainOneLayer eta xs ts ls 

trainSet :: Double -> [[Double]] -> [Double] -> Layer -> Layer 
trainSet _ [] _ n = n 
trainSet eta (v:vs) (t:ts) l = trainSet eta vs ts $ trainOneLayer eta v t l

train :: Int -> Double -> [[Double]] -> [Double] -> Layer -> Layer
train 0 _ _ _ n = n 
train epoch eta vs ts n = train (epoch - 1) eta vs ts $ trainSet eta vs ts n 

x :: [[Double]]
x = [[x,y,z] | x <- [0..n], y <- [0..n], z <- [0..n]]
  where n = 5

y :: [Double]
y = map objective x 

objective :: [Double] -> Double 
objective [x,y,z] = threshold ((x - 1.0)^4 + (y - 1.0)^5 - z - 1.0)

predict :: Layer -> [[Double]] -> [Double] 
predict model = map (!!0) . map (recallLayer model) 

accuracy :: [Double] -> [Double] -> Double 
accuracy y y' = (n - error) / n
  where 
    error = sum $ zipWith (\y y' -> abs (y - y')) y y'
    n = fromIntegral $ length y 

main :: IO () 
main = putStrLn $ acc 
  where
    acc = "Acc: " ++ show (accuracy y y_pred)
    y_pred = predict model x  
    model = train epochs eta x y (initLayer 3 3) 
    eta = 0.01  
    epochs = 50
