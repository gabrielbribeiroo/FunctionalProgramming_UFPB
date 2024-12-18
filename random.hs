-- Função para gerar listas com números pseudoaleatórios

seed :: Double
seed = 0.435424592845920

logMap :: (Eq a, Num a) => a -> Double -> Double
logMap 0 seed = seed
logMap t seed = 4.0 * x_1 * (1 - x_1)
                where 
                  x_1 = logMap (t - 1) seed

randomList n i seed = [round ((logMap t seed)*10^i) | t <- [1..n]]