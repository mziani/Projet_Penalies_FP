import Control.Monad (replicateM)

-- Modélisation des données d'un pronostic
data Pronostic = Pronostic
  { cote :: Double  -- Cote du pronostic
  }

-- Fonction pour calculer le gain potentiel d'un pronostic en fonction de la mise
calculateGain :: Double -> Pronostic -> Double
calculateGain mise pronostic = mise * (cote pronostic - 1)

-- Fonction pour calculer le gain potentiel d'un combiné de pronostics
calculateCombinedGain :: Double -> [Pronostic] -> Double
calculateCombinedGain mise pronostics = product $ map (calculateGain mise) pronostics

main :: IO ()
main = do
  putStrLn "Entrez la mise effectuée par le parieur (en euros) : "
  miseParieurStr <- getLine
  let miseParieur = read miseParieurStr :: Double

  putStrLn "Combien de pronostics dans le combiné ?"
  numPronosticsStr <- getLine
  let numPronostics = read numPronosticsStr :: Int

  -- Collecte des cotes pour chaque pronostic
  pronostics <- collecterCotes numPronostics

  let gainPotentiel = calculateCombinedGain miseParieur pronostics

  putStrLn $ "Gain potentiel : " ++ show gainPotentiel ++ " euros"

-- Fonction factice pour collecter les cotes des pronostics
collecterCotes :: Int -> IO [Pronostic]
collecterCotes n = do
  putStrLn "Entrez les cotes pour chaque pronostic (séparées par des espaces) : "
  cotesStr <- getLine
  let cotes = map read (words cotesStr) :: [Double]
  if length cotes == n
    then return $ map Pronostic cotes
    else do
      putStrLn "Le nombre de cotes ne correspond pas au nombre de pronostics. Réessayez."
      collecterCotes n
