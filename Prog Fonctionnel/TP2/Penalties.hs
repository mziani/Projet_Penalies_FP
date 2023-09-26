import System.Random
import Test.HUnit

data PenaltyResult = PenaltyMarqué | PenaltyManqué deriving (Show, Eq)

tirAuBut :: IO PenaltyResult
tirAuBut = do
  rand <- randomIO :: IO Float
  return $ if rand < 0.6 then PenaltyMarqué else PenaltyManqué

simulerSeance :: IO ()
simulerSeance = do
  let scores = (0, 0)
  resultat <- simulerTirs scores 0
  putStrLn $ "Résultat final : " ++ show resultat

simulerTirs :: (Int, Int) -> Int -> IO (Int, Int)
simulerTirs scores tour
  | tour >= 5 = return scores
  | otherwise = do
    putStrLn $ "Tir " ++ show (tour + 1) ++ " :"
    tir1 <- tirAuBut
    putStrLn $ "Équipe 1 : " ++ show tir1
    tir2 <- tirAuBut
    putStrLn $ "Équipe 2 : " ++ show tir2
    let scores' = case (tir1, tir2) of
                    (PenaltyMarqué, PenaltyManqué) -> (fst scores + 1, snd scores)
                    (PenaltyManqué, PenaltyMarqué) -> (fst scores, snd scores + 1)
                    _ -> scores
    putStrLn $ "Score actuel : " ++ show (fst scores') ++ " - " ++ show (snd scores')
    if (fst scores' > snd scores' + (5 - tour)) || (snd scores' > fst scores' + (5 - tour))
      then return scores'
      else simulerTirs scores' (tour + 1)

-- Fonction pour tester tirAuBut
testTirAuBut :: Test
testTirAuBut = "tirAuBut" ~: test [
    "Test PenaltyMarqué" ~: do
        result <- tirAuBut
        assertBool "Le résultat doit être PenaltyMarqué" (result == PenaltyMarqué),

    "Test PenaltyManqué" ~: do
        result <- tirAuBut
        assertBool "Le résultat doit être PenaltyManqué" (result == PenaltyManqué)
  ]

-- Fonction pour tester simulerTirs
testSimulerTirs :: Test
testSimulerTirs = "simulerTirs" ~: test [
    "Test réussi" ~: do
        let scores = (0, 0)
        result <- simulerTirs scores 0
        assertEqual "Le résultat doit être un tuple (Int, Int)" (0, 0) result
  ]

-- Fonction principale de test
main :: IO ()
main = do
    putStrLn "Début de la séance de tirs au but"
    simulerSeance
    
    -- Exécution des tests
    let tests = test [
            "Test tirAuBut" ~: testTirAuBut,
            "Test simulerTirs" ~: testSimulerTirs
            ]
    counts <- runTestTT tests
    if errors counts == 0 && failures counts == 0
        then putStrLn "Tous les tests ont réussi."
        else putStrLn "Des tests ont échoué."
