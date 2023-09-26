import Test.HUnit
import MyModule 

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then putStrLn "Tous les tests ont réussi!"
    else putStrLn "Certains tests ont échoué."

tests :: Test
tests = TestList [testFinalistCountriesWithWins, testCountriesWithAllFinalsWon]

testFinalistCountriesWithWins :: Test
testFinalistCountriesWithWins =
  TestCase $
    assertEqual
      "Pays finalistes ayant remporté au moins une finale"
      expectedFinalistCountries
      (finalistCountriesWithWins sampleCountries)

testCountriesWithAllFinalsWon :: Test
testCountriesWithAllFinalsWon =
  TestCase $
    assertEqual
      "Pays ayant remporté chacune de leurs finales"
      expectedCountriesWithAllFinalsWon
      (countriesWithAllFinalsWon sampleCountries)

-- Définissez les données d'échantillon ici (sampleCountries) et les résultats attendus (expectedFinalistCountries, expectedCountriesWithAllFinalsWon)

sampleCountries :: [Country]
sampleCountries = undefined

expectedFinalistCountries :: [Country]
expectedFinalistCountries = undefined

expectedCountriesWithAllFinalsWon :: [Country]
expectedCountriesWithAllFinalsWon = undefined
