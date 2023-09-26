{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Data.List (nub) -- Importez le module Data.List pour nub

-- Définir la structure de données pour représenter un pays
data Country = Country
  { name :: Text,
    continent :: Text,
    confederation :: Text,
    victories :: [Int],
    finals :: [Int]
  }
  deriving (Show, Generic)

instance FromJSON Country
instance ToJSON Country

-- Fonction pour charger les données JSON à partir d'un fichier
loadCountries :: FilePath -> IO [Country]
loadCountries filePath = do
  json <- B.readFile filePath
  case eitherDecode json of
    Right countries -> return countries
    Left err -> do
      putStrLn ("Erreur lors de la lecture du fichier JSON : " ++ err)
      return []

-- Fonction pour filtrer les pays finalistes qui ont remporté au moins une finale
finalistCountriesWithWins :: [Country] -> [Country]
finalistCountriesWithWins = filter (\country -> not (null (victories country)))

-- Fonction pour filtrer les pays qui ont remporté toutes les finales qu'ils ont jouées
countriesWithAllFinalsWon :: [Country] -> [Country]
countriesWithAllFinalsWon countries =
  filter (\country -> all (\year -> year `elem` victories country) (finals country)) countries

-- Fonction pour filtrer les pays qui ont participé à des finales sans en remporter
countriesWithFinalsWithoutWins :: [Country] -> [Country]
countriesWithFinalsWithoutWins countries =
  filter (\country -> not (null (finals country)) && null (victories country)) countries

-- Fonction pour filtrer les pays qui ont à la fois remporté et perdu des finales
countriesWithWinsAndLosses :: [Country] -> [Country]
countriesWithWinsAndLosses countries =
  filter (\country -> not (null (victories country)) && not (null (finals country))) countries


main :: IO ()
main = do
  let filePath = "countries.json"
  countries <- loadCountries filePath
  let finalistCountries = finalistCountriesWithWins countries
      countriesWithAllWins = countriesWithAllFinalsWon countries
      countriesWithoutWins = countriesWithFinalsWithoutWins countries
      countriesWithWinsAndLosses' = countriesWithWinsAndLosses countries
  putStrLn "Pays finalistes ayant remporté au moins une finale :"
  mapM_ (\country -> putStrLn (show (name country))) finalistCountries
  putStrLn "\nPays ayant remporté chacune de leurs finales :"
  mapM_ (\country -> putStrLn (show (name country))) countriesWithAllWins
  putStrLn "\nPays ayant participé à des finales sans en remporter :"
  mapM_ (\country -> putStrLn (show (name country))) countriesWithoutWins
  putStrLn "\nPays ayant à la fois remporté et perdu des finales :"
  mapM_ (\country -> putStrLn (show (name country))) countriesWithWinsAndLosses'