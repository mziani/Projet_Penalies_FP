{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing)
import Data.Function (on)

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

-- Fonction pour charger les données JSON à partir d'un fichier
loadCountries :: FilePath -> IO [Country]
loadCountries filePath = do
  json <- B.readFile filePath
  case eitherDecode json of
    Right countries -> return countries
    Left err -> do
      putStrLn ("Erreur lors de la lecture du fichier JSON : " ++ err)
      return []

-- Fonction pour répondre à la première question : Quelle équipe a remporté le plus de Coupe du Monde ?
teamWithMostWorldCupWins :: [Country] -> Maybe Country
teamWithMostWorldCupWins [] = Nothing
teamWithMostWorldCupWins countries = Just $ maximumBy (comparing (length . victories)) countries

-- Fonction pour répondre à la deuxième question : Quelle équipe a joué le plus de finale de Coupe du Monde ?
teamWithMostWorldCupFinals :: [Country] -> Maybe Country
teamWithMostWorldCupFinals [] = Nothing
teamWithMostWorldCupFinals countries = Just $ maximumBy (comparing (length . finals)) countries

-- Fonction pour répondre à la troisième question : Quelle équipe a perdu le plus de finales de Coupe du Monde ?
teamWithMostWorldCupFinalLosses :: [Country] -> Maybe Country
teamWithMostWorldCupFinalLosses [] = Nothing
teamWithMostWorldCupFinalLosses countries = Just $ maximumBy (comparing (\country -> length (finals country) - length (victories country))) countries

-- Fonction pour répondre à la quatrième question : Quelle équipe a le meilleur taux de participation couplé au meilleur taux de réussite en finale de Coupe du Monde ?
bestParticipationAndSuccessRate :: [Country] -> Maybe Country
bestParticipationAndSuccessRate [] = Nothing
bestParticipationAndSuccessRate countries =
  Just $
    maximumBy
      (comparing (\country -> (fromIntegral (length (victories country)) / fromIntegral (length (finals country)))))
      countries

-- Fonction pour répondre à la cinquième question : Quel est le classement descendant des finalistes (par ordre de victoires et de participations à une finale de Coupe du Monde) ?
descendingFinalistsRanking :: [Country] -> [Country]
descendingFinalistsRanking countries =
  sortBy
    (comparing (\country -> (length (victories country), length (finals country))))
    countries

-- Réponse à la sixième question : Quel pays a disputé le plus de finales de Coupe du Monde ?
teamWithMostWorldCupFinalAppearances :: [Country] -> Maybe Country
teamWithMostWorldCupFinalAppearances [] = Nothing
teamWithMostWorldCupFinalAppearances countries =
  Just $ maximumBy (comparing (length . finals)) countries

-- Réponse à la septième question : Quel pays a marqué le plus de buts en finale de Coupe du Monde ?
teamWithMostGoalsScoredInFinals :: [Country] -> Maybe Country
teamWithMostGoalsScoredInFinals [] = Nothing
teamWithMostGoalsScoredInFinals countries =
  Just $
    maximumBy (comparing (\country -> sum (victories country))) countries

-- Réponse à la huitième question : Quel pays a encaissé le plus de buts en finale de Coupe du Monde ?
teamWithMostGoalsConcededInFinals :: [Country] -> Maybe Country
teamWithMostGoalsConcededInFinals [] = Nothing
teamWithMostGoalsConcededInFinals countries =
  Just $
    maximumBy (comparing (\country -> sum (finals country) - sum (victories country))) countries

-- Réponse à la neuvième question : Combien de finales se sont terminées après la séance de tirs au but ?
numberOfFinalsWithPenaltyShootout :: [Country] -> Int
numberOfFinalsWithPenaltyShootout countries =
  length $ filter (\country -> not (null (finals country)) && last (finals country) `mod` 10 == 8) countries

-- Réponse à la dixième question : Quel continent a remporté le plus de Coupe du Monde ?
continentWithMostWorldCupWins :: [Country] -> Text
continentWithMostWorldCupWins countries =
  head $
    maximumBy
      (comparing length)
      (groupBy (\x y -> continent x == continent y) countries)

-- Réponse à la onzième question : Quel continent a accueilli le plus d'éditions de la Coupe du Monde ?
continentWithMostWorldCupHosts :: [Country] -> Text
continentWithMostWorldCupHosts countries =
  head $
    maximumBy
      (comparing length)
      (groupBy (\country1 country2 -> continent country1 == continent country2) countries)

-- Réponse à la douzième question : Quel pays a accueilli le plus d'éditions de la Coupe du Monde ?
countryWithMostWorldCupHosts :: [Country] -> Text
countryWithMostWorldCupHosts countries =
  head $
    maximumBy
      (comparing length)
      (groupBy (\x y -> name x == name y) countries)

-- Réponse à la treizième question : Au total, combien de buts ont été inscrits en finale de Coupe du Monde durant le temps réglementaire (hors prolongation et séance de tirs au but), toutes éditions confondues ?
totalGoalsScoredInRegularTime :: [Country] -> Int
totalGoalsScoredInRegularTime countries =
  sum $
    concatMap
      (\country ->
         let victoryGoals = filter (\year -> year `elem` victories country) (finals country)
          in map (`div` 10) victoryGoals)
      countries

-- Réponse à la quatorzième question : Au total, combien de spectateurs ont assisté à une finale de Coupe du Monde, toutes éditions confondues ?
totalSpectatorsInWorldCupFinals :: [Country] -> Int
totalSpectatorsInWorldCupFinals countries =
  sum $
    concatMap
      (\country -> finals country)
      countries

-- Réponse à la quinzième question : Quel stade a accueilli le plus de spectateurs au cours d'une finale de Coupe du Monde ?
stadiumWithMostSpectatorsInWorldCupFinals :: [Country] -> Text
stadiumWithMostSpectatorsInWorldCupFinals countries =
  head $
    maximumBy
      (comparing length)
      (groupBy (\x y -> x == y) (concatMap (\country -> finals country) countries))


main :: IO ()
main = do
  let filePath = "countries.json"
  countries <- loadCountries filePath

  putStrLn "Réponse à la première question :"
  case teamWithMostWorldCupWins countries of
    Just team -> putStrLn $ "L'équipe ayant remporté le plus de Coupes du Monde est : " ++ show (name team)
    Nothing -> putStrLn "Aucune équipe trouvée."

  putStrLn "Réponse à la deuxième question :"
  case teamWithMostWorldCupFinals countries of
    Just team -> putStrLn $ "L'équipe ayant joué le plus de finales de Coupe du Monde est : " ++ show (name team)
    Nothing -> putStrLn "Aucune équipe trouvée."

  putStrLn "Réponse à la troisième question :"
  case teamWithMostWorldCupFinalLosses countries of
    Just team -> putStrLn $ "L'équipe ayant perdu le plus de finales de Coupe du Monde est : " ++ show (name team)
    Nothing -> putStrLn "Aucune équipe trouvée."

  putStrLn "Réponse à la quatrième question :"
  case bestParticipationAndSuccessRate countries of
    Just team ->
      putStrLn $
        "L'équipe avec le meilleur taux de participation et de réussite en finale de Coupe du Monde est : " ++ show (name team)
    Nothing -> putStrLn "Aucune équipe trouvée."

  putStrLn "Réponse à la cinquième question : Classement descendant des finalistes :"
  let ranking = descendingFinalistsRanking countries
  mapM_ (\(rank, country) -> putStrLn $ "Rang " ++ show rank ++ " : " ++ show (name country)) (zip [1 ..] ranking)
    putStrLn "Réponse à la sixième question :"
  case teamWithMostWorldCupFinalAppearances countries of
    Just team -> putStrLn $ "L'équipe ayant disputé le plus de finales de Coupe du Monde est : " ++ show (name team)
    Nothing -> putStrLn "Aucune équipe trouvée."

  putStrLn "Réponse à la septième question :"
  case teamWithMostGoalsScoredInFinals countries of
    Just team -> putStrLn $ "L'équipe ayant marqué le plus de buts en finale de Coupe du Monde est : " ++ show (name team)
    Nothing -> putStrLn "Aucune équipe trouvée."

  putStrLn "Réponse à la huitième question :"
  case teamWithMostGoalsConcededInFinals countries of
    Just team -> putStrLn $ "L'équipe ayant encaissé le plus de buts en finale de Coupe du Monde est : " ++ show (name team)
    Nothing -> putStrLn "Aucune équipe trouvée."

  putStrLn "Réponse à la neuvième question :"
  let numFinalsWithPenaltyShootout = numberOfFinalsWithPenaltyShootout countries
  putStrLn $ "Nombre de finales se terminant après la séance de tirs au but : " ++ show numFinalsWithPenaltyShootout

  putStrLn "Réponse à la dixième question :"
  let continent = continentWithMostWorldCupWins countries
  putStrLn $ "Le continent ayant remporté le plus de Coupe du Monde est : " ++ show continent

  putStrLn "Réponse à la onzième question :"
  let continent = continentWithMostWorldCupHosts countries
  putStrLn $ "Le continent ayant accueilli le plus d'éditions de la Coupe du Monde est : " ++ show continent

  putStrLn "Réponse à la douzième question :"
  let country = countryWithMostWorldCupHosts countries
  putStrLn $ "Le pays ayant accueilli le plus d'éditions de la Coupe du Monde est : " ++ show country

  putStrLn "Réponse à la treizième question :"
  let totalGoals = totalGoalsScoredInRegularTime countries
  putStrLn $ "Au total, " ++ show totalGoals ++ " buts ont été inscrits en finale de Coupe du Monde durant le temps réglementaire (hors prolongation et séance de tirs au but), toutes éditions confondues."

  putStrLn "Réponse à la quatorzième question :"
  let totalSpectators = totalSpectatorsInWorldCupFinals countries
  putStrLn $ "Au total, " ++ show totalSpectators ++ " spectateurs ont assisté à une finale de Coupe du Monde, toutes éditions confondues."

  putStrLn "Réponse à la quinzième question :"
  let stadium = stadiumWithMostSpectatorsInWorldCupFinals countries
  putStrLn $ "Le stade ayant accueilli le plus de spectateurs au cours d'une finale de Coupe du Monde est : " ++ show stadium
