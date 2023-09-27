{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import qualified Data.Text as T

-- Fonction pour effectuer une requête HTTP et récupérer le contenu de la page
fetchHTML :: String -> IO T.Text
fetchHTML url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  let responseBody = responseBody response
  return $ T.decodeUtf8 $ LBS.toStrict responseBody


-- Fonction pour extraire des données spécifiques de la page web (exemple : équipes)
extractDataFromHTML :: T.Text -> [T.Text]
extractDataFromHTML htmlContent = do
  let cursor = fromDocument $ parseLBS htmlContent
  -- Utilisez des sélecteurs XPath ou CSS pour extraire les données souhaitées
  -- Par exemple, si les noms des équipes sont dans des balises <div class="team-name"> :
  teamNames <- cursor $// element "div" >=> attributeIs "class" "team-name" &// content
  return teamNames

main :: IO ()
main = do
  putStrLn "Entrez l'URL du site web à scraper : "
  url <- getLine
  htmlContent <- fetchHTML url
  let dataItems = extractDataFromHTML htmlContent
  putStrLn "Données extraites :"
  mapM_ putStrLn dataItems
