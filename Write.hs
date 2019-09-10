{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Maybe
import Data.Aeson
import GHC.Generics (Generic)
import Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as B

-- A partir de cada se tendra que escribir en la consola. 
data Persona = Persona
    { cedula :: String
    , nombre :: String
    , edad :: String
    } deriving (Show, Generic)

instance ToJSON Persona where
    toJSON (Persona cedula nombre edad) =
        object [ 
                "cedula"  .= cedula
               , "nombre"   .= nombre
               , "edad"        .= edad
             ]

-- Hasta aca se tendra que escribir en la consola.

-- Este tendrá que estar dentro de la consola.
data Identificador = Identificador
    {   id :: String,
        dataList :: [Persona],
        dataRelationship :: [String]
    } deriving (Show, Generic)
    
 -- Definición   
 
instance BI.Binary Persona
instance BI.Binary Identificador
 

 writeJSON = B.writeFile "Data.json"(Data.Aeson.encode informacion)
 
 --writeJSON = B.writeFile "Data.json"(Data.Aeson.encode informacion)
 writeBSON = B.writeFile "Data.bson" (BI.encode informacion)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
