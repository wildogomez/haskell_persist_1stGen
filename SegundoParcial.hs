{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Text.Encoding as E
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Bson
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import Data.ByteString as BS
import Data.Typeable
import System.IO
import AbesonV2
import Data.Maybe
import Data.Bson.Generic
import Data.Bson.Binary
import Data.ByteString.Char8 as CH
import Data.ByteString.Lazy.Char8 as CH8
import Data.Binary as BI

data Persona = Persona
    { cedula :: String
    , nombre :: String
    , edad :: String
    } deriving (Show, Generic)


instance FromJSON Persona where
    parseJSON (Object v) =
        Persona <$> v .: "cedula"
            <*> v .: "nombre"
            <*> v .: "edad"
 
instance ToJSON Persona where
    toJSON (Persona cedula nombre edad) =
        object [ 
                "cedula"  .= cedula
               , "nombre"   .= nombre
               , "edad"        .= edad
             ]

data Identificador = Identificador
    {   id :: String,
        dataList :: [Persona],
        dataRelationship :: [String]
    } deriving (Show, Generic)


instance ToJSON Identificador where
 toJSON (Identificador id dataList dataRelationship) =
    object [    
                "id"  .= id,
                "dataList"   .= dataList,
                "dataRelationship" .= dataRelationship
           ]

instance FromJSON Identificador where
 parseJSON (Object v) =
    Identificador     <$> v .: "id"
           <*> v .: "dataList"
           <*> v .: "dataRelationship"
                      
instance BI.Binary Persona
instance BI.Binary Identificador

-- Creamos una funcion que nos permita apuntar a la direccion del archivo.        
jsonFile :: FilePath
jsonFile = "Data.json"    

{--     Demostración de la obtención de los datos convertidos a JSON,
demostración realizada para el Primer Parcial de Proyecto II
--}
obtenerDatosJSON :: IO B.ByteString
obtenerDatosJSON = B.readFile jsonFile

-- Creamos nuestra información, a partir de datos sintéticos.
persona1 = Persona "4738461" "Wildo" "23"
persona2 = Persona "3770212" "Leonardo" "26"
persona3 = Persona "7879456" "Derlis" "20"

-- Los mismos datos serán representados como una lista.
personas :: [Persona]
personas = [persona1, persona2, persona3]

-- Creamos nuestra primera entidad y la guardamos.
informacion = Identificador "Persona" personas []

-- Función que crear nuestro archivo JSON a partir de nuestra información de prueba.
crearArchivoJSON = B.writeFile "Data.json"(Data.Aeson.encode informacion)

-- Obtener informacion/datos en forma de objetos.
obtenerInformacionJSON = do
             input <- B.readFile "Data.json"
             let datos = Data.Aeson.decode input :: Maybe Identificador
             print $ input
             print $ eliminate(datos)

-- Creamos nuestro archivo bson con los datos sintéticos.
crearArchivoBSON = B.writeFile "Data.bson" (BI.encode informacion)

obtenerDatosBSON = do
                datosBSON <- BI.decodeFile "Data.bson" :: IO Identificador   
                print $ datosBSON
                print $ typeOf $ Data.Aeson.encode datosBSON
{-- 
    Función creada para recibir on Objeto, y eliminar del mismo el constructor polimorfico Maybe.
--}
eliminate :: Maybe a -> a
eliminate (Just a) = a

-- Funcion que nos prepara el dato/informacion para ser serializado.
preConvertirABSON = do
    input <- B.readFile "Data.json"
    -- Preparando los datos obtenidos a AESON OBJECT.
    let datos = Data.Aeson.decode input :: Maybe Data.Aeson.Object
    print $ eliminate(datos)
    print ("----------------------------------------------------------------")
    print $ toBson(eliminate(datos))
    print ("----------------------------------------------------------------")
    print $ typeOf $ toBson(eliminate(datos))


-- Función principal del programa.
main = do
    input <- B.readFile "Data.json"

    
    let datos = Data.Aeson.decode input :: Maybe Data.Aeson.Object
    
    --let pruebaBinario =  toBson(eliminate(datos))
    
    --let pruebaBinario = BI.decode input1 :: IO Data.Aeson.Object
      
    --B.writeFile "datas.bson" BI.encode(input)
    
   
      
    print ("----------------------------------------------------------------")
    let id = Data.Aeson.encode $ toAeson $ toBson $ eliminate $ datos
    print ("----------------------------------------------------------------")
    print $ eliminate(datos)
    print ("----------------------------------------------------------------")
    print $ toBson(eliminate(datos))
    print ("----------------------------------------------------------------")
    print $ toBson(eliminate(datos))
    print ("----------------------------------------------------------------")
    print $ typeOf(toBson(eliminate(datos)))

    
 
