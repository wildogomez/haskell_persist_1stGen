{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Persona
(
    Persona(..)
) where


import GHC.Generics (Generic)

data Persona = Persona
    {
        cedula :: String,
        nombre :: String,
        edad :: Int,
        peso :: Float,
        nacionalidad :: String
    } deriving (Show, Generic)
