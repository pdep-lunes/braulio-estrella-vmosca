module Lib () where

import Text.Show.Functions ()


data Personajes = UnEquipo {
                            nombre :: String,
                            poderBasico :: Poder, 
                            superPoder :: Poder, 
                            poderActivo :: Bool, 
                            cantidadDeVida :: Int
                        } deriving Show 

type Poder = Personajes -> Personajes

espina :: Personajes
espina  =  UnEquipo "Espina" bolaEspinosa granadaDeEspinas True 4800

pamela :: Personajes
pamela =  UnEquipo "Pamela" lluviaDeTuercas torretaCurativa False 9600

--bolaEspinosa :: Poder 
--bolaEspinosa unPersonaje = unPersonaje {cantidadDeVida - 1000} 

granadaDeEspinas :: Int -> Poder
granadaDeEspinas radio unPersonaje = unPersonaje 
| unPersonaje radio > 3 = {nombre ++ "espina estuvo aqui"} 
| unPersonaje cantidadDeVida < 800 =  {poderActivo = False, cantidadDeVida = 0}
| otherwise = bolaEspinosa
