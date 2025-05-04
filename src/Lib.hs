module Lib () where

import Text.Show.Functions ()


data Personaje = UnPersonaje {
                            nombre :: String,
                            poderBasico :: Poder, 
                            superPoder :: Poder, 
                            radioDeSuperPoder :: Int,
                            poderActivo :: Bool, 
                            cantidadDeVida :: Int
                        } deriving Show

type Poder = Personaje -> Personaje

espina :: Personaje
espina  =  UnPersonaje "Espina" bolaEspinosa granadaDeEspinas 5 True 4800

pamela :: Personaje
pamela =  UnPersonaje "Pamela" lluviaDeTuercas torretaCurativa 0 False 9600

bolaEspinosa :: Poder 
bolaEspinosa unPersonaje = unPersonaje {cantidadDeVida = max 0 (cantidadDeVida unPersonaje - 1000)} 

lluviaDeTuercas :: Poder
lluviaDeTuercas unPersonaje 
    |esAliado unPersonaje = unPersonaje {cantidadDeVida = cantidadDeVida unPersonaje + 800}
    |otherwise = unPersonaje {cantidadDeVida = div (cantidadDeVida unPersonaje)  2}

esAliado :: Personaje -> Bool
esAliado unPersonaje =  elem (nombre unPersonaje)  (map nombre equipo)

equipo :: [Personaje]
equipo = [espina, pamela]

torretaCurativa :: Poder
torretaCurativa unPersonaje
    | esAliado unPersonaje = unPersonaje { poderActivo = True , cantidadDeVida = cantidadDeVida unPersonaje *2}
    | otherwise= unPersonaje

granadaDeEspinas :: Poder 
granadaDeEspinas unPersonaje = unPersonaje