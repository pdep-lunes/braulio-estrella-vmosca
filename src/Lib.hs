module Lib () where

import Text.Show.Functions ()


data Personaje = UnPersonaje {
                            nombre :: String,
                            poderBasico :: Poder, 
                            superPoder :: Poder, 
                            poderActivo :: Bool, 
                            cantidadDeVida :: Int
                        } deriving Show

type Poder = Personaje -> Personaje

espina :: Personaje
espina  =  UnPersonaje "Espina" bolaEspinosa (granadaDeEspinas 5) True 4800

pamela :: Personaje
pamela =  UnPersonaje "Pamela" lluviaDeTuercas torretaCurativa False 9600

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

granadaDeEspinas :: Int -> Poder 
granadaDeEspinas radio unPersonaje
    |radio > 3 && cantidadDeVida unPersonaje < 800 = unPersonaje { nombre = nombre unPersonaje ++ " espina estuvo aqui", cantidadDeVida = 0,poderActivo = False} 
    |radio > 3 = unPersonaje  {nombre = nombre unPersonaje ++ " espina estuvo aqui"}
    | otherwise = bolaEspinosa unPersonaje

atacarConElPoderEspecial :: Personaje -> Personaje -> Personaje
atacarConElPoderEspecial unPersonaje contrincante 
    |poderActivo unPersonaje = (poderBasico unPersonaje . superPoder unPersonaje) contrincante 
    |otherwise = unPersonaje

quienesEstanEnLasUltimas :: [Personaje] -> [String]
quienesEstanEnLasUltimas braulios = map nombre ( filter ((< 800) . cantidadDeVida) braulios)
