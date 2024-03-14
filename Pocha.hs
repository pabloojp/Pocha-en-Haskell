-- Las librerias que hemos utilizado
import Data.List ((\\))
import Control.Concurrent
import System.Random

{- Los data definidos en este proyecto.
En todos nuestros data ni derivamos ni definimos un read ya que resulta engorroso hacerlo.
Ademas, resulta mas comodo no hacer uso de ello.
-}
data Valor = Dos | Cuatro | Cinco | Seis | Siete | Sota | Caballo | Rey | Tres | As
    deriving (Eq,Ord,Enum)
    
instance Show Valor where
    show As = "1"          
    show Dos = "2"
    show Tres = "3"
    show Cuatro = "4"
    show Cinco = "5"
    show Seis = "6"
    show Siete = "7"
    show Sota = "10"
    show Caballo = "11"
    show Rey = "12"

data Palo = Oro | Copa | Espada | Basto
    deriving (Eq,Ord,Enum)

instance Show Palo where
    show Oro = "O"
    show Copa = "C"
    show Espada = "E"
    show Basto = "B"

{- Representamos de este modo las cartas.
Aqui no derivamos Ord, ya que dependiendo de la situacion querremos ordenar las listas de cartas de forma distinta.
-}
data Carta = Carta {palo :: Palo,valor :: Valor} 
    deriving (Eq)

instance Show Carta where
    show (Carta palo valor) = (show valor) ++ " " ++ (show palo)   
    
instance Enum Carta where
    fromEnum (Carta palo valor) = 10*(fromEnum palo) + fromEnum valor
    toEnum n = Carta (toEnum (div n 10) :: Palo) (toEnum (mod n 10) :: Valor)

-- Los types definidos en este proyecto

-- Nos referiremos a la carta que pinta con este tipo
type Pinta = Carta

-- La lista de cartas que tiene el jugador en su mano
type Mano = [Carta]

-- La lista de cartas sobre la mesa en cada baza
type Baza = [Carta]  

-- Asi nos referiremos al resto de listas de cartas
type Mazo = [Carta]

-- La lista del nº de bazas pedidas por jugador
type Pedidas = [Int]

-- La mano normal pero con las cartas marcadas (se explica mas adelante)
type Marcadas = [(Carta,Bool)]
   
-- Almacena los datos: Nombre,Mano,Puntuacion,Pedidas,Hechas y Orden de un jugador
type Jugador = (String,Mano,Int,Int,Int,Int)  
type JugadorM = (String,Marcadas,Int,Int,Int,Int,Bool)  --Lo mismo pero con la mano marcada

--Almacena los datos: Lista de jugadores,Pinta,Baza,Descartes,Ronda,Orden de la mano y Dificultad como datos globales de la partida
type EstadoPartida = ([Jugador],Pinta,Baza,Mazo,Int,Int,Int)   
type EstadoPartidaM = ([JugadorM],Pinta,Baza,Mazo,Int,Int,Int) --Lo mismo pero con los jugadores marcados

--El nombre y la puntuacion de un jugador 
type Puntuacion = (String,Int)

-- Funciones para acceder a/modificar los atributos de los data
paloC :: Carta -> Palo
paloC (Carta palo valor) = palo

valorC :: Carta -> Valor
valorC (Carta palo valor) = valor

nombreJ :: Jugador -> String
nombreJ (nombre,_,_,_,_,_) = nombre

manoJ :: Jugador -> Mano
manoJ (_,mano,_,_,_,_) = mano

puntJ :: Jugador -> Int
puntJ (_,_,punt,_,_,_) = punt

pedidasJ :: Jugador -> Int
pedidasJ (_,_,_,pedidas,_,_) = pedidas

hechasJ :: Jugador -> Int
hechasJ (_,_,_,_,hechas,_) = hechas

ordenJ :: Jugador -> Int
ordenJ (_,_,_,_,_,orden) = orden

nombreJM :: JugadorM -> String
nombreJM (nombre,_,_,_,_,_,_) = nombre

marcadasJM :: JugadorM -> Marcadas
marcadasJM (_,marcadas,_,_,_,_,_) = marcadas

puntJM :: JugadorM -> Int
puntJM (_,_,punt,_,_,_,_) = punt

pedidasJM :: JugadorM -> Int
pedidasJM (_,_,_,pedidas,_,_,_) = pedidas

hechasJM :: JugadorM -> Int
hechasJM (_,_,_,_,hechas,_,_) = hechas

ordenJM :: JugadorM -> Int
ordenJM (_,_,_,_,_,orden,_) = orden

ultMarJM :: JugadorM -> Bool
ultMarJM (_,_,_,_,_,_,ultMar) = ultMar

modifManoJ :: Jugador -> Mano -> Jugador
modifManoJ (nombre,mano,punt,pedidas,hechas,orden) nMano = (nombre,nMano,punt,pedidas,hechas,orden)

modifPuntJ :: Jugador -> Int -> Jugador
modifPuntJ (nombre,mano,punt,pedidas,hechas,orden) nPunt = (nombre,mano,nPunt,pedidas,hechas,orden)

modifPedidasJ :: Jugador -> Int -> Jugador
modifPedidasJ (nombre,mano,punt,pedidas,hechas,orden) nPedidas = (nombre,mano,punt,nPedidas,hechas,orden)

modifHechasJ :: Jugador -> Int -> Jugador
modifHechasJ (nombre,mano,punt,pedidas,hechas,orden) nHechas = (nombre,mano,punt,pedidas,nHechas,orden)

modifOrdenJ :: Jugador -> Int -> Jugador
modifOrdenJ (nombre,mano,punt,pedidas,hechas,orden) nOrden = (nombre,mano,punt,pedidas,hechas,nOrden)

modifMarcadasJM :: JugadorM -> Marcadas -> JugadorM
modifMarcadasJM (nombre,marcadas,punt,pedidas,hechas,orden,ultMar) nMarcadas = (nombre,nMarcadas,punt,pedidas,hechas,orden,ultMar)

modifPuntJM :: JugadorM -> Int -> JugadorM
modifPuntJM (nombre,marcadas,punt,pedidas,hechas,orden,ultMar) nPunt = (nombre,marcadas,nPunt,pedidas,hechas,orden,ultMar)

modifPedidasJM :: JugadorM -> Int -> JugadorM
modifPedidasJM (nombre,marcadas,punt,pedidas,hechas,orden,ultMar) nPedidas = (nombre,marcadas,punt,nPedidas,hechas,orden,ultMar)

modifHechasJM :: JugadorM -> Int -> JugadorM
modifHechasJM (nombre,marcadas,punt,pedidas,hechas,orden,ultMar) nHechas = (nombre,marcadas,punt,pedidas,nHechas,orden,ultMar)

modifOrdenJM :: JugadorM -> Int -> JugadorM
modifOrdenJM (nombre,marcadas,punt,pedidas,hechas,orden,ultMar) nOrden = (nombre,marcadas,punt,pedidas,hechas,nOrden,ultMar)

modifUltMarJM :: JugadorM -> Bool -> JugadorM
modifUltMarJM (nombre,marcadas,punt,pedidas,hechas,orden,ultMar) nUltMar = (nombre,marcadas,punt,pedidas,hechas,orden,nUltMar)

-- Menús y funciones que hacen avanzar la partida

-- Es el equivalente al "main" del proyecto
jugar :: IO()
jugar = do intro
           menuPrincipal

-- Muestra por pantalla el cartel de bienenida
intro :: IO()
intro = do contenido <- readFile "bienvenidaPocha.txt"
           putStrLn " "
           putStrLn " "
           putStrLn " "
           putStr contenido
           putStrLn " "
           putStrLn " "
           putStrLn " "
           threadDelay 2500000

-- Es el menú principal que nos aparece al iniciar una partida
menuPrincipal :: IO()
menuPrincipal = do putStrLn "MENU PRINCIPAL"
                   putStrLn "1. Instrucciones de la pocha."
                   putStrLn "2. Reanudar la partida anterior."
                   putStrLn "3. Empezar una nueva partida."
                   putStrLn "4. Salir del juego."
                   putStr "Seleccione una opcion: "
                   option <- leeIntEnRango 1 4
                   case option of
                        1 -> do 
                             putStrLn " "
                             menuInstrucciones
                        -- Aqui cargamos la partida guardada
                        2 -> do 
                             putStrLn " "
                             contenido <- readFile "guardado.txt"
                             let lineas = lines contenido
                                 estado = read (lineas !! 0) :: Int
                             if estado == 0 then do putStrLn "No hay ninguna partida guardada. "
                                                    putStrLn " "
                                                    threadDelay 1000000
                                                    menuPrincipal
                             else do nombreIn <- menuNombre False
                                     estPar <- reanudarPartida nombreIn
                                     estParF <- ejecutarRonda estPar
                                     finPartida estParF
                        -- Aqui realizamos la inicializacion del estadoPartida   
                        3 -> do 
                             nombreJug <- menuNombre True
                             dif <- menuDificultad
                             orden <- inicOrden
                             let jug1 = (nombreJug,[],0,0,0,0)
                                 jug2 = ("Alejandro",[],0,0,0,1)
                                 jug3 = ("Claudia",[],0,0,0,2)
                                 jug4 = ("Pablo",[],0,0,0,3)
                                 listaJug = [jug1,jug2,jug3,jug4]
                                 estPar = (listaJug,Carta Basto Cuatro,[],[],0,orden,dif)
                             inicializarFichero
                             estParF <- ejecutarRonda estPar
                             finPartida estParF

                        4 -> do 
                             salirPartida

-- Coloca un 0 en el fichero de guardado (para que detecte que se trata de un archivo nuevo)
inicializarFichero :: IO()
inicializarFichero = do writeFile "guardado.txt" "0"

-- Menu que muestra las instrucciones que desee el usuario (todas o solo una seccion)                               
menuInstrucciones :: IO ()
menuInstrucciones = do putStrLn "MENU DE INSTRUCCIONES"
                       putStrLn "1. Instrucciones completas de la pocha."
                       putStrLn "2. Objetivo del juego."
                       putStrLn "3. Numero de jugadores."
                       putStrLn "4. Baraja de cartas."
                       putStrLn "5. Orden y valor de las cartas."
                       putStrLn "6. Distribucion de las cartas."
                       putStrLn "7. Rondas."
                       putStrLn "8. Previsiones y peticiones."
                       putStrLn "9. El carteo." 
                       putStrLn "10. La puntuación."
                       putStr "Seleccione una opcion: "
                       option <- leeIntEnRango 1 10
                       let xs = ["","Objetivo del juego","Numero de jugadores","Baraja de cartas","Orden y valor de las cartas","Distribucion de las cartas","Rondas","Previsiones y peticiones","El carteo","La puntuacion"]
                       leerInstruccionesParte (xs !! (option - 1)) option "instrucciones.txt"
                       putStrLn " "
                       threadDelay 2500000
                       menuPrincipal

-- Funciones auxiliares de menuInstrucciones
mostrarTodo :: [String] -> IO ()
mostrarTodo [] = return () 
mostrarTodo (x:xs) = do putStrLn x
                        mostrarTodo xs 

subSeccion :: String -> Int -> [String] -> IO ()
subSeccion parte n xs = do let numero = 2 + 3 * (n-2) + 1
                           putStr (parte ++ ": ")
                           putStrLn (xs!!numero)

leerInstruccionesParte :: String -> Int -> String -> IO ()
leerInstruccionesParte _ 1 nombre = do contenido <- readFile nombre
                                       putStrLn " "
                                       let lineas = lines contenido
                                       mostrarTodo lineas
leerInstruccionesParte parte n nombre = do contenido <- readFile nombre
                                           putStrLn " "
                                           let lineas = lines contenido
                                           subSeccion parte n lineas

leeInt :: IO Int 
leeInt = do c <- getLine
            return (read c)

leeIntEnRango :: Int -> Int -> IO Int
leeIntEnRango men may = do n <- leeInt       
                           if (n<men) || (n>may)
                              then do putStr "Numero incorrecto, introduce un valor válido: "
                                      leeIntEnRango men may
                              else return n

-- Menu para introducir el nombre de usuario. Debe de ser distinto de "Alejandro", "Claudia" y "Pablo".
menuNombre :: Bool -> IO String
menuNombre esNueva = do
    putStrLn " "
    if esNueva then do putStr "Nombre de usuario: "
    else do putStr "Recuerdame tu nombre de usuario: "
    nombreIn <- getLine
    let booleano = nombreRepetido nombreIn ["Alejandro", "Claudia", "Pablo"]
    putStrLn " "
    if booleano
        then do
            putStrLn "Ya existe este nombre de usuario. Por favor, introduce otro diferente."
            menuNombre esNueva
        else
            return nombreIn

nombreRepetido :: String -> [String] -> Bool
nombreRepetido _ [] = False
nombreRepetido nombre (x:xs)
    | nombre == x = True
    | otherwise = nombreRepetido nombre xs

-- Menu para introducir la dificultad de la IA
menuDificultad :: IO Int
menuDificultad = do putStrLn "DIFICULTAD DE LA IA"
                    putStrLn "1. Nivel facil."
                    putStrLn "2. Nivel dificil."
                    putStr "Seleccione una opcion: "
                    option <- leeIntEnRango 1 2
                    putStrLn " "
                    return option

-- Se elige de manera aleatoria quien es mano en la primera ronda
inicOrden :: IO Int
inicOrden = do o <- randomRIO (0,3) :: IO Int
               return o

{- Funcion que ejecuta cada ronda.
Se encarga de mostrar por pantalla la informacion correspondiente y de actualizar los data Jugador y EstadoPartida.
También se encarga de barajar las cartas y repartir las manos y la pinta.
-}
ejecutarRonda :: EstadoPartida -> IO EstadoPartida --V
ejecutarRonda (listaJug,pinta,baza,descartes,ronda,ordenMano,dif)
    |ronda == 22 = do return (listaJug,pinta,baza,descartes,ronda,ordenMano,dif)
    |otherwise = do putStrLn ("RONDA " ++ show rondaA)
                    threadDelay 1000000
                    putStrLn " "
                    baraja <- barajarMazo   
                    let listaJugA = actualizarOrdenR listaJug ordenMano
                        (listaManos, pintaR) = repartirCartasPinta baraja rondaA
                        listaJugAc = actualizarMano listaJugA listaManos
                        listaJugAct = inicHechas listaJugAc
                    putStrLn ("La pinta es: " ++ show pintaR)
                    putStrLn " "
                    threadDelay 1000000
                    (listaJugD,pintaD,bazaD,descartesD,rondaD,ordenManoD,difD) <- ejecutarPedir (listaJugAct,pintaR,[],[],ronda,ordenMano,dif) 0 []
                    putStrLn "_____________________________"
                    threadDelay 700000
                    putStrLn " "
                    if (dif == 1) then do estParF <- ejecutarBazaF (listaJugD,pintaD,bazaD,descartesD,rondaD,ordenManoD,difD) rondaA
                                          ejecutarRonda estParF
                    else do let listaJugM = inicializarMarcadas listaJugD pinta rondaA
                                estParM = (listaJugM,pintaD,bazaD,descartesD,rondaD,ordenManoD,difD)
                            estParD <- ejecutarBazaD estParM rondaA
                            ejecutarRonda estParD
    where listaRonda = [1..9] ++ replicate 4 10 ++ reverse [1..9]
          rondaA = listaRonda !! ronda

-- Funciones para barajar y repartir cartas

-- Aqui se utiliza la funcion randomRIO para asegurar que en cada ronda las cartas que se reparten son al azar
barajarMazo :: IO [Carta]
barajarMazo = do listaIntRand <- listaRandomIntMinMax 0 39
                 let baraja = map toEnum listaIntRand :: [Carta]
                 return baraja

listaRandomIntMinMax :: Int -> Int -> IO [Int]
listaRandomIntMinMax min max = listaRandomIntMinMax' [min..max] []

listaRandomIntMinMax' :: [Int] -> [Int] -> IO [Int]
listaRandomIntMinMax' [] listaIntF = do return listaIntF
listaRandomIntMinMax' listaIntI listaIntF = do let lenI = length listaIntI
                                               i <- randomRIO (0,(lenI-1)) :: IO Int
                                               let r = listaIntI !! i
                                               listaRandomIntMinMax' (listaIntI \\ [r]) (listaIntF ++ [r])

repartirCartasPinta :: Mazo -> Int -> ([Mano],Pinta)
repartirCartasPinta mazo r
    |r == 10 = (manosSep,mazo !! 39)
    |otherwise = (manosSep,mazo !! (4*r))
    where manosSep = splitn r 4 mazo

splitn :: Eq a => Int -> Int -> [a] -> [[a]]
splitn 0 n lista = []
splitn r 0 lista = []
splitn r n [] = []
splitn r n lista = part:splitn r (n-1) (lista \\ part)
    where part = take r lista

-- Más funciones auxiliares de ejecutarRonda
actualizarMano :: [Jugador] -> [Mano] -> [Jugador]
actualizarMano (j:listaJug) (m:listaMano) = (modifManoJ j m):actualizarMano listaJug listaMano
actualizarMano _ _ = []

inicHechas :: [Jugador] -> [Jugador]
inicHechas [] = []
inicHechas (j:listaJug) = (modifHechasJ j 0):inicHechas listaJug 

inicializarMarcadas :: [Jugador] -> Pinta -> Int -> [JugadorM]
inicializarMarcadas [] _ _ = []
inicializarMarcadas (j:listaJug) pinta ronda = j':inicializarMarcadas listaJug pinta ronda
    where (nombre,mano,punt,pedidas,hechas,orden) = j
          mJugM = marcarManoInic mano pinta pedidas ronda
          j' = (nombre,mJugM,punt,pedidas,hechas,orden,False)

{- Funcion que ejecuta la parte en la que cada jugador pide las bazas que desea hacerse.
Se trata por separado el caso ordJug == 3, ya que es el que desajusta.
-}
ejecutarPedir :: EstadoPartida -> Int -> [Int] -> IO EstadoPartida
ejecutarPedir (listaJug,pinta,baza,descartes,ronda,ordenMano,dif) 3 lPed
    |esIA (nombreJ jugador) = do let manoJug = manoJ jugador
                                     nombreJug = nombreJ jugador
                                     ronAct = length manoJug
                                     pedJugF = pedirNumBazasF manoJug pinta lPed ronAct
                                     pedJugD = pedirNumBazasD manoJug pinta lPed ronAct
                                     jugadorAF = modifPedidasJ jugador pedJugF
                                     jugadorAD = modifPedidasJ jugador pedJugD
                                     listaJugAF = actualizarListaJug listaJug jugadorAF 3
                                     listaJugAD = actualizarListaJug listaJug jugadorAD 3
                                 if (dif == 1) then do putStrLn (nombreJug ++ " ha pedido " ++ show pedJugF ++ " bazas.")
                                                       putStrLn " "
                                                       return (listaJugAF,pinta,baza,descartes,ronda,ordenMano,dif)
                                 else do putStrLn (nombreJug ++ " ha pedido " ++ show pedJugD ++ " bazas.")
                                         putStrLn " "
                                         return (listaJugAD,pinta,baza,descartes,ronda,ordenMano,dif)
    |otherwise = do let mJug = manoJ jugador
                        lenM = length mJug
                    putStrLn ("Tu mano: " ++ mostrarMano (ordenarManoVis mJug pinta))
                    threadDelay 800000
                    pedidas <- preguntarNumBazasD lenM lPed
                    putStrLn " "
                    let jugadorAct = modifPedidasJ jugador pedidas
                        listaJugAct = actualizarListaJug listaJug jugadorAct 3
                    return (listaJugAct,pinta,baza,descartes,ronda,ordenMano,dif)
    where jugador = verJug listaJug 3
ejecutarPedir (listaJug,pinta,baza,descartes,ronda,ordenMano,dif) ordJug lPed
    |esIA (nombreJ jugador) = do let manoJug = manoJ jugador
                                     nombreJug = nombreJ jugador
                                     ronAct = length manoJug
                                     pedJugF = pedirNumBazasF manoJug pinta lPed ronAct
                                     pedJugD = pedirNumBazasD manoJug pinta lPed ronAct
                                     jugadorAF = modifPedidasJ jugador pedJugF
                                     jugadorAD = modifPedidasJ jugador pedJugD
                                     listaJugAF = actualizarListaJug listaJug jugadorAF ordJug
                                     listaJugAD = actualizarListaJug listaJug jugadorAD ordJug
                                 if (dif == 1) then do putStrLn (nombreJug ++ " ha pedido " ++ show pedJugF ++ " bazas.")
                                                       putStrLn " "
                                                       threadDelay 800000
                                                       ejecutarPedir (listaJugAF,pinta,baza,descartes,ronda,ordenMano,dif) (ordJug + 1) (lPed ++ [pedJugF])
                                 else do putStrLn (nombreJug ++ " ha pedido " ++ show pedJugD ++ " bazas.")
                                         putStrLn " "
                                         threadDelay 800000
                                         ejecutarPedir (listaJugAD,pinta,baza,descartes,ronda,ordenMano,dif) (ordJug + 1) (lPed ++ [pedJugD])
    |otherwise = do let mJug = manoJ jugador
                        lenM = length mJug
                    putStrLn ("Tu mano: " ++ mostrarMano (ordenarManoVis mJug pinta))
                    threadDelay 800000
                    ped <- preguntarNumBazas lenM
                    putStrLn " "
                    let jugadorA = modifPedidasJ jugador ped
                        listaJugA = actualizarListaJug listaJug jugadorA ordJug
                    ejecutarPedir (listaJugA,pinta,baza,descartes,ronda,ordenMano,dif) (ordJug + 1) (lPed ++ [ped])
    where jugador = verJug listaJug ordJug

-- Se hacen dos funciones por separado para tratar el caso en el que el jugador tiene que desajustar y en el que no
preguntarNumBazas :: Int -> IO Int
preguntarNumBazas lenM = do putStr "Elige cuantas bazas te pides: "
                            m <- getLine
                            let n = (read m) :: Int
                            if perteneceALista n [0..lenM] then return n
                            else do putStrLn ("No puedes pedir " ++ m)
                                    threadDelay 600000
                                    preguntarNumBazas lenM

preguntarNumBazasD :: Int -> [Int] -> IO Int
preguntarNumBazasD lenM lPed = do let d = sum lPed
                                  putStr ("Elige cuantas bazas te pides. ")
                                  if (lenM - d) >= 0 then putStr ("No puedes pedir "++ (show (lenM - d)) ++ ": ")
                                  else putStr ("Puedes pedir las que quieras: ")
                                  m <- getLine
                                  let n = (read m) :: Int
                                  if perteneceALista n ([0..lenM]\\[lenM - d]) then return n
                                  else do putStrLn ("No puedes pedir " ++ m)
                                          threadDelay 600000
                                          preguntarNumBazasD lenM lPed

{- Funcion para ejecutar cada baza con la IA facil.
Tambien se encarga de actualizar las puntuaciones, la ronda y el orden de la mano al acabar la ronda.
-}
ejecutarBazaF :: EstadoPartida -> Int -> IO EstadoPartida
ejecutarBazaF (listaJug,pinta,baza,descartes,ronda,ordenMano,dif) 0 = do
                let listaResul = obtenerResRonda listaJug
                    lCambioPunt = map calcularPunt listaResul
                    listaJugA = actualizarPunt listaJug lCambioPunt
                putStrLn ("RONDA TERMINADA")
                putStrLn " "
                threadDelay 2000000
                let estPar = (listaJugA,pinta,baza,[],(ronda + 1),(mod (ordenMano + 1) 4),dif)
                menuPartida estPar
                return estPar    
ejecutarBazaF estPar nBaza = do estParA <- ejecutarCartaF estPar 0
                                ejecutarBazaF estParA (nBaza - 1)

--Lo mismo pero con la IA dificil. Notese que se hacen dos funciones por los diferentes tipos de EstadoPartida
ejecutarBazaD :: EstadoPartidaM -> Int -> IO EstadoPartida
ejecutarBazaD (listaJugM,pinta,baza,descartes,ronda,ordenMano,dif) 0 = do
                let listaJug = mapSinMarcas listaJugM
                    listaResul = obtenerResRonda listaJug
                    lCambioPunt = map calcularPunt listaResul
                    listaJugA = actualizarPunt listaJug lCambioPunt
                putStrLn ("RONDA TERMINADA")
                putStrLn " "
                threadDelay 2000000
                let estPar = (listaJugA,pinta,baza,[],(ronda + 1),(mod (ordenMano + 1) 4),dif)
                menuPartida estPar
                return estPar
ejecutarBazaD estPar nBaza = do estParA <- ejecutarCartaD estPar 0
                                ejecutarBazaD estParA (nBaza - 1)

-- Funciones auxiliares de ejecutarBaza que calculan y actualizan las puntuaciones
obtenerResRonda :: [Jugador] -> [(Int,Int)]
obtenerResRonda [] = []
obtenerResRonda (j:listaJug) = ((h - p),p):obtenerResRonda listaJug
    where p = pedidasJ j
          h = hechasJ j

calcularPunt :: (Int,Int) -> Int
calcularPunt (n,p)
    |n == 0 = 10 + 5*p
    |otherwise = (-5) * (abs n)

actualizarPunt :: [Jugador] -> [Int] -> [Jugador]
actualizarPunt (j:listaJug) (p:lPunt) = (modifPuntJ j puntA):actualizarPunt listaJug lPunt
    where punt = puntJ j
          puntA = punt + p
actualizarPunt _ _ = []    

mapSinMarcas :: [JugadorM] -> [Jugador]
mapSinMarcas [] = []
mapSinMarcas ((nombre,manoM,puntuacion,pedidas,hechas,orden,ultMar):listaJugMar) = (nombre,desmarcarMano manoM,puntuacion,pedidas,hechas,orden):mapSinMarcas listaJugMar   

{- Menú que aparece tras acabar cada ronda.
Se da la opcion al usuario de continuar, guardar partida, consultar puntuaciones o salir de la partida
-}
menuPartida :: EstadoPartida -> IO ()
menuPartida (xs,pinta,baza,descartes,ronda,ordMano,dif) = do 
                 putStrLn "1. Continuar partida."
                 putStrLn "2. Guardar partida."
                 putStrLn "3. Consultar puntuaciones."
                 putStrLn "4. Salir de la partida."
                 putStr "Seleccione una opcion: "
                 option <- leeIntEnRango 1 4
                 case option of
                      1 -> do putStrLn " "
                           
                      2 -> do putStrLn " "
                              guardarPartida xs ronda ordMano dif 
                              menuPartida (xs,pinta,baza,descartes,ronda,ordMano,dif)
                           
                      3 -> do putStrLn " "
                              clasificacion (jugadorApuntuacion xs) True
                              menuPartida (xs,pinta,baza,descartes,ronda,ordMano,dif)

                      4 -> do putStrLn " "
                              salirPartida

{- Esta funcion se encarga de mostrar la clasificacion actual de la partida.
Tambien se encarga de mostrar la clasificacion final al acabar la partida y dar la enhorabuena a los ganadores.
-}                            
clasificacion :: [Puntuacion] -> Bool -> IO () 
clasificacion jugadores noFinal = do
    let ordenados = ordenarJugadores jugadores
        clasific = zip [1..] ordenados
        grupos = ajustarOrden clasific 0 1
    if noFinal
        then do putStrLn "La clasificación es: "
        else do putStrLn "La clasificación final es: "
    threadDelay 800000
    putStrLn " "
    escribirClasif grupos
    threadDelay 2000000
    if not noFinal 
        then do putStrLn " "
                enhorabuena grupos
        else do putStrLn " "

-- Llamamos a esta funcion cuando terminamos la ultima ronda.
finPartida :: EstadoPartida -> IO()
finPartida (listaJug,_,_,_,ronda,_,_) = do let puntos = jugadorApuntuacion listaJug
                                           clasificacion puntos False

--Funciones auxiliares de clasificacion
jugadorApuntuacion :: [Jugador] -> [Puntuacion]
jugadorApuntuacion [] = []
jugadorApuntuacion (x:xs) = (nombreJ x, puntJ x) : jugadorApuntuacion xs

ordenarJugadores :: [Puntuacion] -> [Puntuacion]
ordenarJugadores [] = []
ordenarJugadores (x:xs) = ordenarJugadores [y| y <- xs, snd(y) > snd(x) ] ++ [x] ++ ordenarJugadores [y| y <- xs, snd(y) <= snd(x) ]

puntosTupla :: (Int,Puntuacion) -> Int
puntosTupla x = snd $ snd x 

nombreTupla :: (Int,Puntuacion) -> String
nombreTupla x = fst $ snd x

posicionTupla :: (Int,Puntuacion) -> Int
posicionTupla x = fst x 

ajustarOrden :: [(Int,Puntuacion)] -> Int -> Int -> [(Int,Puntuacion)]
ajustarOrden [] _ _= [] 
ajustarOrden (x:xs) ptsAnt posAnt 
        | puntosTupla x == ptsAnt = (posAnt, (nombreTupla x,ptsAnt)) : ajustarOrden xs (puntosTupla x) posAnt
        | otherwise = x : ajustarOrden xs (puntosTupla x) (posicionTupla x)

textoPuntuaciones :: (Int, Puntuacion) -> String
textoPuntuaciones x = show (posicionTupla x) ++ ". " ++ show (nombreTupla x) ++ " con " ++ show (puntosTupla x) ++ " puntos."

escribirClasif :: [(Int, Puntuacion)] -> IO()
escribirClasif [] = do putStrLn "____________________________________"
escribirClasif (x:xs) = do putStrLn $ textoPuntuaciones x 
                           escribirClasif xs 

enhorabuena :: [(Int,Puntuacion)] -> IO ()
enhorabuena grupos 
            | longitud == 1 = do putStrLn $ "ENHORABUENA " ++ unwords winners ++ ", HAS GANADO LA PARTIDA!!!!"
            | otherwise = do putStrLn $ "ENHORABUENA " ++ textoW ++ ", HABEIS GANADO LA PARTIDA!!!!"
    where posiciones = map posicionTupla grupos
          cantidad1 = filter (\x -> x == 1) posiciones
          longitud = length cantidad1
          winners = ganadores grupos 
          signPuntu = replicate ((length winners) - 2) "," ++ ["y"]
          textoW = unwords $ intercalarListas winners signPuntu

ganadores :: [(Int,Puntuacion)] -> [String]
ganadores grupos = winners
        where cantidad1 = filter (\(x,(name,pts)) -> x == 1) grupos
              winners = map nombreTupla cantidad1

intercalarListas :: [String] -> [String] -> [String]
intercalarListas (x:xs) [] = x : []
intercalarListas (x:xs) (y:ys) = x:y: intercalarListas xs ys
intercalarListas _ _ = []

-- Funciones para guardar y cargar la partida
guardarPartida :: [Jugador] -> Int -> Int -> Int -> IO ()
guardarPartida jugadores ronda ordenMano dificult= do 
    let puntuacion = jugadorApuntuacion jugadores
        puntuaciones = escribirStringPuntuaciones puntuacion
        ordenJugs = map ordenJ jugadores
    threadDelay 700000
    putStrLn "Guardando la partida ..."
    threadDelay 700000
    writeFile "guardado.txt" (escribirFicheroRonda puntuaciones ronda ordenMano dificult ordenJugs)
    putStrLn " "
    threadDelay 700000
    putStrLn "Partida guardada con exito!"
    threadDelay 700000
    putStrLn " "

escribirStringPuntuaciones :: [Puntuacion] -> String
escribirStringPuntuaciones [] = []
escribirStringPuntuaciones (x:xs) = (fst x) ++ " " ++ (show (snd x)) ++ "\n" ++ (escribirStringPuntuaciones xs)

escribirFicheroRonda :: String -> Int -> Int -> Int -> [Int] -> String
escribirFicheroRonda puntuaciones ronda ordenMano dificult ordenJugs = "1 \n" ++ puntuaciones ++ (show ronda) ++ "\n" ++ (show ordenMano) ++ "\n" ++ (show dificult) ++ "\n" ++ (show ordenJugs)

reanudarPartida :: String -> IO EstadoPartida
reanudarPartida nombreIn = do contenido <- readFile "guardado.txt"
                              let lineas = lines contenido 
                              putStrLn " "
                              estadoPartida <- sePuedeReanudar nombreIn (tail lineas)
                              return estadoPartida

sePuedeReanudar :: String -> [String] -> IO EstadoPartida
sePuedeReanudar nombreIn lineas = do
    let palabras = map words lineas
        puntosInic = iniciarPuntos (head (head palabras)) ["Alejandro", "Claudia", "Pablo"]
        ronda = read (lineas !! 4) :: Int
        ordenMano = read (lineas !! 5) :: Int
        dificult = read (lineas !! 6) :: Int
        ordenJugs = read (lineas !! 7) :: [Int]
    putStrLn " "
    threadDelay 500000
    putStrLn $ "El nombre de usuario anterior era " ++ (obtenerNomUsuarPunt puntosInic) ++ ". Se ha actualizado a " ++ nombreIn ++ "."
    putStrLn " "
    threadDelay 800000
    let puntuaciones = cambiarNombrePuntuaciones nombreIn $ actualizarPuntos puntosInic $ listaACuatrupla $ segundosLista $ take 4 palabras
        complej = cambioIntDific dificult 
        jugadoresI = inicializarJugadoresReanu puntuaciones
        jugadoresA = actualizarJugadoresReanu jugadoresI ordenJugs
    putStrLn ("La dificultad de la partida que se reanuda era " ++ complej ++ ".")
    putStrLn " "
    threadDelay 800000
    return $ inicializarEstadoPartida jugadoresA ronda ordenMano dificult

-- Funciones auxiliares de reanudarPartida
cambiarNombrePuntuaciones :: String -> [Puntuacion] -> [Puntuacion]
cambiarNombrePuntuaciones nombreIn ((nombreOut,x):xs) = ((nombreIn,x):xs)

iniciarPuntos :: String -> [String] -> [Puntuacion]
iniciarPuntos _ [] = []
iniciarPuntos [] (x:xs) = (x, 0) : iniciarPuntos [] xs
iniciarPuntos nombreIn xs = iniciarPuntos [] (nombreIn : xs)

actualizarPuntos :: [Puntuacion] -> (Int, Int, Int, Int) -> [Puntuacion]
actualizarPuntos [] _ = []  -- Handle the case of an empty list
actualizarPuntos [x] (a, b, c, d) = [(fst x, a + snd x)]
actualizarPuntos (x:xs) (a, b, c, d) = (fst x, a + snd x) : actualizarPuntos xs (b, c, d, 0)

obtenerNomUsuarPunt :: [Puntuacion] -> String
obtenerNomUsuarPunt (x:xs) = fst x
obtenerNomUsuarPunt _ = ""

segundosLista :: [[String]] -> [Int]
segundosLista (x:xs) = (read (x!!1)) : (segundosLista xs)
segundosLista [] = []

listaACuatrupla :: [a] -> (a,a,a,a)
listaACuatrupla [x,y,z,w] = (x,y,z,w)

cambioIntDific :: Int -> String
cambioIntDific 1 = "NIVEL FACIL"
cambioIntDific _ = "NIVEL DIFICIL"

inicializarJugadoresReanu :: [Puntuacion] -> [Jugador]
inicializarJugadoresReanu [] = []
inicializarJugadoresReanu (x:xs) = crearJugadorReanu x : inicializarJugadoresReanu xs 

crearJugadorReanu :: Puntuacion -> Jugador
crearJugadorReanu x = (fst x, [], snd x, 0, 0, 0)

actualizarJugadoresReanu :: [Jugador] -> [Int] -> [Jugador]
actualizarJugadoresReanu (j:listaJug) (o:listaOrd) = (modifOrdenJ j o):actualizarJugadoresReanu listaJug listaOrd
actualizarJugadoresReanu _ _ = []

inicializarEstadoPartida :: [Jugador] -> Int -> Int -> Int -> EstadoPartida
inicializarEstadoPartida xs numRonda ordenMano dif = (xs, Carta Basto Cuatro,[],[],numRonda,ordenMano,dif)

{- Funcion para salir de la partida.
Para forzar la salida, se lanza un error indicando que la partida ha sido suspendida.
-}                                             
salirPartida :: IO()
salirPartida = do
    threadDelay 700000
    putStrLn "Saliendo de la partida... "
    threadDelay 700000
    putStrLn " "
    threadDelay 700000
    putStrLn "¡Hasta pronto!"
    threadDelay 700000
    putStrLn " "
    error "Partida suspendida"
    
{- Esta funcion se encarga de ejecutar el lanzamiento de cartas de cada jugador con la IA facil.
Tambien se encarga de decidir el ganador de cada baza, y de mostrar y actualizar los datos correspondientes
-}
ejecutarCartaF :: EstadoPartida -> Int -> IO EstadoPartida
ejecutarCartaF (listaJug,pinta,baza,descartes,ronda,ordenMano,dif) 4 = do
                let jugGanador = decidirGanadorB listaJug baza pinta
                    hechasJG = hechasJ jugGanador
                    nombreJG = nombreJ jugGanador
                    ordenJG = ordenJ jugGanador
                    jugGanadorA = modifHechasJ jugGanador (hechasJG + 1)
                    listaJugAct = actualizarListaJug listaJug jugGanadorA ordenJG
                    listaJugDef = actualizarOrden listaJugAct ordenJG
                putStrLn (nombreJG ++ " ha ganado esta baza.")
                putStrLn "_____________________________"
                putStrLn " "
                threadDelay 1000000
                return (listaJugDef,pinta,[],descartes ++ baza,ronda,ordenMano,dif)
ejecutarCartaF (listaJug,pinta,baza,descartes,ronda,ordenMano,dif) ordJug
              |esIA (nombreJ jugador) = do let (nombre,mano,puntuacion,pedidas,hechas,orden) = verJug listaJug ordJug
                                               cartaT = fst (tirarCartaF mano baza pinta hechas pedidas)
                                               manoA = snd (tirarCartaF mano baza pinta hechas pedidas)
                                               jugadorA = (nombre,manoA,puntuacion,pedidas,hechas,orden)
                                               listaJugA = actualizarListaJug listaJug jugadorA ordJug
                                           putStrLn ((nombreJ jugador) ++ " ha tirado el: " ++ show cartaT)
                                           putStrLn " "
                                           threadDelay 700000
                                           ejecutarCartaF (listaJugA,pinta,baza ++ [cartaT],descartes,ronda,ordenMano,dif) (ordJug + 1)
              |otherwise = do let manoJug = ordenarManoVis (manoJ jugador) pinta
                                  manoF = ordenarManoVis (filtrarMano manoJug baza pinta) pinta
                                  lenM = length manoF
                              putStrLn ("Tu mano: " ++ mostrarMano manoJug)
                              threadDelay 400000
                              putStrLn ("Las cartas que puedes jugar: " ++ mostrarMano manoF)
                              putStrLn ("                             " ++ mostrarFlechas lenM manoF)
                              putStrLn ("                             " ++ mostrarIndices 0 lenM manoF)
                              putStrLn " "
                              putStr ("Pedidas: " ++ show (pedidasJ jugador) ++ " / " ++ "Hechas: " ++ show (hechasJ jugador))
                              putStrLn ("    " ++ "Pinta: " ++ show pinta)
                              putStrLn " "
                              threadDelay 700000
                              cartaJ <- preguntarCarta manoF
                              putStrLn " "
                              threadDelay 400000
                              let f c = c /= cartaJ
                                  manoJAc = filter f manoJug
                                  jugadorAc = modifManoJ jugador manoJAc
                                  listaJugAc = actualizarListaJug listaJug jugadorAc ordJug
                              ejecutarCartaF (listaJugAc,pinta,baza ++ [cartaJ],descartes,ronda,ordenMano,dif) (ordJug + 1)
              where jugador = verJug listaJug ordJug

-- Lo mismo con la IA dificil. De nuevo, hay dos funciones distintas por los distintos tipos de EstadoPartida.
ejecutarCartaD :: EstadoPartidaM -> Int -> IO EstadoPartidaM
ejecutarCartaD (listaJugM,pinta,baza,descartes,ronda,ordenMano,dif) 4 = do
                let jugGanadorM = decidirGanadorBM listaJugM baza pinta
                    hechasJMG = hechasJM jugGanadorM
                    nombreJMG = nombreJM jugGanadorM
                    ordenJMG = ordenJM jugGanadorM
                    lMarcas = map ultMarJM listaJugM
                    lActMarcas = evaluarActMar lMarcas ordenJMG
                    jugGanadorMA = modifHechasJM jugGanadorM (hechasJMG + 1)
                    listaJugMAct = actualizarListaJugM listaJugM jugGanadorMA ordenJMG
                    listaJugMD = actualizarOrdenM listaJugMAct ordenJMG
                    listaJugMDef = actualizarMarcas listaJugMD lActMarcas (listaJugM,pinta,baza,descartes,ronda,ordenMano,dif)
                putStrLn (nombreJMG ++ " ha ganado esta baza.")
                putStrLn "_____________________________"
                putStrLn " "
                threadDelay 1000000
                return (listaJugMDef,pinta,[],descartes ++ baza,ronda,ordenMano,dif)
ejecutarCartaD (listaJugM,pinta,baza,descartes,ronda,ordenMano,dif) ordJug
              |esIA (nombreJM jugadorM) = do let (nombre,manoM,puntuacion,pedidas,hechas,orden,ultMar) = verJugM listaJugM ordJug
                                                 ronAct = length manoM
                                                 cartaT = fst (tirarCartaD manoM pinta descartes baza hechas pedidas ronAct)
                                                 manoMA = snd (tirarCartaD manoM pinta descartes baza hechas pedidas ronAct)
                                                 ultMarJ = esMarcada manoM cartaT
                                                 jugadorMA = (nombre,manoMA,puntuacion,pedidas,hechas,orden,ultMarJ)
                                                 listaJugMA = actualizarListaJugM listaJugM jugadorMA ordJug
                                             putStrLn ((nombreJM jugadorM) ++ " ha tirado el: " ++ show cartaT)
                                             putStrLn " "
                                             threadDelay 700000
                                             ejecutarCartaD (listaJugMA,pinta,baza ++ [cartaT],descartes,ronda,ordenMano,dif) (ordJug + 1)
              |otherwise = do let manoJugM = marcadasJM jugadorM
                                  manoJug = ordenarManoVis (desmarcarMano manoJugM) pinta
                                  manoF = ordenarManoVis (filtrarMano manoJug baza pinta) pinta
                                  lenM = length manoF
                              putStrLn ("Tu mano: " ++ mostrarMano manoJug)
                              threadDelay 400000
                              putStrLn ("Las cartas que puedes jugar: " ++ mostrarMano manoF)
                              putStrLn ("                             " ++ mostrarFlechas lenM manoF)
                              putStrLn ("                             " ++ mostrarIndices 0 lenM manoF)
                              putStr ("Pedidas: " ++ show (pedidasJM jugadorM) ++ " / " ++ "Hechas: " ++ show (hechasJM jugadorM))
                              putStrLn ("    " ++ "Pinta: " ++ show pinta)
                              putStrLn " "
                              threadDelay 700000
                              cartaJ <- preguntarCarta manoF
                              putStrLn " "
                              threadDelay 400000
                              let f c = (fst c) /= cartaJ
                                  manoJMAc = filter f manoJugM
                                  jugadorMAc = modifMarcadasJM jugadorM manoJMAc
                                  listaJugMAc = actualizarListaJugM listaJugM jugadorMAc ordJug
                              ejecutarCartaD (listaJugMAc,pinta,baza ++ [cartaJ],descartes,ronda,ordenMano,dif) (ordJug + 1)
              where jugadorM = verJugM listaJugM ordJug               

-- Funciones auxiliares de ejecutarCarta                                     
esIA :: String -> Bool
esIA nombreJ = perteneceALista nombreJ ["Alejandro","Claudia","Pablo"]

perteneceALista :: Eq a => a -> [a] -> Bool
perteneceALista x xs = or $ map (== x) xs

verJug :: [Jugador] -> Int -> Jugador
verJug listaJug ordJug = head $ filter f listaJug
    where f j = (ordenJ j) == ordJug

verJugM :: [JugadorM] -> Int -> JugadorM
verJugM listaJugM ordJug = head $ filter f listaJugM
    where f j = (ordenJM j) == ordJug

actualizarListaJug :: [Jugador] -> Jugador -> Int -> [Jugador]
actualizarListaJug [] _ _ = []
actualizarListaJug (j:listaJug) jug ordJug
    |ordenJ j == ordJug = jug:listaJug
    |otherwise = j:(actualizarListaJug listaJug jug ordJug)

actualizarListaJugM :: [JugadorM] -> JugadorM -> Int -> [JugadorM]
actualizarListaJugM [] _ _ = []
actualizarListaJugM (j:listaJug) jug ordJug
    |ordenJM j == ordJug = jug:listaJug
    |otherwise = j:(actualizarListaJugM listaJug jug ordJug)

-- Para mostrar la mano de forma mas agradable, utilizamos estas tres funciones
mostrarMano :: Mano -> String
mostrarMano [] = ""
mostrarMano [c] = show c
mostrarMano (c:mano) = (show c) ++ " | " ++ mostrarMano mano

mostrarFlechas :: Int -> Mano -> String
mostrarFlechas 0 _ = ""
mostrarFlechas n (c:mano)
    |perteneceALista (valorC c) [Sota,Caballo,Rey] = "  ^    " ++ mostrarFlechas (n-1) mano
    |otherwise = " ^    " ++ mostrarFlechas (n-1) mano

mostrarIndices :: Int -> Int -> Mano -> String
mostrarIndices _ 0 _ = ""
mostrarIndices i n (c:mano) 
    |perteneceALista (valorC c) [Sota,Caballo,Rey] = "  " ++ (show i) ++ "    " ++ mostrarIndices (i+1) (n-1) mano
    |otherwise = " " ++ (show i) ++ "    " ++ mostrarIndices (i+1) (n-1) mano

-- Las siguientes funciones se encargan de decidir quien gana la baza
decidirGanadorB :: [Jugador] -> Baza -> Pinta -> Jugador
decidirGanadorB listaJug baza pinta = verJug listaJug i
    where cartaG = hallarCartaGan baza pinta
          i = indiceCartaGan baza cartaG

decidirGanadorBM :: [JugadorM] -> Baza -> Pinta -> JugadorM
decidirGanadorBM listaJugM baza pinta = verJugM listaJugM i
    where cartaG = hallarCartaGan baza pinta
          i = indiceCartaGan baza cartaG

hallarCartaGan :: Baza -> Pinta -> Carta
hallarCartaGan baza pinta
    |hayPalo baza (paloC pinta) = last $ ordenarManoF baza pinta
    |otherwise = last $ ordenarManoF bazaF pinta
    where paloS = paloC (head baza)
          bazaF = filtrarPalo baza paloS

indiceCartaGan :: Baza -> Carta -> Int
indiceCartaGan baza cartaG = indiceCartaGan' baza cartaG 0

indiceCartaGan' :: Mazo -> Carta -> Int -> Int
indiceCartaGan' [] _ _ = -1
indiceCartaGan' (c:mano) cartaG n
    |c == cartaG = n
    |otherwise = indiceCartaGan' mano cartaG (n+1)

-- Mas funciones auxiliares (sobre todo actualizacion de datos)
actualizarOrden :: [Jugador] -> Int -> [Jugador]
actualizarOrden [] _ = []
actualizarOrden (j:listaJug) ordG = (modifOrdenJ j (mod (ord - ordG) 4)):actualizarOrden listaJug ordG
    where ord = ordenJ j

actualizarOrdenM :: [JugadorM] -> Int -> [JugadorM]
actualizarOrdenM [] _ = []
actualizarOrdenM (j:listaJugM) ordG = (modifOrdenJM j (mod (ord - ordG) 4)):actualizarOrdenM listaJugM ordG
    where ord = ordenJM j

actualizarOrdenR :: [Jugador] -> Int -> [Jugador]
actualizarOrdenR listaJug ord = actualizarOrdenR' listaJug ord 0

actualizarOrdenR' :: [Jugador] -> Int -> Int -> [Jugador]
actualizarOrdenR' _ _ 4 = []
actualizarOrdenR' (j:listaJug) ord n = (modifOrdenJ j (mod (n - ord) 4)):actualizarOrdenR' listaJug ord (n+1)

evaluarActMar :: [Bool] -> Int -> [Int]
evaluarActMar lMarcas ord = evaluarActMar' lMarcas ord 0

evaluarActMar' :: [Bool] -> Int -> Int -> [Int]
evaluarActMar' [] _ _ = []
evaluarActMar' (m:lMarcas) ord n
    |m && (n /= ord) = (-1):evaluarActMar' lMarcas ord (n+1)
    |not m && (n == ord) = 1:evaluarActMar' lMarcas ord (n+1)
    |otherwise = 0:evaluarActMar' lMarcas ord (n+1)

actualizarMarcas :: [JugadorM] -> [Int] -> EstadoPartidaM -> [JugadorM]
actualizarMarcas (j:listaJugM) (n:lActMar) estPar
    |esIA nomJ = (actualizarMarcaJug j n estPar):actualizarMarcas listaJugM lActMar estPar
    |otherwise = j:actualizarMarcas listaJugM lActMar estPar
    where nomJ = nombreJM j
actualizarMarcas _ _ _ = []

actualizarMarcaJug :: JugadorM -> Int -> EstadoPartidaM -> JugadorM
actualizarMarcaJug jugM cambMar (listaJugM,pinta,baza,descartes,ronda,ordenMano,dif) = modifMarcadasJM jugM manoMA
    where manoM = marcadasJM jugM
          manoMA = actualizarMarcadas manoM pinta descartes ronda cambMar

-- Funcion que ordena la mano por valor y por palo, de modo que el palo de la pinta se muestra antes que el resto.         
ordenarManoVis :: Mano -> Pinta -> Mano
ordenarManoVis [] _ = []
ordenarManoVis (c:mano) pinta = mayores ++ [c] ++ menores
    where mayores = ordenarManoVis [carta | carta <- mano, compararCartaVis carta c pinta] pinta
          menores = ordenarManoVis [carta | carta <- mano, not (compararCartaVis carta c pinta)] pinta

compararCartaVis :: Carta -> Carta -> Pinta -> Bool
compararCartaVis c1 c2 pinta
        |paloC c1 == paloC pinta && paloC c1 /= paloC c2 = True
        |paloC c1 > paloC c2 = True
        |paloC c1 == paloC c2 && valorC c1 > valorC c2 = True
        |otherwise = False

-- Menu que pregunta por el indice de la carta que se va a jugar          
preguntarCarta :: Mano -> IO Carta
preguntarCarta mano = do putStr "Elige el indice de la carta que vas a jugar: "
                         n <- getLine
                         let i = (read n) :: Int
                             lenM = (length mano) - 1
                         if perteneceALista i [0..lenM] then return (mano !! i)
                         else do putStrLn "Indice incorrecto"
                                 preguntarCarta mano

--IA nivel facil
pedirNumBazasF :: Mano -> Pinta -> Pedidas -> Int -> Int    --Pide una baza por cada triunfo o cada carta 
pedirNumBazasF mano pinta pedidas ronda
    |suma + bazas == ronda && (length pedidas) == 3 && bazas == ronda = bazas - 1
    |suma + bazas == ronda && (length pedidas) == 3 = bazas + 1    --El desajuste
    |otherwise = bazas
    where suma = sum pedidas
          bazas = pedirNumBazasF' mano pinta
          
--Pide el numero de bazas en base a los triunfos y el valor de las cartas con un criterio sencillo (ases y triunfos por encima del siete de copas).
pedirNumBazasF' :: Mano -> Pinta -> Int     
pedirNumBazasF' [] _ = 0
pedirNumBazasF' ((Carta paloM valorM):ms) (Carta paloP valorP)
    |valorM == As = 1 + pedirNumBazasF' ms (Carta paloP valorP)
    |paloM == paloP && valorM > Siete = 1 + pedirNumBazasF' ms (Carta paloP valorP)
    |otherwise = pedirNumBazasF' ms (Carta paloP valorP)

{- Para saber que carta tirar, primero la ordenamos en funcion de si es triunfo o no y de su valor.
Luego filtramos la mano para saber que cartas podemos jugar.
Por ultimo, en funcion de si aun nos quedan cartas por hacernos o no, tiramos la carta mas alta o la mas baja que tengamos.
-}
tirarCartaF :: Mano -> Baza -> Pinta -> Int -> Int -> (Carta,Mano)
tirarCartaF mano baza pinta hechas pedidas = (cartaT,filter f mano)
    where manoO = ordenarManoF mano pinta
          cartaT = tirarCartaF' manoO baza pinta hechas pedidas
          f c = c /= cartaT

tirarCartaF' :: Mano -> Baza -> Pinta -> Int -> Int -> Carta   --Suponemos la mano ya ordenada segun la pinta
tirarCartaF' mano [] _ hechas pedidas
    |hechas < pedidas = last mano
    |otherwise = head mano
tirarCartaF' mano baza pinta hechas pedidas
    |hechas < pedidas = last validas
    |otherwise = head validas
    where validas = filtrarMano mano baza pinta

filtrarMano :: Mano -> Baza -> Pinta -> Mano     --Esta función filtra las cartas de la mano que se pueden jugar en función de la carta de la que sale el rival
filtrarMano mano baza pinta
    |baza == [] = mano
    |hayCartaMejor mano cartaS && not (hayPalo baza (paloC pinta)) && (paloC cartaS /= paloC pinta) = filtrarCartaMejor mano cartaS
    |hayPalo mano (paloC cartaS) && (paloC cartaS /= paloC pinta) = filtrarPalo mano (paloC cartaS)
    |hayCartaMejor mano cartaS && (paloC cartaS == paloC pinta) = filtrarCartaMejor mano cartaS
    |hayPalo mano (paloC cartaS) && (paloC cartaS == paloC pinta) = filtrarPalo mano (paloC cartaS)
    |hayPalo mano (paloC pinta) && not (hayPalo baza (paloC pinta)) = filtrarPalo mano (paloC pinta)
    |hayPalo mano (paloC pinta) && (hayPalo baza (paloC pinta)) && hayCartaMejor mano cartaA = filtrarCartaMejor mano cartaA
    |otherwise = mano
    where cartaSal = head baza
          cartaS = cartaMasAlta baza (paloC cartaSal)
          cartaA = cartaMasAlta baza (paloC pinta)

filtrarCartaMejor :: Mano -> Carta -> Mano
filtrarCartaMejor [] _ = []
filtrarCartaMejor ((Carta paloM valorM):ms) (Carta paloS valorS)
    |paloM == paloS && valorM > valorS = (Carta paloM valorM):filtrarCartaMejor ms (Carta paloS valorS)
    |otherwise = filtrarCartaMejor ms (Carta paloS valorS)

compararCartaP :: Palo -> Carta -> Carta -> Carta   --Hacemos un comparador mas simple solo para la baza
compararCartaP palo carta1 carta2
    |(paloC carta1) /= palo && (paloC carta2) == palo = carta2
    |(paloC carta1) == palo && (paloC carta2) == palo && (valorC carta1) < (valorC carta2) = carta2
    |otherwise = carta1

compararCartaF :: Palo -> Carta -> Carta -> Bool    --Devuelve True si carta1 "<" carta2 y False en caso contrario
compararCartaF paloP (Carta palo1 valor1) (Carta palo2 valor2)
    |palo1 /= paloP && palo2 == paloP = True
    |palo1 /= paloP && palo2 /= paloP && valor1 < valor2 = True
    |palo1 == paloP && palo2 == paloP && valor1 < valor2 = True
    |otherwise = False

ordenarManoF :: Mano -> Pinta -> Mano    --Ordenamos la mano poniendo al final los triunfos y al principio el resto de cartas por orden de valor
ordenarManoF [] _ = []
ordenarManoF (cartaM:ms) pinta = (ordenarManoF [carta | carta <- ms, compararCartaF paloP carta cartaM] pinta) ++ [cartaM] ++ (ordenarManoF [carta | carta <- ms, not (compararCartaF paloP carta cartaM)] pinta)
    where paloP = paloC pinta

--Las siguientes funciones son generales de ambos niveles de dificultad de las IA

hayCartaMejor :: Mano -> Carta -> Bool
hayCartaMejor ms carta = or $ map (esCartaMejor carta) ms

esCartaMejor :: Carta -> Carta -> Bool
esCartaMejor cartaS cartaM = ((paloC cartaM) == paloC cartaS) && ((valorC cartaM) > valorC cartaS)

cartaMasAlta :: Baza -> Palo -> Carta
cartaMasAlta baza palo = foldr1 (compararCartaP palo) baza

filtrarValor :: Mano -> Valor -> Mano
filtrarValor mano valor = filter (esValor valor) mano

esValor :: Valor -> Carta -> Bool
esValor valor carta = (valor == valorC carta)

filtrarPalo :: Mano -> Palo -> Mano
filtrarPalo mano palo = filter (esPalo palo) mano

eliminarPalo :: Mano -> Palo -> Mano
eliminarPalo [] palo = []
eliminarPalo (m:ms) palo
    |paloC m == palo = eliminarPalo ms palo
    |otherwise = m:eliminarPalo ms palo

hayPalo :: Mano -> Palo -> Bool
hayPalo ms palo = or $ map (esPalo palo) ms

noHayPalo :: Mano -> Palo -> Bool
noHayPalo ms palo = or $ map (noEsPalo palo) ms

esPalo :: Palo -> Carta -> Bool
esPalo palo carta = (palo == paloC carta)

noEsPalo :: Palo -> Carta -> Bool
noEsPalo palo carta = not (esPalo palo carta)

contarNumValor :: Mano -> Valor -> Int
contarNumValor mano valor = length $ filter (== valor) (map valorC mano)

contarNumPalo :: Mano -> Palo -> Int
contarNumPalo mano palo = length $ filter (== palo) (map paloC mano)

cartasRestantesPalo :: Mano -> Mazo -> Pinta -> Int -> Palo -> Int
cartasRestantesPalo mano descartes pinta ronda palo 
    |ronda == 10 = 10 - (length $ filtrarPalo (mano ++ descartes) palo)
    |otherwise = 10 - (length $ filtrarPalo (mano ++ descartes ++ [pinta]) palo)

perteneceAMano :: Carta -> Mano -> Bool
perteneceAMano carta mano = or $ map (== carta) mano
    
--IA nivel dificil
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*factorial (n-1)

combinatorio :: Int -> Int -> Integer  -- Una implementación de los numeros combinatorios.
combinatorio n k 
    |n < k = 0
    |otherwise = div (factorial (toInteger n)) ((factorial (toInteger k))*(factorial (toInteger(n-k))))

{- El pilar fundamental de la IA dificil es calcular a priori y con precisión la probabilidad de llevarnos una carta a priori, es decir,
que probabilidad habria de llevarnos la baza si tiramos esa carta de primeras.
Para ello, tenemos que tener en cuenta la carta, la mano, la pinta, los descartes, la ronda (ya que en la ronda de 10 todas las cartas
están repartidas y la pinta la tiene alguien, lo cual simplifica los cálculos) y la probabilidad de fallo en caso de tirar una carta
que no sea triunfo.
Para ello utilizamos una serie de funciones auxiliares que cubren cada caso.
-}
probAPriori :: Carta -> Mano -> Pinta -> Mazo -> Int -> Float   
probAPriori carta mano pinta descartes ronda
    |(paloC carta == paloC pinta) && ronda < 10 = probAPrioriT carta mano pinta descartes
    |(paloC carta == paloC pinta) && ronda == 10 = probAPrioriTr10 carta mano pinta descartes
    |ronda < 10 = probAPrioriNT carta mano pinta descartes
    |otherwise = probAPrioriNTr10 carta mano pinta descartes

probAPrioriT :: Carta -> Mano -> Pinta -> Mazo -> Float
probAPrioriT carta mano pinta descartes = (fromIntegral (combinatorio (n-a) r)) / (fromIntegral (combinatorio n r))
    where n = 40 - (length mano + 1)     --Nº de cartas sin revelar
          a = 9 - fromEnum (valorC carta) - (length $ filtrarCartaMejor (mano ++ [pinta] ++ descartes) carta)    --Nº de cartas mayores que pueden tener los rivales
          r = 3 * length mano    --Nº de cartas rivales sin revelar

probAPrioriTr10 :: Carta -> Mano -> Pinta -> Mazo -> Float
probAPrioriTr10 carta mano pinta descartes
    |a == 0 = 1
    |otherwise = 0
    where a = 9 - fromEnum (valorC carta) - (length $ filtrarCartaMejor (mano ++ descartes) carta)

probAPrioriNT :: Carta -> Mano -> Pinta -> Mazo -> Float
probAPrioriNT carta mano pinta descartes = (probAPrioriST carta mano pinta descartes) * (1 - probFallo carta mano pinta descartes)

probAPrioriNTr10 :: Carta -> Mano -> Pinta -> Mazo -> Float
probAPrioriNTr10 carta mano pinta descartes
    |a > 0 = 0
    |paloC carta == paloC pinta = 1
    |otherwise = probFallo carta mano pinta descartes
    where a = 9 - fromEnum (valorC carta) - (length $ filtrarCartaMejor (mano ++ descartes) carta)

probAPrioriST :: Carta -> Mano -> Pinta -> Mazo -> Float
probAPrioriST carta mano pinta descartes = (fromIntegral (combinatorio (n-a) r)) / (fromIntegral (combinatorio n r))
    where n = 40 - (length mano + length descartes + 1)     --Nº de cartas sin revelar
          a = 9 - fromEnum (valorC carta) - (length $ filtrarCartaMejor (mano ++ descartes) carta)    --Nº de cartas mayores que pueden tener los rivales
          r = 3 * length mano    --Nº de cartas rivales sin revelar

probUnFallo :: Carta -> Mano -> Pinta -> Mazo -> Float
probUnFallo carta mano pinta descartes = pNoPalo * pTriunfo
    where n = 40 - (length mano + length descartes + 1)
          a1 = 9 - (length $ filtrarPalo (mano ++ descartes) (paloC carta))
          a2 = 9 - (length $ filtrarPalo (mano ++ descartes) (paloC pinta))
          r = length mano
          pNoPalo = (fromIntegral (combinatorio (n-a1) r)) / (fromIntegral (combinatorio n r))
          pTriunfo = 1 - (fromIntegral (combinatorio (n-a2) r)) / (fromIntegral (combinatorio n r))

probFallo :: Carta -> Mano -> Pinta -> Mazo -> Float
probFallo carta mano pinta descartes = 1 - (1 - probUnFallo carta mano pinta descartes) ** 3

esProbEntre :: Float -> Float -> Mano -> Pinta -> Mazo -> Int -> Carta -> Bool
esProbEntre pInf pSup mano pinta descartes ronda carta = (pInf <= pCarta) && (pCarta <= pSup)
    where pCarta = probAPriori carta mano pinta descartes ronda

{- Esta IA tiene un criterio mucho mas sofisticado para pedir el numero de bazas que se quiere hacer. Tiene en cuenta:
- Triunfos: En función de la ronda, divide los triunfos en tres grupos: altos, medios y bajos, en funcion de la probabilidad a priori.
Siempre pide los altos, la mitad de los medios salvo que vayan bien acompañados y los bajos solo a partir del 2/3/4 triunfo dependiendo
de la ronda.
-Ases: Pide todos los ases.
-Treses: Pide una cantidad variable de los treses que hay segun la ronda y si van acompañados de ases
-Doses: Solo pide algunos si van bien acompañados y hay triunfos (para evitar el fallo)
-Fallo: Si tenemos triunfos bajos (no solemos contar con ellos) y no tenemos cartas de un palo, pide una baza más.
-Desajuste: Cuando somos postre tenemos varias situaciones:
    + En caso de que la suma de todas las pedidas vaya muy por debajo del numero de ronda, pide una mas.
    + En caso de que la suma de todas las pedidas vaya muy por encima del numero de ronda, pide una menos.
    + En caso de que la suma de todas las pedidas sea el nuemro de ronda, salvo que no sea posible pide una mas (suele ser conservadora).
    + En cualquier otro caso se pide las que tenia pensado pedirse
-}
pedirNumBazasD :: Mano -> Pinta -> Pedidas -> Int -> Int
pedirNumBazasD mano pinta pedidas ronda
    |length pedidas == 3 && sum pedidas + apuestaI >= ronda + 2 && apuestaI /= 0 = apuestaI - 1
    |length pedidas == 3 && sum pedidas + apuestaI <= ronda - 3 = apuestaI + 1
    |length pedidas == 3 && sum pedidas + apuestaI == ronda && apuestaI == ronda = apuestaI - 1
    |length pedidas == 3 && sum pedidas + apuestaI == ronda = apuestaI + 1
    |otherwise = apuestaI
    where apuestaI = pedirNumBazasSD mano pinta ronda

pedirNumBazasSD :: Mano -> Pinta -> Int -> Int
pedirNumBazasSD mano pinta ronda
    |ronda /= 1 = pedirTriunfos mano pinta ronda + pedirAses mano pinta + pedirTreses mano pinta ronda + pedirReyes mano pinta ronda + pedirFallo mano pinta ronda
    |hayPalo mano (paloC pinta) = 1
    |otherwise = 0

divTriunfos :: Mano -> Pinta -> Int -> (Mano,Mano,Mano)    
divTriunfos mano pinta ronda
    |ronda < 10 = (altasMen10,medianasMen10,bajasMen10)
    |otherwise = (altas10,medianas10,bajas10)
    where pAltas = ajustarProbAltas ronda
          pBajas = ajustarProbBajas ronda
          probCarta carta = probAPrioriT carta mano pinta []
          paloP = paloC pinta
          altasMen10 = [carta | carta <- mano, probCarta carta >= pAltas]
          medianasMen10 = [carta | carta <- mano, probCarta carta < pAltas, probCarta carta > pBajas]
          bajasMen10 = [carta | carta <- mano, probCarta carta < pBajas]
          altas10 = [carta | carta <- mano, (probAPrioriTr10 carta mano pinta []) == 1]
          medianas10 = [carta | carta <- mano, aux <- [Carta paloP Tres,Carta paloP Rey,Carta paloP Caballo,Carta paloP Sota], carta == aux, (probAPrioriTr10 carta mano pinta []) == 0]
          bajas10 = [carta | carta <- mano, aux <- [Carta paloP Dos,Carta paloP Cuatro,Carta paloP Cinco,Carta paloP Seis,Carta paloP Siete], carta == aux]

ajustarProbAltas :: Int -> Float
ajustarProbAltas ronda = 0.81 - 0.03 * (fromIntegral ronda)

ajustarProbBajas :: Int -> Float
ajustarProbBajas ronda = 0.17 - 0.013 * (fromIntegral ronda)

pedirTriunfos :: Mano -> Pinta -> Int -> Int    
pedirTriunfos mano pinta ronda
    |length triunfos == 1 = pedirTriunfos1 mano pinta ronda
    |or $ map (== ronda) [2,3,4] = nAltas + nMedias + pedirBajas altas medias bajas 2
    |or $ map (== ronda) [5,6,7,8] = nAltas + nMedias + pedirBajas altas medias bajas 3
    |or $ map (== ronda) [9,10] = nAltas + nMedias + pedirBajas altas medias bajas 4
    |otherwise = 0
    where triunfos = filtrarPalo mano (paloC pinta)
          (altas,medias,bajas) = divTriunfos triunfos pinta ronda
          nAltas = length altas
          nMedias = pedirMedias altas medias ronda

pedirTriunfos1 :: Mano -> Pinta -> Int -> Int 
pedirTriunfos1 mano pinta ronda 
    |ronda == 10 = round $ probAPrioriTr10 triunfo mano pinta []
    |(probAPrioriT triunfo mano pinta []) > 0.6 = 1
    |otherwise = 0
    where triunfo = head $ filtrarPalo mano (paloC pinta)

pedirMedias :: Mano -> Mano -> Int -> Int
pedirMedias _ [] _ = 0
pedirMedias [] medias ronda
    |ronda <= 7 = length medias - 1
    |otherwise = floor $ (fromIntegral (length medias)) / 2
pedirMedias _ medias ronda 
    |ronda <= 7 = ceiling $ 2 * (fromIntegral (length medias)) / 3
    |otherwise = ceiling $ (fromIntegral (length medias)) / 2

pedirBajas :: Mano -> Mano -> Mano -> Int -> Int
pedirBajas altas medias bajas num
    |(length (altas ++ medias)) >= num = nBajas
    |(length (altas ++ medias ++ bajas)) >= num && nBajas > 0 = nBajas - (num - length (altas ++ medias))
    |otherwise = 0
    where nBajas = length bajas

pedirAses :: Mano -> Pinta -> Int
pedirAses mano pinta = length $ filtrarValor manoST As
    where manoST = eliminarPalo mano (paloC pinta)

pedirTreses :: Mano -> Pinta -> Int -> Int
pedirTreses mano pinta ronda
    |numTriunfos == 0 && (or $ map (== ronda) [2,3,4,5]) = floor ((2 * fromIntegral nTresAcomp) / 3) + floor (fromIntegral (nTres - nTresAcomp) / 3)
    |numTriunfos == 0 = floor (fromIntegral nTresAcomp / 2)
    |(or $ map (== ronda) [2,3,4,5]) || ((or $ map (== ronda) [6,7,8,9,10]) && numTriunfos >= 2) = nTresAcomp + floor (fromIntegral (nTres - nTresAcomp) / 2)
    |otherwise = floor ((2 * fromIntegral nTresAcomp) / 3) + floor (fromIntegral (nTres - nTresAcomp) / 3)
    where manoST = eliminarPalo mano (paloC pinta)
          nTres = length $ filtrarValor manoST Tres
          nTresAcomp = numValorAcompanado manoST Tres
          numTriunfos = length mano - length manoST

pedirReyes :: Mano -> Pinta -> Int -> Int
pedirReyes mano pinta ronda
    |numTriunfos >= 1 && (or $ map (== ronda) [2,3,4]) = floor ((2 * fromIntegral nReyesAcomp) / 3) + floor (fromIntegral (nReyes - nReyesAcomp) / 3)
    |numTriunfos >= 2 && (or $ map (== ronda) [5,6,7,8,9,10]) = floor ((2 * fromIntegral nReyesAcomp) / 3)
    |otherwise = 0
    where manoST = eliminarPalo mano (paloC pinta)
          nReyes = length $ filtrarValor manoST Rey
          nReyesAcomp = numValorAcompanado manoST Rey
          numTriunfos = length mano - length manoST

numValorAcompanado :: Mano -> Valor -> Int
numValorAcompanado mano valor = length $ filter ((flip esValorAcompanado) mano) (filtrarValor mano valor)

esValorAcompanado :: Carta -> Mano -> Bool
esValorAcompanado _ [] = False
esValorAcompanado carta (m:ms)
    |valorC m == As && (paloC m) == paloC carta = True
    |otherwise = esValorAcompanado carta ms

pedirFallo :: Mano -> Pinta -> Int -> Int
pedirFallo mano pinta ronda
    |(or $ map (== ronda) [1,2,3]) && hayFallo mano pinta && length bajas == 1 && length triunfos == 1 = 1
    |(or $ map (== ronda) [4,5,6,7]) && hayFallo mano pinta && length (medias ++ bajas) > 0 && length triunfos <= 2 = 1
    |(or $ map (== ronda) [8,9,10]) && hayFallo mano pinta && length (medias ++ bajas) > 0 && length triunfos <= 3 = 1
    |otherwise = 0
    where triunfos = filtrarPalo mano (paloC pinta)
          (altas,medias,bajas) = divTriunfos triunfos pinta ronda

hayFallo :: Mano -> Pinta -> Bool
hayFallo mano pinta = hayPalo mano (paloC pinta) && (or $ map (noHayPalo mano) ([Oro,Copa,Espada,Basto] \\ [paloC pinta]))

{- El criterio para tirar cartas también es mucho mas sofisticado qen en el nivel facil. El concepto clave es el marcaje de cartas:
En función de las bazas pedidas, marcamos tantas cartas como bazas pedidas, en función de triunfos y probabilidades a priori.
Estas marcas nos guían para saber que carta echar en cada caso, según si:
    + Salimos nosotros o no.
    + Aun nos quedan cartas por hacernos (marcas) o ya nos las hemos hecho.
En caso de o bien no hacernos una carta marcada o bien hacernos una no marcada, reajustamos la marca en funcion de la probabilidad
a priori del resto de cartas.
-}
tirarCartaD :: Marcadas -> Pinta -> Mazo -> Baza -> Int -> Int -> Int -> (Carta,Marcadas)
tirarCartaD manoM pinta descartes baza hechas pedidas ronda = (cartaT,filter f manoM)
    where cartaT = tirarCartaD' manoM pinta descartes baza hechas pedidas ronda
          f m = (fst m) /= cartaT

tirarCartaD' :: Marcadas -> Pinta -> Mazo -> Baza -> Int -> Int -> Int -> Carta
tirarCartaD' manoM pinta descartes baza hechas pedidas ronda
    |hechas < pedidas && baza == [] = tirarCartaSalNH manoM pinta descartes ronda
    |hechas < pedidas = tirarCartaBazaNH manoM pinta descartes baza ronda
    |baza == [] = tirarCartaSalH manoM pinta descartes ronda
    |otherwise = tirarCartaBazaH manoM pinta descartes baza ronda

{- En el caso en el que salimos y aun quedan cartas por hacernos:
- Si tenemos una carta marcada de un palo y es la única que queda la tiramos.
- Si no tenemos una carta marcada muy probable y tenemos alguna carta no marcada con probabilidad ligeramente mas alta
  de lo que nos gustaria la tiramos, para poder reajustar en caso de hacernosla.
- En caso contrario tira la carta marcada mas probable
-}
tirarCartaSalNH :: Marcadas -> Pinta -> Mazo -> Int -> Carta
tirarCartaSalNH manoM pinta descartes ronda
    |(cartasRestantesPalo mano descartes pinta ronda (paloC pinta)) == 0 && (hayPaloMarcado manoM (paloC pinta)) = tirarUnica manoM (paloC pinta)
    |(cartasRestantesPalo mano descartes pinta ronda (paloC pinta)) == 0 && (or $ zipWith (&&) palosMarcados (map (== 0) numCartasRivNT)) = tirarUnicaNT manoM pinta descartes ronda
    |(probAPriori cartaMP mano pinta descartes ronda) < 0.8 && length cartasEnRango > 0 = head $ ordenarProb cartasEnRango pinta descartes ronda
    |otherwise = head $ ordenarProb (filtrarMarcadas manoM) pinta descartes ronda
    where mano = desmarcarMano manoM
          cartaMP = head $ ordenarProb (filtrarMarcadas manoM) pinta descartes ronda
          palosMarcados = map (hayPaloMarcado manoM) ([Oro,Copa,Espada,Basto] \\ [paloC pinta])
          numCartasRivNT = map (cartasRestantesPalo mano descartes pinta ronda) ([Oro,Copa,Espada,Basto] \\ [paloC pinta])
          cartasEnRango = filter (esProbEntre 0.15 0.3 mano pinta descartes ronda) (mano \\ filtrarMarcadas manoM)

{- En el caso en el que salimos y ya no quedan cartas por hacernos:
- Si tenemos pinta tiramos la mas alta
- Si no tiramos la carta que mas se acerque a tener un 15% de probabilidad de llevarnosla
-}
tirarCartaSalH :: Marcadas -> Pinta -> Mazo -> Int -> Carta
tirarCartaSalH manoM pinta descartes ronda
    |hayPalo mano (paloC pinta) = head $ ordenarProb (filtrarPalo mano (paloC pinta)) pinta descartes ronda
    |length cartasEnRango > 0 = head $ ordenarProb cartasEnRango pinta descartes ronda
    |otherwise = last $ ordenarProb mano pinta descartes ronda
    where mano = desmarcarMano manoM
          cartasEnRango = filter (esProbEntre 0 0.15 mano pinta descartes ronda) mano

{- En el caso en el que no salimos y aun quedan cartas por hacernos se filtra la mano y:
- Si tenemos una carta marcada en la que confiamos y aun nos la podemos hacer la tiramos.
- Si solo tenemos cartas marcadas en las que no confiamos aun pudiendo hacernoslas tiramos la mas baja.
- En caso contrario tiramos la mas baja que podamos.
-}
tirarCartaBazaNH :: Marcadas -> Pinta -> Mazo -> Baza -> Int -> Carta
tirarCartaBazaNH manoM pinta descartes baza ronda
    |esSuperada mano pinta baza && hayCartaMejor mano cartaC && hayPaloMarcado manoFM (paloC cartaS) && probCartaA > 0.7 = cartaA
    |esSuperada mano pinta baza && hayPaloMarcado manoFM (paloC pinta) = cartaB
    |otherwise = last $ ordenarProb manoF pinta descartes ronda
    where mano = desmarcarMano manoM
          manoF = filtrarMano mano baza pinta
          manoFM = filtrarManoMarcada manoM baza pinta 
          cartaS = head baza
          cartaC = cartaMasAlta baza (paloC cartaS) 
          cartaA = head $ ordenarProb (desmarcarMano manoFM) pinta descartes ronda
          cartaB = last $ ordenarProb (desmarcarMano manoFM) pinta descartes ronda
          probCartaA = probAPriori cartaA mano pinta descartes ronda

{- En el caso en el que no salimos y ya no quedan cartas por hacernos se filtra la mano y:
- Si no superamos tiramos la carta con mayor probabilidad que podamos.
- En cado contrario tiramos la carta con probabilidad que mas se acerque al 20%.
-}
tirarCartaBazaH :: Marcadas -> Pinta -> Mazo -> Baza -> Int -> Carta    --Mirar
tirarCartaBazaH manoM pinta descartes baza ronda
    |not (esSuperada mano pinta baza) = head $ ordenarProb manoF pinta descartes ronda
    |hayCartaMejor mano cartaC && length cartasEnRango > 0 = head $ ordenarProb cartasEnRango pinta descartes ronda
    |otherwise = last $ ordenarProb manoF pinta descartes ronda
    where mano = desmarcarMano manoM
          manoF = filtrarMano mano baza pinta
          cartaS = head baza
          cartaC = cartaMasAlta baza (paloC cartaS) 
          cartasEnRango = filter (esProbEntre 0 0.2 manoF pinta descartes ronda) mano
          
tirarUnica :: Marcadas -> Palo -> Carta
tirarUnica [] _ = Carta Basto Cuatro
tirarUnica (m:manoM) palo
    |snd m && paloC (fst m) == palo = fst m
    |otherwise = tirarUnica manoM palo

tirarUnicaNT :: Marcadas -> Pinta -> Mazo -> Int -> Carta
tirarUnicaNT manoM pinta descartes ronda = tirarUnica manoM paloU
    where mano = desmarcarMano manoM
          f p = cartasRestantesPalo mano descartes pinta ronda p == 0
          paloU = head $ filter f ([Oro,Copa,Espada,Basto] \\ [paloC pinta])

ordenarProb :: Mano -> Pinta -> Mazo -> Int -> Mano   --Ordena el mazo en funcion de la probabilidad a priori, de mayor a menor.
ordenarProb [] _ _ _ = []
ordenarProb (cartaM:ms) pinta descartes ronda = mayores ++ [cartaM] ++ menores
    where mayores = ordenarProb [carta | carta <- ms, compararCartaProb carta cartaM (cartaM:ms) pinta descartes ronda] pinta descartes ronda
          menores = ordenarProb [carta | carta <- ms, not (compararCartaProb carta cartaM (cartaM:ms) pinta descartes ronda)] pinta descartes ronda

compararCartaProb :: Carta -> Carta -> Mano -> Pinta -> Mazo -> Int -> Bool
compararCartaProb carta1 carta2 mano pinta descartes ronda = prob1 >= prob2
    where prob1 = probAPriori carta1 mano pinta descartes ronda
          prob2 = probAPriori carta2 mano pinta descartes ronda

marcarManoInic :: Mano -> Pinta -> Int -> Int -> Marcadas
marcarManoInic mano pinta pedidas ronda = zip ordenadas marcadas
    where ordenadas = ordenarManoInic mano pinta ronda
          marcadas = (replicate pedidas True) ++ (replicate (ronda - pedidas) False)

ordenarManoInic :: Mano -> Pinta -> Int -> Mano
ordenarManoInic mano pinta ronda = altas ++ ases ++ resto
    where pedidasT = pedirTriunfos mano pinta ronda
          altas = take pedidasT (ordenarProb (filtrarPalo mano (paloC pinta)) pinta [] ronda)
          ases = filtrarValor (mano \\ altas) As
          resto = mano \\ (altas ++ ases)

esMarcada :: Marcadas -> Carta -> Bool
esMarcada [] _ = False
esMarcada (m:manoM) carta
    |fst m == carta = snd m
    |otherwise = esMarcada manoM carta

desmarcarMano :: Marcadas -> Mano
desmarcarMano marcadas = map fst marcadas

hayPaloMarcado :: Marcadas -> Palo -> Bool
hayPaloMarcado [] _ = False
hayPaloMarcado (m:manoM) palo
    |snd m && esPalo palo (fst m) = True
    |otherwise = hayPaloMarcado manoM palo
    
filtrarMarcadas :: Marcadas -> Mano
filtrarMarcadas marcadas = filter (esMarcada marcadas) (desmarcarMano marcadas)

actualizarMarcadas :: Marcadas -> Pinta -> Mazo -> Int -> Int -> Marcadas
actualizarMarcadas manoM pinta descartes ronda cambioMarca
    |manoM == [] = []
    |cambioMarca == -1 && length marcadas /= length manoM = marcarUna manoM pinta descartes ronda
    |cambioMarca == 1 && length marcadas /= 0 = desmarcarUna manoM pinta descartes ronda
    |otherwise = manoM
    where marcadas = filtrarMarcadas manoM

marcarUna :: Marcadas -> Pinta -> Mazo -> Int -> Marcadas
marcarUna manoM pinta descartes ronda = cambiarMarca manoM cartaM
    where mano = desmarcarMano manoM
          manoNM = mano \\ (filtrarMarcadas manoM)
          cartaM = head $ ordenarProb manoNM pinta descartes ronda

desmarcarUna :: Marcadas -> Pinta -> Mazo -> Int -> Marcadas
desmarcarUna manoM pinta descartes ronda = cambiarMarca manoM cartaM
    where mano = desmarcarMano manoM
          manoMar = filtrarMarcadas manoM
          cartaM = last $ ordenarProb manoMar pinta descartes ronda

cambiarMarca :: Marcadas -> Carta -> Marcadas
cambiarMarca [] _ = []
cambiarMarca (m:manoM) cartaM 
    |fst m == cartaM = (fst m,not (snd m)):manoM
    |otherwise = m:cambiarMarca manoM cartaM

esSuperada :: Mano -> Pinta -> Baza -> Bool
esSuperada mano pinta baza 
    |noHayPalo baza (paloC pinta) && hayCartaMejor mano cartaC = True
    |noHayPalo mano (paloC cartaS) && noHayPalo baza (paloC pinta) && hayPalo mano (paloC pinta) = True
    |noHayPalo mano (paloC cartaS) && hayPalo baza (paloC pinta) && hayCartaMejor mano cartaA = True
    |otherwise = False
    where cartaS = head baza
          cartaC = cartaMasAlta baza (paloC cartaS) 
          cartaA = cartaMasAlta baza (paloC pinta)

filtrarManoMarcada :: Marcadas -> Baza -> Pinta -> Marcadas
filtrarManoMarcada [] _ _ = []
filtrarManoMarcada (m:manoM) baza pinta 
    |snd m && perteneceAMano (fst m) manoF = m:filtrarManoMarcada manoM baza pinta 
    |otherwise = filtrarManoMarcada manoM baza pinta 
    where mano = desmarcarMano manoM
          manoF = filtrarMano mano baza pinta