# Pocha-en-Haskell

# Juego de la Pocha con Haskell

1. Descripción
2. Instalación
3. Uso
4. Objetivos
5. Contacto

# Descripción

Esta práctica consiste en la implementación de "La Pocha" en el lenguaje de programación Haskell.
La Pocha es un juego de cartas, para el cual se emplea la baraja española de 40 cartas.
El número de jugadores puede ser igual a tres, cuatro o cinco. En este trabajo hemos considerado cuatro jugadores,
de modo que 1 jugador (el usuario) juega contra los otros 3 (Inteligencia Artificial).

# Instalación

El usuario que quiera jugar a La Pocha deberá descargar los siguientes archivos:

- bienvenidaPocha.txt
- guardado.txt
- instrucciones.txt
- Pocha.hs

Los tres primeros archivos son ficheros necesarios para la ejecución del juego en sí, el cual se encuentra
programado en el archivo .hs.

Para comenzar a jugar, el usuario deberá ejecutar el archivo .hs en la terminal de su ordenador. 
Se precisa la previa instalación de GHCI. Si el usuario no tiene dicho intérprete de Haskell, puede instalarlo
siguiendo las instrucciones en esta página web: https://www.haskell.org/downloads/.

En la implementación del juego se ha precisado la instalación de librerías adicionales. El usuario deberá instalar
la librería random. Para ello, simplemente ejecute la instrucción siguiente en la PowerShell: cabal install --lib random

Deberá obtener la ruta del archivo Pocha.hs, del estilo: C:\Users\Usuario\Downloads\PracticaPocha.
Una vez el usuario se encuentre en dicho directorio, deberá ejecutar la siguiente instrucción en su
terminal/PowerShell: ghci Pocha.hs

# Uso

Para comenzar a jugar, el usuario solo deberá escribir "jugar" (sin comillas), y ejecutar dicha instrucción.
Se desplegará un mensaje de bienvenida, y a continuación un menú principal con 4 distintas opciones:
- Instrucciones de La Pocha
- Reanudar la partida anterior
- Empezar una nueva partida, ó
- Salir del juego.

Para acceder a las distintas opciones, el usuario deberá escribir el índice que desee.

Elegida la opción tres (3), el usuario deberá escribir su nombre. Inmediatamente después, deberá elegir el nivel de
dificultad para la IA. Precisado el nivel, comenzará automáticamente la partida (RONDA 1).

Terminada la ronda, se mostrará por pantalla otro menú, que permitirá al usuario bien:
- Continuar la partida
- Guardar la partida
- Consultar las puntuaciones de los jugadores, o 
- Salir de la partida.

Esta implementación permite al usuario libremente abandonar La Pocha sin necesidad de completar todas las rondas.

Se recomienda al usuario tenga completamente desplegada la pestaña de la terminal/PowerShell para una correcta 
visualización del desarrollo del juego.

Asimismo, se advierte al usuario de un delay intencional durante todo el juego (threadDelay n). La motivación tras su
uso es brindar al jugador una experiencia más agradable, para que este pueda ir analizando en un tiempo razonable la
información que se muestra por pantalla.

De igual forma, la distinción entre el Nivel Fácil y Nivel Difícil está pensada para enriquecer la experiencia del 
jugador.

# Objetivos

Los objetivos de esta práctica (para los programadores) han sido los siguientes:

- Obtener experiencia con un lenguaje de programación funcional puro
- Lograr el nivel de abstracción necesario para implementar un juego de cartas en el ordenador
- Diseñar una Inteligencia Artificial (IA) capaz de competir contra un humano (el usuario), y
- Cooperar y trabajar en grupo

Asimismo, se espera que el usuario pueda:

- Mejorar su estrategia jugando a La Pocha
- Divertirse con este juego popular

# Contacto

Para cualquier consulta, duda o sugerencia, puede contactarnos a alejmele@ucm.es, pablji04@ucm.es y claudi14@ucm.es
¡Muchas gracias! Esperemos que su experiencia sea muy positiva.
