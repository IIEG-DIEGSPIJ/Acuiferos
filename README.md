# Acuiferos
Concesionarios de aguas subterráneas de Jalisco (CONASJAL)

La carpeta incluye el proyecto de R-shiny completo.

En la carpeta "/datos" se encuentran un csv y un archivo RDS con la información recopilada de Datos abiertos de la Comisión Nacional del Agua (CONAGUA), “Concesiones, asignaciones, permisos otorgados y registros de obras situadas en zonas de libre alumbramiento de CONAGUA” y el “ACUERDO por el que se actualiza la disponibilidad media anual de agua subterránea de los 653 acuíferos de los Estados Unidos Mexicanos, mismos que forman parte de las regiones hidrológico-administrativas que se indican”, Diario Oficial de la Federación, jueves 17 de septiembre de 2020.
También se encuentran los archivos shape (geográficos) de límites municipales, el límite estatal y de acuíferos del estado de Jalisco. 

En la carpeta "/www" se encuentra un ".zip", que es la información que puede descargar el usuario desde la plataforma, así como los archivos para las fuentes(*.otf) y el archivo "style.css", que ayuda a dar el formato a la aplicación.


Si no funciona correctamente, puede ser porque no se han instalado las librerías necesarias.
Para instalar todas las librerías necesarias en R, copie y pegue el siguiente código en la consola de R y corra el código.

lib <- c("shinyjs","shiny","tidyr","leaflet","dplyr",
  "stringr","scales","sf","sp","plotly","ggplot2",
  "kableExtra","DT","shinyWidgets","RColorBrewer")
lapply(lib, install.packages, character.only = TRUE)
