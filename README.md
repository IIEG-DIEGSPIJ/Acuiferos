# Acuiferos
Concesionarios de aguas subterráneas de Jalisco (CONASJAL)

La carpeta incluye el proyecto de R-shiny completo.

Si no funciona correctamente, puede ser porque no se han instalado las librerías necesarias.
Para instalar todas las librerías necesarias en R, copie y pegue el siguiente código en la consola de R y corra el código.

lib <- c("shinyjs","shiny","tidyr","leaflet","dplyr",
  "stringr","scales","sf","sp","plotly","ggplot2",
  "kableExtra","DT","shinyWidgets","RColorBrewer")
lapply(lib, install.packages, character.only = TRUE)
