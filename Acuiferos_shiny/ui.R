library(shinyWidgets)
library(shinyjs)
library(shiny)
library(tidyr)
library(leaflet)
library(dplyr)
library(stringr)
library(scales)
library(sf)
library(sp)
library(plotly)
library(ggplot2)
library(kableExtra)
library(shinybusy)
library(DT)

# Datos para la selección
pozos <- readRDS("datos/pozos_jalisco.rds") %>% 
  filter(Acuifero_homo!="EL LLANO")

#  Listas utilizadas
Lista.despliegue <- list("Concesionario" = 1,
                         "Acuífero" = 2,
                         "Municipio" = 3)
Lista.selección <- list("Aprovechamiento anual del pozo en m3" = 2,
                        "Volumen de extracción anual de aprovechamientos subterraneos en m3" = 3)
Lista.pp <- list("Total" = 1,
                 "Pública" = 2,
                 "Privada" = 3)
Lista.cuen_con <- list("Acuífero" = 1,
                       "Concesionario" = 2)
Lista.aqua <- vector("list", length(unique(pozos$Acuifero_homo))+1)
for (i in 1:length(unique(pozos$Acuifero_homo))) {
  names(Lista.aqua)[i] <- unique(pozos$Acuifero_homo)[i]
  Lista.aqua[[i]] <- unique(pozos$Clave_acuifero_homo)[i]
}
names(Lista.aqua)[length(unique(pozos$Acuifero_homo))+1] <- "Todos"
Lista.aqua[[length(unique(pozos$Acuifero_homo))+1]] <- 1
# UI para ver lo de los pozos de agua
shinyUI(
  tagList(
    tags$style(type="text/css", ".shiny-server-account { z-index: 1000; }"),
    includeCSS("./www/style.css"),
    
    fluidPage(
      add_busy_spinner(onstart = F, spin = "fading-circle", color = "#E34F70"),
      
      # Application title
      div(class = "navbar1", 
          navbarPage(title = "",
                     header = 
                       busy_start_up(
                         loader = spin_epic("swapping-squares", color = "#FBBB27"),
                         text = "Cargando... Puede tomar unos minutos",
                         timeout = 1500,
                         color = "#FFBE52",
                         background = " #393D3F"
                       ),
                     
                     # Mapa de los pozos
                     tabPanel("Mapa de pozos",
                              # Barra de filtros
                              fluidRow(id="filtros",
                                       fluidRow(HTML("</br>")),
                                       column(3,
                                              radioButtons("Desglose", "Seleccione el nivel de desagregación deseado",
                                                           choices = Lista.despliegue, selected = "1")
                                       ),
                                       column(3, 
                                              actionBttn(
                                                inputId = "info1_desglose",
                                                icon = icon("fas fa-info"),
                                                size = "xs",
                                                color = "default",
                                                style = "material-circle"
                                              )
                                       ),
                                       column(4,
                                              conditionalPanel(condition = "input.Desglose == '1'",
                                                               fluidRow(radioButtons("Pu_pri", 
                                                                                     "Seleccione el tipo de concesión", 
                                                                                     choices = Lista.pp, selected = 1)),
                                                               fluidRow(
                                                                 column(6,
                                                                        #Lista de concesionarios filtrada
                                                                        conditionalPanel(condition = "input.Pu_pri == '1'",
                                                                                         selectInput("Concesionario_1",
                                                                                                     "Seleccione el concesionario que desea consultar",
                                                                                                     choices = c("Todos",sort(unique(pozos$Titular))),
                                                                                                     selected = 1)),
                                                                        conditionalPanel(condition = "input.Pu_pri == '2'",
                                                                                         selectInput("Concesionario_2",
                                                                                                     "Seleccione el concesionario que desea consultar",
                                                                                                     choices = c("Todos",sort(unique(pozos$Titular[pozos$Tipo_Concesionario=="PUBLICO"]))),
                                                                                                     selected = 1)),
                                                                        conditionalPanel(condition = "input.Pu_pri == '3'",
                                                                                         selectInput("Concesionario_3",
                                                                                                     "Seleccione el concesionario que desea consultar",
                                                                                                     choices = c("Todos",sort(unique(pozos$Titular[pozos$Tipo_Concesionario=="PRIVADO"]))),
                                                                                                     selected = 1))
                                                                 ),
                                                                 column(1, 
                                                                        actionBttn(
                                                                          inputId = "info1_c",
                                                                          icon = icon("fas fa-info"),
                                                                          size = "xs",
                                                                          color = "default",
                                                                          style = "material-circle"
                                                                        )
                                                                 )
                                                               )
                                              )
                                       ),
                                       # Este es el botón del glosario, lo demás siguen siendo filtros
                                       column(2, 
                                              actionBttn(
                                                inputId = "info1",
                                                label = "Notas", 
                                                color = "default"
                                              )
                                       )
                              ),
                              #El espacio del mapa con su título
                              fluidRow(HTML("</br>")),
                              fluidRow(column(1),
                                       column(10,
                                              id="titulo_imagen",
                                              fluidRow(id="titulo_mapa",
                                                       textOutput("Titulo_mapa")),
                                              fluidRow(HTML("</br>")),
                                              fluidRow(id="mapa_leaflet",
                                                       leafletOutput(outputId = "map", height = 500))
                                       ),
                                       column(1)
                              ),
                              fluidRow(HTML("</br>")),
                              fluidRow(tags$div(id="Fuente",
                                                tags$h5(
                                                  tags$p(
                                                    tags$b("Fuente: "),
                                                    "elaborado por el IIEG, con base a Datos abiertos de la Comisión Nacional del Agua (CONAGUA), “Concesiones, asignaciones, permisos otorgados y registros de obras situadas en zonas de libre alumbramiento de CONAGUA”  y el “ACUERDO por el que se actualiza la disponibilidad media anual de agua subterránea de los 653 acuíferos de los Estados Unidos Mexicanos, mismos que forman parte de las regiones hidrológico-administrativas que se indican”, Diario Oficial de la Federación, jueves 17 de septiembre de 2020.")
                                                )
                              )
                              )
                     ),
                     # Gráfica de consumo acumulado
                     tabPanel("Consumo por acuífero",
                              fluidRow(id="filtros",
                                       fluidRow(HTML("</br>")),
                                       column(3, offset = .5,
                                              radioButtons("Publico_privado", "Seleccione el tipo de concesión", 
                                                           choices = Lista.pp, selected = 1)),
                                       column(4, offset = .5,
                                              pickerInput(inputId = "Uso_acumulado", 
                                                          label = "Seleccione los usos que le interesen", 
                                                          choices = sort(unique(pozos$Uso_ampara_titulo)),
                                                          options = list(`actions-box` = TRUE,
                                                                         `deselect-all-text` = "Quitar selección",
                                                                         `select-all-text` = "Seleccionar todo",
                                                                         `none-selected-text` = "Ninguno"), 
                                                          multiple = TRUE, 
                                                          selected = sort(unique(pozos$Uso_ampara_titulo))
                                              )
                                       ),
                                       column(4, offset = .5,
                                              selectInput("Cuenca", "Seleccione el acuífero que desea consultar", 
                                                          choices = Lista.aqua[1:59], selected = "1413")
                                       ),
                                       column(1, 
                                              actionBttn(
                                                inputId = "info2",
                                                icon = icon("fas fa-info"),
                                                size = "xs",
                                                color = "default",
                                                style = "material-circle"
                                              )
                                       )
                              ),
                              fluidRow(HTML("</br>")),
                              fluidRow(id="titulo_imagen",
                                       column(6, offset = .5,
                                              textOutput("Titulo_maptab2")),
                                       column(6, offset = .5,
                                              textOutput("Titulo_grphtab2"))),
                              fluidRow(HTML("</br>")),
                              fluidRow(id="imagen_tab2",
                                       column(6, offset = .5,
                                              leafletOutput("map2")),
                                       column(6, offset = .5,
                                              plotlyOutput("Consumo"))),
                              fluidRow(HTML("</br>")),
                              fluidRow(
                                tags$div(id="Fuente",
                                         tags$h5(
                                           tags$p(
                                             tags$b("Fuente: "),
                                             "elaborado por el IIEG, con base a Datos abiertos de la Comisión Nacional del Agua (CONAGUA), “Concesiones, asignaciones, permisos otorgados y registros de obras situadas en zonas de libre alumbramiento de CONAGUA”  y el “ACUERDO por el que se actualiza la disponibilidad media anual de agua subterránea de los 653 acuíferos de los Estados Unidos Mexicanos, mismos que forman parte de las regiones hidrológico-administrativas que se indican”, Diario Oficial de la Federación, jueves 17 de septiembre de 2020."),
                                           tags$p(
                                             tags$b("*"),
                                             "La recarga media anual disponible se refiere a la 'recarga total media anual' menos 'descarga natural comprometida' (ver glosario).")
                                         )
                                )
                              )
                     ),
                     # Top concesionarios acaparadores capitalistas
                     tabPanel("Información de concesionarios",
                              fluidRow(id="filtros",
                                       fluidRow(HTML("</br>")),
                                       column(5, offset = .5,
                                              radioButtons("Con_Cuen", "¿Qué información desea consultar?", 
                                                           choices = Lista.cuen_con, selected = 1)),
                                       column(4, offset = .5,
                                              conditionalPanel(condition = "input.Con_Cuen == '1'",
                                                               selectInput("Cuenca2", 
                                                                           "Seleccione el acuífero que desea consultar",
                                                                           choices = Lista.aqua,
                                                                           selected = 1,
                                                                           multiple = T)
                                              ),
                                              conditionalPanel(condition = "input.Con_Cuen == '2'",
                                                               column(10,
                                                                      selectInput("Consen", "Seleccione el concesionario que desea consultar",
                                                                                  choices = c("Todos",sort(unique(pozos$Titular))),
                                                                                  selected = "Todos",
                                                                                  multiple = T)
                                                               ),
                                                               column(1,
                                                                      fluidRow(actionBttn(
                                                                        inputId = "info3_c",
                                                                        icon = icon("fas fa-info"),
                                                                        size = "xs",
                                                                        color = "default",
                                                                        style = "material-circle"
                                                                      )
                                                                      )
                                                               ),
                                                               column(1)
                                              )
                                       ),
                                       column(3,
                                              fluidRow(
                                                column(4),
                                                column(4,
                                                       actionBttn(
                                                         inputId = "info31",
                                                         icon = icon("far fa-question"),
                                                         size = "xs",
                                                         color = "default",
                                                         style = "material-circle"
                                                       )
                                                ),
                                                column(4)
                                              ),
                                              fluidRow(HTML("</br>")),
                                              fluidRow(
                                                useShinyjs(),
                                                tags$div(class = "filters_cols",
                                                         actionButton("filters_aply",
                                                                      "Aplicar filtros")
                                                )
                                              )
                                       )
                              ),
                              fluidRow(HTML("</br>")),
                              fluidRow(id="descarga",
                                       column(6,
                                              column(1,
                                                     conditionalPanel(condition = "input.Con_Cuen == '1'",
                                                                      downloadButton("Descarga_acui", "Descarga la siguiente tabla")
                                                     ),
                                                     conditionalPanel(condition = "input.Con_Cuen == '2'",
                                                                      downloadButton("Descarga_cons", "Descarga la siguiente tabla")
                                                     )
                                              ),
                                              column(11)),
                                       column(6)
                              ),
                              fluidRow(HTML("</br>")),
                              fluidRow(id="accion3",
                                       conditionalPanel(condition = "input.Con_Cuen == '1'",
                                                        fluidRow(column(6, offset = .5,
                                                                        fluidRow(id="titulo_imagen",
                                                                                 textOutput("Titulo_cuenca")),
                                                                        fluidRow(HTML("</br>")),
                                                                        fluidRow(tableOutput("tabla_cuenca"))
                                                        ),
                                                        column(6, offset = .5,
                                                               fluidRow(id="titulo_imagen",
                                                                        textOutput("Titulo_cuenca2")),
                                                               fluidRow(HTML("</br>")),
                                                               fluidRow(plotlyOutput("Consumo_cuenca"))
                                                        ))
                                       ),
                                       conditionalPanel(condition = "input.Con_Cuen == '2'",
                                                        fluidRow(column(6, offset = .5,
                                                                        fluidRow(id="titulo_imagen",
                                                                                 textOutput("Titulo_cons")),
                                                                        fluidRow(HTML("</br>")),
                                                                        fluidRow(tableOutput("tabla_cons"))
                                                        ),
                                                        column(6, offset = .5,
                                                               fluidRow(id="titulo_imagen",
                                                                        textOutput("Titulo_cons2")),
                                                               fluidRow(HTML("</br>")),
                                                               fluidRow(plotlyOutput("Consumo_cons"))
                                                        ))
                                       )
                              ),
                              fluidRow(HTML("</br>")),
                              fluidRow(
                                tags$div(id="Fuente",
                                         tags$h5(
                                           tags$p(
                                             tags$b("Fuente: "),
                                             "elaborado por el IIEG, con base en Datos abiertos de la Comisión Nacional del Agua (CONAGUA), Concesiones, asignaciones, permisos otorgados y registros de obras situadas en zonas de libre alumbramiento de CONAGUA.")
                                         )
                                )
                              )
                     ),
                     tabPanel("Lista de concesionarios",
                              id="dt_amiga",
                              # fluidRow(id="descarga",
                              #          column(1,
                              #                 downloadButton("Descarga_direc", "Descarga el directorio")),
                              #          column(11)
                              # ),
                              fluidRow(id="descarga",
                                       column(1,
                                              downloadButton("Descarga_direc", "Descarga el directorio")),
                                       column(5),
                                       column(1,
                                              downloadButton("Descarga_data", "Descarga la base de datos")),
                                       column(5)
                              ),
                              fluidRow(HTML("</br>")),
                              fluidRow(dataTableOutput("Tabla_directorio"))),
                     tabPanel("Glosario",
                              id="glosario",
                              htmlOutput("texto_glosario"))
          )
      )
    )
  )
)
