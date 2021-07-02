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
library(DT)
library(shinyWidgets)
library(RColorBrewer)

pozos <- readRDS("datos/pozos_jalisco.rds")
Municipios <- st_read("datos/Limite_Mun.shp") %>% 
    drop_na(CLAVE)
Acuifero <- st_read("datos/acuifeo.shp", options = "ENCODING=utf-8") %>% 
    rename(CONDICION = CONDICIÃ.) %>% 
    mutate(CONDICION = replace(CONDICION, str_detect(CONDICION, "FICIT"), "DÉFICIT")) %>% 
    mutate(NOM_ACUI = replace(NOM_ACUI, str_detect(NOM_ACUI, "SAN MARTIN DE BOLA"), "SAN MARTIN DE BOLAÑOS"))


# Para mapear los pozos, es necesario quitar los puntos que no están dentro del polígono del estado
pozos_mapita <- pozos %>%
    filter(Mapea == 1)

# Unir la base de datos con el shape de municipios
db_mun <- pozos %>% 
    group_by(CLAVE, Municipio) %>% 
    summarise(Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
              Vol_extrac_subterraneo = sum(Vol_extrac_subterraneo),
              Aprov_subterraneo_titulo = sum(Aprov_subterraneo_titulo),
              Titlulares = length(unique(Titular)))
db_mun <- sp::merge(Municipios, db_mun, by="CLAVE")

# Unir la base de datos con el shape de acuíferos
aqua <- pozos %>%
    rename(CLV_ACUI = Clave_acuifero_homo) %>% 
    group_by(CLV_ACUI, Acuifero_homo) %>% 
    summarise(Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
              Vol_extrac_subterraneo = sum(Vol_extrac_subterraneo),
              Aprov_subterraneo_titulo = sum(Aprov_subterraneo_titulo),
              Disponibilidad_subsuelo = max(Disponibilidad_subsuelo),
              Titlulares = length(unique(Titular)))
m_aqua1 <- sp::merge(Acuifero, aqua, by="CLV_ACUI")
aqua1 <- pozos %>%
    rename(CLV_ACUI = Clave_acuifero_homo) %>%
    group_by(CLV_ACUI, Acuifero_homo, Tipo_Concesionario) %>%
    summarise(Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
              Vol_extrac_subterraneo = sum(Vol_extrac_subterraneo),
              Aprov_subterraneo_titulo = sum(Aprov_subterraneo_titulo),
              Disponibilidad_subsuelo = max(Disponibilidad_subsuelo),
              Titlulares = length(unique(Titular)))
# aqua1 <- sp::merge(Acuifero, aqua1, by="CLV_ACUI") 
aqua_x <- pozos %>%
    rename(CLV_ACUI = Clave_acuifero_homo) %>% 
    group_by(CLV_ACUI, Acuifero_homo, Uso_ampara_titulo, Tipo_Concesionario) %>% 
    summarise(Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
              Vol_extrac_subterraneo = sum(Vol_extrac_subterraneo),
              Aprov_subterraneo_titulo = sum(Aprov_subterraneo_titulo),
              Disponibilidad_subsuelo = max(Disponibilidad_subsuelo))

# Definimos el server
shinyServer(function(input, output, session) {
    
    #Título de la plataforma
    output$Titulo_mapa <- renderText({paste0("Agua subterránea en Jalisco")})
    
    # Mapa base de la primera pestaña
    output$map <- renderLeaflet({
        leaflet() %>%
            fitBounds(lng1 = max(pozos_mapita$Longitud),
                      lng2 = min(pozos_mapita$Longitud),
                      lat1 = max(pozos_mapita$Latitud),
                      lat2 = min(pozos_mapita$Latitud)) %>%
            addProviderTiles(providers$CartoDB.Positron)
    })
    
    # Botones
    observeEvent(input$info1, {
        showModal(modalDialog(
            title = HTML("<p><b>Información de la georreferenciación</p></b>"),
            HTML("<p>Los Títulos concesionados/asignados son los que están en el Registro Público de Derechos de Agua (REPDA) al 31 de diciembre de 2020. </br>
            </br>En el Diario Oficial de la Federación se publicó el día 17 de septiembre de 2020 la disponibilidad media anual de agua subterránea con fecha de corte del 20 de febrero de 2020; para la realización de esta plataforma, de este acuerdo se tomaron los valores de la recarga total media anual (R) y la descarga natural comprometida (DNC); respecto al volumen concesionado/asignado de aguas subterráneas (VCAS), se tomaron los valores actualizados en el REPDA al 31 de diciembre de 2020.</br>
            </br><b>Observación:</b> De los 20,918 concesionarios o asignatarios en Jalisco, por cuestiones de georreferenciación, en el mapa solo se muestran 20,704 concesiones (214 concesionarios no tienen sus pozos georreferenciados al interior de Jalisco y 311 concesionarios tienen al menos un pozo con una georreferenciación que no está en el interior de la entidad). </br>
            </br>De los 27,802 pozos que amparan los Títulos concesionados en Jalisco, por cuestiones de georreferenciación, en el mapa solo se muestran 27,229 pozos.</p>"),
            footer = tagList(
                modalButton("OK")
            )))
    })
    observeEvent(input$info1_c, {
        showModal(modalDialog(
            title = HTML("<p><b>Lista de concesionarios</b></p>"),
            HTML("<p>En el desplegado solo se muestran los primeros 1,000 concesionarios registrados en la plataforma. Para hacer una consulta diferente, borre la opción seleccionada y escriba el nombre del concesionario que desee consultar. La lista de <b>todos los concesionarios registrados en el REPDA al 31 de diciembre de 2020</b> puede consultarla en la pestaña: 'Lista de concesionarios'. (Solo se pude seleccionar un concesionario por consulta).</p>"),
            footer = tagList(
                modalButton("OK")
            )))
    })
    observeEvent(input$info1_desglose, {
        showModal(modalDialog(
            title = HTML("<p><b>En este apartado encontrará la siguiente desagregación:</p></b>"),
            HTML("<p><b>Concesionarios:</b> : Se muestran los concesionarios o asignados en el REPDA (Ver el apartado Notas), con la opción de seleccionar las concesiones de tipo privada o las concesiones o asignaciones de tipo pública. Al elegir un concesionario, se mostrarán en el mapa los pozos que amparan su(s) título(s). Al dar clic en un punto, se mostrará:</br>
            </br>Nombre del titular, aprovechamiento anual del pozo en m3, pozos que ampara el título, fecha de registro de la concesión y tipo de uso del pozo. </br>
            </br><b>Acuíferos:</b> Disponibilidad media anual de aguas subterráneas en m3 de cada uno de los 59 acuíferos de Jalisco (a nivel nacional existen 653 acuíferos). Si es positivo o negativo se define por el volumen de recarga total media anual, menos la descarga anual comprometida, menos el volumen de aguas subterráneas concesionados e inscritos en el REPDA (Ver el apartado Notas).</br>
            </br>Al dar clic el cursor en un punto, se mostrará información del acuífero, aprovechamiento anual de los pozos ubicados en el acuífero en m3, número de titulares o concesionarios cuyas concesiones se ubican en el acuífero de referencia, número de pozos concesionados que se ubican en el acuífero y disponibilidad media anual del agua del subsuelo.</br>
            </br><b>Municipios:</b> Total del volumen de extracción subterránea anual en m3 que ampara los Títulos concesionados o asignados en cada municipio.</br>
            </br>Al dar clic el cursor en un punto, se mostrará información del municipio, aprovechamiento anual de los pozos ubicados en el municipio en m3, número de titulares o concesionarios cuyas concesiones se ubican en el municipio de referencia, número de pozos concesionados que se ubican en el municipio.</p>"),
            footer = tagList(
                modalButton("Cerrar")
            )))
    })
    observeEvent(input$info2, {
        showModal(modalDialog(
            title = HTML("<p><b>Información de la georreferenciación</p></b>"),
            HTML("<p>Los Títulos concesionados/asignados son los que están en el Registro Público de Derechos de Agua (REPDA) al 31 de diciembre de 2020. </br>
            </br>En el Diario Oficial de la Federación se publicó el día 17 de septiembre de 2020 la disponibilidad media anual de agua subterránea con fecha de corte del 20 de febrero de 2020; para la realización de esta plataforma, de este acuerdo se tomaron los valores de la recarga total media anual (R) y la descarga natural comprometida (DNC). En este apartado encontrará la siguiente desagregación:</br>
            </br><b>Tipo de concesión:</b> Se muestran la opción de concesión de tipo privado y la concesión o asignación de tipo pública.</br>
            </br><b>Tipo de uso:</b> Muestra el tipo de uso que ampara cada concesión.</br>
            </br><b>Acuífero:</b> Se despliega el listado de los 59 acuíferos de Jalisco. (Solo se puede seleccionar un acuífero por consulta).</br>
            </br>Después de hacer la selección de las variables de su interés, en el mapa aparecerá el acuífero seleccionado; al posicionarse con el cursor, se mostrará la recarga media anual disponible*, Volumen concesionado/asignado de aguas subterráneas, Volumen concesionado/asignado de aguas subterráneas por el tipo de uso seleccionado y la disponibilidad media anual de agua del subsuelo.</br>
            </br>En la gráfica se puede observar una curva compuesta por puntos que representan la suma acumulada de la cantidad de agua que pueden extraer los concesionarios, así como una línea horizontal que constituye la recarga media anual disponible* del acuífero. Al posicionar el cursor sobre un punto de la curva, se muestra el nombre del titular, el uso que ampara el título, el volumen anual del pozo en m3, el porcentaje que representa con respecto a la descarga media anual disponible y el porcentaje acumulado de los concesionarios que muestra la gráfica.</p>"),
            footer = tagList(
                modalButton("OK")
            )))
    })
    observeEvent(input$info3_c, {
        showModal(modalDialog(
            title = HTML("<p><b>Lista de concesionarios</b></p>"),
            HTML("<p>En el desplegado solo se muestran los primeros 1,000 concesionarios registrados alfabéticamente en la plataforma. Para hacer una consulta diferente, borre la opción seleccionada y escriba el nombre del concesionario que desee consultar. La lista de <b>todos los concesionarios registrados en el REPDA al 31 de diciembre de 2020</b> puede consultarla en la pestaña: 'Lista de concesionarios'. (Solo se pude seleccionar un concesionario por consulta).</p>"),
            footer = tagList(
                modalButton("OK")
            )))
    })
        observeEvent(input$info31, {
        showModal(modalDialog(
            title = HTML("<p><b>Información</p></b>"),
            HTML("<p>En este apartado puede encontrar los concesionarios clasificados por el acuífero en donde se ubican sus pozos o por todos los concesionarios registrados en Jalisco:</br>
            </br><b>Acuífero:</b> Al seleccionar los acuíferos que se desean consultar, en la tabla se mostrarán los 50 principales concesionarios registrados en el acuífero de acuerdo con la suma del aprovechamiento anual en m3 de los pozos concesionados al titular en el acuífero y que tengan el mismo uso que ampare su(s) título(s).</br>
            </br><b>NOTA:</b> Es posible que el titular posea otras concesiones, pero que estén ubicadas en otro acuífero o que el uso que ampare el título sea diferente.</br>
            </br>En la gráfica se mostrarán los titulares que previamente se seleccionaron y al pasar el cursor en cada barra se mostrará información del titular, el aprovechamiento anual en m3 de los pozos concesionados, títulos ubicados en el acuífero, pozos concesionados y uso que amparan lo(s) titulo(s).</br>
            </br><b>Concesionario:</b> La consulta podrá ser de uno o más titulares, en la tabla se mostrarán las concesiones de acuerdo a la suma del aprovechamiento anual de los pozos en m3 concesionados al titular por acuífero y el uso que ampare el título.</br>
            </br>En la gráfica se mostrarán los titulares que previamente se seleccionaron y al pasar el cursor en cada barra se mostrará información del titular, el aprovechamiento anual en m3 de los pozos concesionados, acuífero y el uso que ampara lo(s) título(s).</p>"),
            footer = tagList(
                modalButton("OK")
            )))
    })
        delay(5, click("filters_aply"))
    
        # Rampa de color partida
        rc1 <- colorRampPalette(colors = c("#2b83ba", "#edf8b9"), space = "Lab")(max(m_aqua1$Disponibilidad_subsuelo)/1000000)
        rc2 <- colorRampPalette(colors = c("#edf8b9", "#d7191c"), space = "Lab")(abs(min(m_aqua1$Disponibilidad_subsuelo))/1000000)
        rampcols <- c(rc1, rc2)
        
    #Cambios de la primera pestaña
    observe({
        show_modal_spinner()
        
        ##Depende de la opción elegida por el usuario
        Capa_pozos <- input$Seleccion
        Capa_Desglose <- input$Desglose
        Capa_concesionario <- ifelse(input$Pu_pri==2, input$Concesionario_2,
                                     ifelse(input$Pu_pri==3, input$Concesionario_3,
                                            input$Concesionario_1))
        Pu_pri <- ifelse(input$Pu_pri==2, "PUBLICO",
                         ifelse(input$Pu_pri==3, "PRIVADO","TOTAL"))
        
        ## Para el mapa de los concesionarios
        # Primero el filtro de privado-público
        if (Pu_pri == "TOTAL") {
            db_consen <- pozos_mapita
        } else {db_consen <- pozos_mapita %>%
            filter(Tipo_Concesionario == Pu_pri)}
        #Ahora el filtro del concesionario
        if (Capa_concesionario=="Todos") {
            db_consen <- db_consen
        } else {db_consen <- db_consen %>%
            filter(Titular %in% Capa_concesionario)}
        
        ### Para hacer el mapa
        # Cuando quiere ver el total de pozos para cada concesionario
        if(Capa_Desglose==1) {
            leafletProxy("map", data = db_consen) %>%
                clearMarkers() %>%
                clearControls() %>%
                clearShapes() %>%
                addCircleMarkers(lng = db_consen$Longitud, lat = db_consen$Latitud,
                                 weight = 0, opacity = .5,
                                 radius = 7*(db_consen$Vol_anual_pozo_m3)/max(db_consen$Vol_anual_pozo_m3)+input$map_zoom/pi,
                                 fillColor = "blue",
                                 popup = paste0('<strong>Titular:</strong> ', db_consen$Titular, '<br/><hr>',
                                                '<strong>Aprovechamiento medio anual del pozo en m3:</strong> ', comma(db_consen$Vol_anual_pozo_m3, 1), '<br/>',
                                                '<strong>Pozos que ampara el título:</strong> ', db_consen$Aprov_subterraneo_titulo, '<br/>',
                                                '<strong>Fecha de registro de la concesión:</strong> ', db_consen$Fecha_registro, '<br/>',
                                                '<strong>Uso :</strong> ', db_consen$Uso_ampara_titulo , '<br/>'))
            }
        # Cuando quiere ver por acuífero
        if (Capa_Desglose==2) {
            paletita <- colorNumeric(palette = rampcols,
                                     domain = m_aqua1$Disponibilidad_subsuelo, 
                                     na.color = "black", reverse = T)
            leafletProxy("map", data = m_aqua1) %>%
                clearMarkers() %>%
                clearControls() %>%
                clearShapes() %>%
                addPolygons(weight = .5, color = "black", smoothFactor = 0.5,
                            fillOpacity = .48, fillColor = ~paletita(Disponibilidad_subsuelo),
                            popup = paste0('<strong>Acuífero:</strong> ', m_aqua1$NOM_ACUI, '<br/><hr>',
                                           '<strong>Aprovechamiento anual de los pozos ubicados en el acuífero en m3:</strong> ',
                                           comma(m_aqua1$Vol_anual_pozo_m3, 1), '<br/>',
                                           '<strong>Titulares:</strong> ', comma(m_aqua1$Titlulares,1), '<br/>',
                                           '<strong>Pozos:</strong> ', comma(m_aqua1$Aprov_subterraneo_titulo,1), '<br/>',
                                           '<strong>Disponibilidad media anual del agua de agua subterránea en m3:</strong> ', comma(m_aqua1$Disponibilidad_subsuelo,1), '<br/>')) %>%
                addLegend(position = "topleft",
                          pal = paletita, values = ~Disponibilidad_subsuelo,
                          title = "Disponibilidad</br>media anual de</br>aguas subterráneas en m3</br>")
        }
        # Cuando quiere ver por municipio
        if (Capa_Desglose==3) {
            paletita <- colorNumeric(c("#2b83ba", "#9dd3a7", "#edf8b9", "#f99e59", "#d7191c"),
                                     domain = db_mun$Vol_anual_pozo_m3, na.color = "black", reverse = F)
            leafletProxy("map", data = db_mun) %>%
                clearMarkers() %>%
                clearControls() %>%
                clearShapes() %>%
                addPolygons(weight = .5, color = "black", smoothFactor = 0.5,
                            fillOpacity = .68, fillColor = ~paletita(Vol_anual_pozo_m3),
                            popup = paste0('<strong>Municipio:</strong> ', db_mun$Municipio, '<br/><hr>',
                                           '<strong>Aprovechamiento anual de los pozos ubicados en el municipio en m3:</strong> ', comma(db_mun$Vol_anual_pozo_m3, 1), '<br/>',
                                           '<strong>Titulares:</strong> ', comma(db_mun$Titlulares,1), '<br/>',
                                           '<strong>Pozos:</strong> ', comma(db_mun$Aprov_subterraneo_titulo,1), '<br/>')) %>%
                addLegend(position = "topleft",
                          pal = paletita, values = ~Vol_anual_pozo_m3,
                          title = "Aprovechamiento </br>anual en m3")
        }
        remove_modal_spinner()
    })
    
    ##Cambios de la segunda pestaña
    observe({
        show_modal_spinner()
        
        ##Depende de la opción elegida por el usuario
        Capa_cuenca <- input$Cuenca
        Uso_acumulado <- input$Uso_acumulado
        pub_priv <- input$Publico_privado
        output$Titulo_maptab2 <- renderText({paste0("Acuífero de ",
                                                    aqua$Acuifero_homo[aqua$CLV_ACUI==Capa_cuenca])})
        output$Titulo_grphtab2 <- renderText({paste0("Pozos en el acuífero de ",
                                                     aqua$Acuifero_homo[aqua$CLV_ACUI==Capa_cuenca])})
        
        ## Para hacer el mapa de acuíferos
        # Necesito saber si quiere información de público, privado o ambos
        if (pub_priv == 1) {
            m_aqua <- aqua_x %>%
                filter(CLV_ACUI == Capa_cuenca)
        } else if (pub_priv == 2) {
            m_aqua <- aqua_x %>%
                filter(CLV_ACUI == Capa_cuenca,
                       Tipo_Concesionario == "PUBLICO")
        } else if (pub_priv == 3) {
            m_aqua <- aqua_x %>%
                filter(CLV_ACUI == Capa_cuenca,
                       Tipo_Concesionario == "PRIVADO")
        }
        # Filtro para hacer el mapa
        m_aqua <- m_aqua %>%
            filter(CLV_ACUI == Capa_cuenca)
        m_aqua <- sp::merge(Acuifero, m_aqua, by="CLV_ACUI")
        
        zoom <- st_bbox(m_aqua) %>%
            as.vector()
        
        ## Mapa base de la segunda pestaña
        output$map2 <- renderLeaflet({
            leaflet(m_aqua) %>%
                clearControls() %>%
                clearShapes() %>%
                fitBounds(zoom[1], zoom[2], zoom[3], zoom[4]) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(weight = .5, color = "black", smoothFactor = 0.5,
                            fillOpacity = .68,
                            fillColor = ifelse(m_aqua$Disponibilidad_subsuelo>0,"#93E7F8",
                                               "#94682A"),
                            popup = paste0('<b>Acuífero:</b> ', m_aqua$Acuifero_homo, '<br/><hr>',
                                           '<b>Recarga media anual disponible*:</b> ', comma(unique(cuencastico$disponible)), '<br/>',
                                           '<b>Volumen concesionado/asignado de aguas subterráneas total:</b> ', comma(sum(pozos$Vol_anual_pozo_m3[pozos$Clave_acuifero_homo==Capa_cuenca])), '<br/>',
                                           '<b>Volumen concesionado/asignado de aguas subterráneas por el tipo de uso seleccionado:</b> ', 
                                           ifelse(nrow(cuencastico1)==0,0,
                                                  comma(max(cuencastico1$Extra), 1)), '<br/>',
                                           '<b>Disponibilidad media anual de agua del subsuelo:</b> ', comma(unique(cuencastico$Disponibilidad_subsuelo))
                            ))
            
        })
        
        ### Filtro para hacer la gráfica
        # Necesito saber si quiere información de público, privado o ambos
        if (pub_priv == 1) {
            cuencastico <- pozos %>%
                filter(Clave_acuifero_homo == Capa_cuenca)
        } else if (pub_priv == 2) {
            cuencastico <- pozos %>%
                filter(Clave_acuifero_homo == Capa_cuenca,
                       Tipo_Concesionario == "PUBLICO")
        } else if (pub_priv == 3) {
            cuencastico <- pozos %>%
                filter(Clave_acuifero_homo == Capa_cuenca,
                       Tipo_Concesionario == "PRIVADO")
        }
        
        
        cuencastico1 <- cuencastico %>%
            filter(Uso_ampara_titulo %in% Uso_acumulado) 
        
        if (nrow(cuencastico1)==0) {
            graph <- ggplot() +
                geom_hline(linetype="dashed",
                           aes(yintercept = max(cuencastico$disponible),
                               text = paste("<b>Recarga media anual disponible*: </b>",
                                            comma(max(cuencastico$disponible),1),"m3")))    
        } else {
            cuencastico1 <- cuencastico1 %>%
                group_by(Titular) %>%
                summarise(Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
                          disponible = max(disponible)) %>%
                arrange(desc(Vol_anual_pozo_m3)) %>%
                mutate(Extra = cumsum(Vol_anual_pozo_m3),
                       Extractor = 1:n(),
                       Porcentaje_A = paste0(comma(Extra/disponible*100,.01),"%"),
                       Porcentaje = paste0(comma(Vol_anual_pozo_m3/disponible*100,.01),"%"))
            
            
            graph <- ggplot(cuencastico1, aes(x=Extractor,y=Extra)) +
                geom_line(colour='black', group = 1) +
                geom_point(colour='#FBBB27',
                           aes(text = paste('</br>','<b>Porcentaje acumulado:</b> ', Porcentaje_A,'</br>',
                                            '<b>Porcentaje usado:</b> ', Porcentaje,'</br>',
                                            '<b>Volumen anual en m3:</b> ', comma(Vol_anual_pozo_m3,1),'</br>',
                                            '<b>Uso que ampara el título:</b> ', m_aqua$Uso_ampara_titulo, '</br>',
                                            '<b>Titular:</b> ', Titular))) +
                geom_hline(linetype="dashed",
                           aes(yintercept = max(disponible),
                               text = paste("<b>Recarga media anual disponible*: </b>",
                                            comma(max(disponible),1),"m3")))    
        }
        
        
        
        # Se le da formato a las grafiquitas
        graph <- graph +
            ylab("Agua concesionada m3") +
            xlab("Titulares") +
            scale_y_continuous(limits = c(0, max(sum(cuencastico$Vol_anual_pozo_m3)*1.1,
                                                 max(cuencastico$disponible)*1.2)),
                               labels = comma) +
            theme(axis.text = element_text(size = 8, color = 'black'),
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.title = element_text(size = 13, color = 'black', face = 'bold'),
                  panel.grid.major.y  = element_line(colour = "grey", size = .001,
                                                     linetype = "dotted"),
                  panel.grid.minor.y  = element_blank(),
                  panel.grid.major.x = element_line(colour = "grey", size = .001,
                                                    linetype = "dotted"),
                  panel.grid.minor.x = element_blank(),
                  panel.background = element_rect(fill = 'ghostwhite'),
                  legend.key = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                  legend.background = element_rect(fill = 'ghostwhite'),
                  legend.title = element_text(size = 11,
                                              color = 'black',
                                              face = 'bold',
                                              hjust = .5),
                  legend.text = element_text(size = 9.5,
                                             color = 'black'),
                  plot.background = element_rect(fill = "ghostwhite"),
                  axis.line = element_line(colour = 'grey'),
                  axis.ticks = element_line(colour = 'grey'))
        
        output$Consumo <- renderPlotly({
            print(
                ggplotly(graph,
                         tooltip = c("text")
                )
            )
        })
        remove_modal_spinner()
    })
    
    ##Cambios de la tercera pestaña
    observeEvent(input$filters_aply,{
        show_modal_spinner()
        
        ##Depende de la opción elegida por el usuario
        Cuenca2 <- input$Cuenca2
        Consen <- input$Consen
        output$Titulo_cuenca <- renderText({paste("Información de los 50 concesionarios principales en el/los Acuífero(s) seleccionados")})
        output$Titulo_cuenca2 <- renderText({paste("Principales concesionarios")})
        output$Titulo_cons <- renderText({paste("Información de los pozos concesionados")})
        output$Titulo_cons2 <- renderText({paste("Principales pozos")})
        
        # Tabla de kable del top 50 concesionarios del acuífero
        if ("1" %in% Cuenca2 | is.null(Cuenca2)) {
            kable_top <- pozos %>%
                group_by(Titulo, Clave_acuifero_homo, Uso_ampara_titulo) %>%
                mutate(Pozos = max(Aprov_subterraneo_titulo)) %>%
                ungroup() %>%
                group_by(Titular, Acuifero_homo, Uso_ampara_titulo) %>%
                summarise(Títulos = length(unique(Titulo)),
                          Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
                          Pozos = n()) %>%
                ungroup() %>%
                arrange(desc(Vol_anual_pozo_m3)) %>%
                mutate(x_axis_n = 1:n())
        } else {
            kable_top <- pozos %>%
                group_by(Titulo, Clave_acuifero_homo, Uso_ampara_titulo) %>%
                mutate(Pozos = max(Aprov_subterraneo_titulo)) %>%
                filter(Clave_acuifero_homo %in% Cuenca2) %>%
                ungroup() %>%
                group_by(Titular, Acuifero_homo, Uso_ampara_titulo) %>%
                summarise(Títulos = length(unique(Titulo)),
                          Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
                          Pozos = n()) %>%
                ungroup() %>%
                arrange(desc(Vol_anual_pozo_m3)) %>%
                mutate(x_axis_n = 1:n())
        }
        kable_top <- kable_top[1:50,]
        # Output de la tabla
        output$tabla_cuenca <- function() {
            kable_top %>%
                mutate(Vol_anual_pozo_m3 = comma(Vol_anual_pozo_m3,1)) %>%
                rename(Acuífero = Acuifero_homo,
                       `Aprovechamiento anual en m3` = Vol_anual_pozo_m3,
                       `Uso que ampara el título` = Uso_ampara_titulo) %>%
                select(-x_axis_n) %>%
                knitr::kable("html") %>%
                kable_styling("striped", full_width = F, 
                              html_font = "\"Arial Regular\", arial, helvetica, sans-serif") %>%
                scroll_box(height = "450px")
        }
        # Botón de descarga - acuíferos
        output$Descarga_acui <- downloadHandler(
            filename = function() {
                paste("Tabla_a", ".csv", sep = "")
            },
            content = function(file) {
                write.csv(kable_top %>%
                              mutate(Vol_anual_pozo_m3 = comma(Vol_anual_pozo_m3,1)) %>%
                              rename(Acuífero = Acuifero_homo,
                                     `Aprovechamiento anual en m3` = Vol_anual_pozo_m3,
                                     `Uso que ampara el título` = Uso_ampara_titulo) %>%
                              select(-x_axis_n),
                          file, row.names = FALSE)
            }
        )
        # Output del plotly
        ### Hacer las gráficas
        graph2 <- ggplot(kable_top, aes(x=reorder(x_axis_n, -Vol_anual_pozo_m3),
                                        y=Vol_anual_pozo_m3,
                                        text = paste('</br>','<b>Titular:</b> ', Titular,'</br>',
                                                     '<b>Aprovechamiento anual en m3:</b> ', comma(Vol_anual_pozo_m3, 1),'</br>',
                                                     '<b>Títulos:</b> ', Títulos,
                                                     '</br>','<b>Pozos:</b> ', Pozos,'</br>',
                                                     '<b>Acuífero:</b> ', Acuifero_homo,'</br>',
                                                     '<b>Uso que ampara el título:</b> ', Uso_ampara_titulo)
        )) +
            geom_bar(fill ='#FBBB27',
                     stat = "identity") +
            ylab("Agua concesionada m3") +
            xlab("Titulares") +
            scale_y_continuous(labels = comma) +
            theme(axis.text = element_text(size = 8, color = 'black'),
                  axis.text.x = element_blank(),
                  axis.title = element_text(size = 13, color = 'black', face = 'bold'),
                  panel.grid.major.y  = element_line(colour = "grey", size = .001,
                                                     linetype = "dotted"),
                  panel.grid.minor.y  = element_blank(),
                  panel.grid.major.x = element_line(colour = "grey", size = .001,
                                                    linetype = "dotted"),
                  panel.grid.minor.x = element_blank(),
                  panel.background = element_rect(fill = 'ghostwhite'),
                  legend.key = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                  legend.background = element_rect(fill = 'ghostwhite'),
                  legend.title = element_text(size = 11,
                                              color = 'black',
                                              face = 'bold',
                                              hjust = .5),
                  legend.text = element_text(size = 9.5,
                                             color = 'black'),
                  plot.background = element_rect(fill = "ghostwhite"),
                  axis.line = element_line(colour = 'grey'),
                  axis.ticks = element_line(colour = 'grey'))
        # Plotly con la gráfica
        output$Consumo_cuenca <- renderPlotly({
            print(
                ggplotly(graph2,
                         tooltip = c("text")
                )
            )
        })
        # Tabla de pozos por concesionarios
        if ("Todos" %in% Consen | is.null(Consen)) {
            kable_cnsencionario <- pozos %>%
                group_by(Titular, Acuifero_homo, Uso_ampara_titulo) %>%
                summarise(Títulos = length(unique(Titulo)),
                          Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
                          Pozos = n()) %>%
                ungroup() %>%
                arrange(desc(Vol_anual_pozo_m3)) %>%
                mutate(Clave = 1:n())
        } else #(Consen != "Todos" & !is.null(Consen))
        {
            kable_cnsencionario <- pozos %>%
                filter(Titular %in% Consen) %>%
                group_by(Titular, Acuifero_homo, Uso_ampara_titulo) %>%
                summarise(Títulos = length(unique(Titulo)),
                          Vol_anual_pozo_m3 = sum(Vol_anual_pozo_m3),
                          Pozos = n()) %>%
                ungroup() %>%
                arrange(desc(Vol_anual_pozo_m3)) %>%
                mutate(Clave = 1:n())
        }
        kable_cnsencionario <- kable_cnsencionario[1:50,] %>%
            drop_na()
        # Output de la tabla
        output$tabla_cons <- function() {
            kable_cnsencionario %>%
                select(-Clave) %>%
                mutate_at(.vars = c("Vol_anual_pozo_m3"),
                          comma, 1) %>%
                rename(Acuífero = Acuifero_homo,
                       `Aprovechamiento anual de los pozos en m3` = Vol_anual_pozo_m3,
                       `Uso que ampara el título` = Uso_ampara_titulo) %>%
                knitr::kable("html") %>%
                kable_styling("striped", full_width = F, 
                              html_font = "\"Arial Regular\", arial, helvetica, sans-serif") %>%
                scroll_box(height = "450px")
        }
        # Botón de descarga - acuíferos
        output$Descarga_cons <- downloadHandler(
            filename = function() {
                paste("Tabla_c", ".csv", sep = "")
            },
            content = function(file) {
                write.csv(kable_cnsencionario %>%
                              select(-Clave) %>%
                              mutate_at(.vars = c("Vol_anual_pozo_m3"),
                                        comma, 1) %>%
                              rename(Acuífero = Acuifero_homo,
                                     `Aprovechamiento anual de los pozos en m3` = Vol_anual_pozo_m3,
                                     `Uso que ampara el título` = Uso_ampara_titulo),
                          file, row.names = FALSE)
            }
        )
        # Output del plotly
        ### Hacer las gráficas
        graph3 <- ggplot(kable_cnsencionario,
                         aes(x=reorder(Clave, -Vol_anual_pozo_m3),
                             y=Vol_anual_pozo_m3,
                             text = paste('</br>','<b>Titular:</b> ', Titular,'</br>',
                                          '<b>Aprovechamiento anual en m3:</b> ', comma(Vol_anual_pozo_m3, 1),'</br>',
                                          '<b>Acuífero:</b> ', Acuifero_homo,'</br>',
                                          '<b>Uso que ampara el título:</b> ', Uso_ampara_titulo),
                             fill = Titular
                         )) +
            geom_bar(fill ='#FBBB27',
                     stat = "identity") +
            ylab("Aprovechamiento anual en m3") +
            xlab("Titulares") +
            scale_y_continuous(labels = comma) +
            scale_y_continuous(labels = comma) +
            theme(axis.text = element_text(size = 8, color = 'black'),
                  axis.text.x = element_blank(),
                  axis.title = element_text(size = 13, color = 'black', face = 'bold'),
                  panel.grid.major.y  = element_line(colour = "grey", size = .001,
                                                     linetype = "dotted"),
                  panel.grid.minor.y  = element_blank(),
                  panel.grid.major.x = element_line(colour = "grey", size = .001,
                                                    linetype = "dotted"),
                  panel.grid.minor.x = element_blank(),
                  panel.background = element_rect(fill = 'ghostwhite'),
                  legend.key = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                  legend.background = element_rect(fill = 'ghostwhite'),
                  legend.title = element_text(size = 11,
                                              color = 'black',
                                              face = 'bold',
                                              hjust = .5),
                  legend.text = element_text(size = 9.5,
                                             color = 'black'),
                  plot.background = element_rect(fill = "ghostwhite"),
                  axis.line = element_line(colour = 'grey'),
                  axis.ticks = element_line(colour = 'grey'))
        # Plotly con la gráfica
        output$Consumo_cons <- renderPlotly({
            print(
                ggplotly(graph3,
                         tooltip = c("text")
                )
            )
        })
        remove_modal_spinner()
    })
    ## Tabla de consecionarios
    output$Tabla_directorio <- renderDataTable({
        datatable(as.data.frame(unique(select(pozos, c(Titular, Tipo_Concesionario)))),
                  colnames = c('Titular', 'Tipo de Concesionario'),
                  rownames = FALSE, filter = 'top',
                  # autoHideNavigation = T,
                  options = list(pageLength = 15, 
                                 autoWidth = TRUE,
                                 dom = 'tip',
                                 language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    })
    # Botón de descarga del directorio
    output$Descarga_direc <- downloadHandler(
        filename = function() {
            paste("Directorio", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(as.data.frame(unique(select(pozos, c(Titular, Tipo_Concesionario)))) %>% 
                          rename(`Tipo de Concesionario` = Tipo_Concesionario),
                      file, row.names = FALSE)
        }
    )
    # Botón de descarga de los datos
    output$Descarga_data <- downloadHandler(
        filename =  function() {
            paste0("datos_acuiferos.zip")
        },
        content = function(file) {
            file.copy("www/datos_acuiferos.zip", file)
        },
        contentType = "application/zip"
    )
    
    # Glosario
    output$texto_glosario <- renderUI({
        HTML("<p><b>Acuífero:</b> cualquier formación geológica por la que circulan o se almacenan aguas subterráneas que puedan ser extraídas para su explotación, uso o aprovechamiento. </br>
        </br><b>Asignación:</b> Título que otorga el Ejecutivo Federal, a través de 'la Comisión' o del Organismo de Cuenca que corresponda, conforme a sus respectivas competencias, para realizar la explotación, uso o aprovechamiento de las aguas nacionales, a los municipios, a los estados o a la Ciudad de México, destinadas a los servicios de agua con carácter público urbano o doméstico. </br>
             </br><b>Concesión:</b> Título que otorga el Ejecutivo Federal, a través de “la Comisión” o del Organismo de Cuenca que corresponda, conforme a sus respectivas competencias, para la explotación, uso o aprovechamiento de las aguas nacionales, y de sus bienes públicos inherentes, a las personas físicas o morales de carácter público y privado, excepto los títulos de asignación. </br>
             </br><b>Descarga Natural Comprometida (DNC):</b> Es el volumen que representa una fracción de la descarga natural de un acuífero, se determina sumando los volúmenes de agua concesionados de los manantiales y del caudal base de los ríos que están comprometidos como agua superficial, alimentados por un acuífero, más las descargas que se deben conservar para no afectar la alimentación de acuíferos adyacentes, sostener el uso ambiental y prevenir la inmigración de agua de mala calidad al acuífero considerado. </br>
             </br><b>Disponibilidad media anual de aguas del subsuelo:</b> En una unidad hidrogeológica -entendida ésta como el conjunto de estratos geológicos hidráulicamente conectados entre sí, cuyos límites laterales y verticales se definen convencionalmente para fines de evaluación, manejo y administración de las aguas nacionales subterráneas-, es el volumen medio anual de agua subterránea que, cuando es positivo, puede ser extraído de un acuífero para diversos usos, adicional a la extracción ya concesionada y a la descarga natural comprometida, sin poner en peligro el equilibrio de los ecosistemas. Cuando este valor es negativo indica un déficit se determina por medio de la siguiente expresión:</p></br>
             </br><p><center><i>DISPONIBILIDAD MEDIA ANUAL DE AGUA SUBTERRANEA EN UNA UNIDAD HIDROGEOLOGICA = RECARGA TOTAL MEDIA ANUAL (R) – DESCARGA NATURAL COMPROMETIDA (DNC) – VOLUMEN CONCESIONADO DE AGUA SUBTERRANEA (VEAS)</center></i></p></br>
             </br><p><b>Capacidad de Carga:</b> Estimación de la tolerancia de un ecosistema al uso de sus componentes, tal que no rebase su capacidad de recuperación en el corto plazo sin la aplicación de medidas de restauración o recuperación para restablecer el equilibrio ecológico. </br>
             </br><b>Extracción de aguas subterráneas:</b> Volumen de agua que se extrae artificialmente de un acuífero para los diversos usos</br>
             </br><b>Recarga total:</b> Volumen de agua que recibe un acuífero, en un intervalo de tiempo específico.</br>
             </br><b>Recarga Media Anual (R):</b> Es el volumen de agua que recibe un acuífero, en un intervalo de tiempo específico, se obtiene dividiendo la recarga total deducida del balance de aguas subterráneas, entre el número de años del intervalo de tiempo utilizado para plantearlo. </br>
             </br><b>Uso Acuacultura:</b> La aplicación de aguas nacionales para el cultivo, reproducción y desarrollo de cualquier especie de la fauna y flora acuáticas. </br>
             </br><b>Uso Agrícola:</b> La aplicación de agua nacional para el riego destinado a la producción agrícola y la preparación de ésta para la primera enajenación, siempre que los productos no hayan sido objeto de transformación industrial. </br>
             </br><b>Uso Doméstico:</b> La aplicación de agua nacional para el uso particular de las personas y del hogar, riego de sus jardines y de árboles de ornato, incluyendo el abrevadero de animales domésticos que no constituya una actividad lucrativa, en términos del Artículo 115 de la Constitución Política de los Estados Unidos Mexicanos. </br>
             </br><b>Uso industrial:</b> La aplicación de aguas nacionales en fábricas o empresas que realicen la extracción, conservación o transformación de materias primas o minerales, el acabado de productos o la elaboración de satisfactores, así como el agua que se utiliza en parques industriales, calderas, dispositivos para enfriamiento, lavado, baños y otros servicios dentro de la empresa, las salmueras que se utilizan para la extracción de cualquier tipo de sustancias y el agua aun en estado de vapor, que sea usada para la generación de energía eléctrica o para cualquier otro uso o aprovechamiento de transformación. </br>
             </br><b>Uso Pecuario:</b> La aplicación de aguas nacionales para la cría y engorda de ganado, aves de corral y otros animales, y su preparación para la primera enajenación siempre que no comprendan la transformación industrial; no incluye el riego de pastizales. </br>
             </br><b>Uso Público Urbano:</b> La aplicación de agua nacional para centros de población y asentamientos humanos, a través de la red municipal. </br>
             </br><b>Volumen de Extracción de Aguas Subterráneas (VEAS):</b> Se determina sumando los volúmenes anuales de agua asignados o concesionados por la Comisión mediante títulos inscritos en el Registro Público de Derechos de Agua (REPDA), los volúmenes de agua que se encuentren en proceso de registro y titulación y, en su caso, los volúmenes de agua correspondientes a reservas, reglamentos y programación hídrica, determinados para el acuífero de que se trate, todos ellos referidos a una fecha de corte específica.</p>"
        )
    })
    
})
