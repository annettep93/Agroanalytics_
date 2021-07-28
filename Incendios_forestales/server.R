
# Paquetes ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)


shinyServer(function(input, output){
  #INTRO
  
  
  output$definicion <- renderText({
    "<center>
    <h3>Incendios Forestales en Argentina</center> "
  })
  
  output$causas <- renderText({
    
    "<center> <dfn>Un incendio forestal es un fuego que se propaga libremente con efecto no deseado para la vegetación y sin estar sujeto al control humano.</dfn></center><hr>
    <justify>Las causas que lo generan pueden ser:<br> 
    Naturales <br>
    Antrópicas: intencionales o por negligencia.<hr></justify>"
  })
  
  output$prevencion <- renderText({
    
    "<center><h3>Prevención</h3></center>
    <hr>
    <ul>Evita fumar y arrojar fósforos, colillas o botellas rotas en el suelo ya que pueden ser un medio para generar fuego por la intensidad del sol.<br>
    No arrojes basura en el campo, especialmente botellas y trozos de vidrio, porque actúan como un incentivo a la producción del fuego.<br>
    Si realizás una fogata, colocá piedras alrededor del fuego y, luego de apagarlo con agua o tierra, asegurate de haberlo hecho correctamente removiendo las cenizas.<br>
    Evitá encender fuego en época de sequía.<br>
    Si estás en una ruta y ves un incendio forestal, avisá de inmediato a los bomberos. Recordá que una columna de humo significa la posibilidad de un incendio forestal.</ul><br>
    
    <h5>Si vivis en una zona con riesgo de incendios forestales:</h5><br>
    
    <ul>No apiles troncos ni ramas cerca de la casa.<br>
    Mantené el pasto corto y construí una línea de defensa de al menos 3 metros de ancho sin vegetación alrededor de la vivienda.<br>
    Mantené los techos, canaletas y desagües libres de hojas, ramas y pinocha, ya que en un incendio es material combustible que alimenta al fuego.<br>
    Si tenes pileta, mantenela con agua durante todo el año, ya que puede ser utilizada por los bomberos para controlar el avance del fuego.<br>
    Si contás con tubos de gas o un depósito de gas butano (Zeppelin), es muy importante que te asesores sobre las normas de seguridad apropiadas.<br>
    Si tenes depósitos con combustibles líquidos (nafta, gasoil), asegurate de que se encuentren en lugares libres de basura o leña y lejos de la exposición solar.<br>
    Colocá cartelería informativa y a la vista.<br>
    Es recomendable usar rejillas matachispas en los escapes de los vehículos para circular por el monte.</ul>"
  })
  
  #TARJETONES
  #Total
  data_pivot <- reactive({
    res <- filter(df_pivot, incendio_anio == input$periodo)
    res
  })
  
  output$total_i <- renderValueBox({
    data_pivot() %>% filter(tipo_incendio=="total") %>%  
      pull() %>% 
      as.integer() %>% 
      valueBox(subtitle = "Total", color = "red", icon = icon("fas fa-circle"))
    
  })
  
  #Desconocido
  output$desc_i <- renderValueBox({
    data_pivot() %>% filter(tipo_incendio=="desconocida") %>%  
      pull() %>% 
      as.integer() %>% 
      valueBox(subtitle = "Desconocido", color = "yellow", icon = icon("fas fa-ban"))
  })
  #Negligencia
  output$negligencia_i <- renderValueBox({
    data_pivot() %>% filter(tipo_incendio=="negligencia") %>%  
      pull() %>% 
      as.integer() %>% 
      valueBox(subtitle = "Negligencia", color = "orange", icon = icon("fas fa-bolt"))
  })
  #Intencional
  output$intencional_i <- renderValueBox({
    data_pivot() %>% filter(tipo_incendio=="intencional") %>% 
      pull() %>% 
      as.integer() %>%
      valueBox(subtitle = "Intencional", color = "blue", icon = icon("fas fa-bomb" ))
  })
  
  #Natural
  output$natural_i <- renderValueBox({
    data_pivot() %>% filter(tipo_incendio=="natural") %>% 
      pull() %>% 
      as.integer() %>% 
      valueBox(subtitle = "Natural", color = "green", icon = icon("fas fa-tree"))
  })
  
  #Evolución incendios
  output$ev_incendios <- renderPlotly({
    ggplot(df_pn, aes(x=incendio_anio))+
      geom_line(aes(y=incendio_total_numero, color="Total"))+
      geom_line(aes(y=incendio_negligencia_numero, color="Negligencia"))+
      geom_line(aes(y=incendio_natural_numero, color="Natural"))+
      geom_line(aes(y=incendio_intencional_numero, color="Intencional"))+
      labs(title = "Cantidad de incendios por año",
           subtitle = "Parques Nacionales en Argentina",
           x="Año",
           y="Número de incendios",
           caption = "Agro Analytics")+
      theme_minimal()
  })
  #Heatmap
  output$heatmap_incendios <- renderPlot({
    df_pivot_prov %>% filter(incendio_anio== input$periodo) %>% 
      ggplot(aes(x=tipo_incendio, y=incendio_provincia, fill=value))+
      geom_tile()+
      scale_fill_gradient(low = "#fdd49e", high = "#b30000")+
      labs(title = "Tipos causales de incendios por provincia",
           x="",
           y="")+
      theme(axis.text.x=element_text(size=rel(0.9), angle=90))
  })
  #Box por tipo
  output$box_tipo <- renderPlotly({
    df_pivot_prov %>% filter(incendio_provincia== input$prov) %>% 
      ggplot(aes(x=tipo_incendio, y=value))+
      geom_boxplot(aes(color=tipo_incendio), show.legend = FALSE)+
      labs(title = "Distribución de los tipos de incendios forestales",
           subtitle = "Año 1993-2019",
           x="Tipo de incendio",
           y="Número",
           caption = "Agro Analytics")+
      theme_minimal()
  })
  
  #Evolucion provincia
  output$ev_prov <- renderPlotly({
    df_pivot_prov %>% filter(incendio_provincia==input$prov, tipo_incendio=="total") %>% 
      plot_ly(x=~incendio_anio,
              y=~value,
              mode="markers",
              type = "scatter",
              frame =~incendio_anio) %>% 
      layout(title=list(text="Evolución total por provincia"),
             xaxis=list(title="Período"),
             yaxis=list(title="Número de incendios"))
  })
  
  output$data <- renderText({
    "<h3><strong>DATOS:</h3><br>
    <a href=' https://datos.gob.ar/dataset/ambiente-incendios-forestales'> Incendios forestales </a><br>
    <a href='https://www.argentina.gob.ar/ambiente/fuego/conocemas/incendioforestal'>Prevención </a>"
  })
  }
  )

