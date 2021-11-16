# Paquetes ----------------------------------------------------------------

require(tidyverse)
require(shiny)
require(shinydashboard)

c_21 <- data.frame(casos_21="1366",
           altas_21="1307",
           mortandad_21="29")

# SERVER ------------------------------------------------------------------

shinyServer(function(input, output){
  
  #Intro
  
  output$titulo <- renderText({
    "<center>
    <h3><stong>COVID-19 en Tartagal, Salta (AR)</strong></center> "
  })
  
  output$introduccion <- renderText({
    
    "<justify>El primero de diciembre de 2019, un hombre de 70 años fue hospitalizado con síntomas de fiebre alta y neumonía; veintinueve días después ya eran varias las personas que ingresaron a distintos hospitales de la ciudad central de Wuhan (China) presentando similar sintomatología. Todos esos casos se relacionaban con un lugar en particular: el mercado mayorista de mariscos de Huanan. <br>
    <br>Durante el último día del año, la Comisión de Salud de Wuhan emitió un comunicado de prensa a sus ciudadanos indicando 27 casos de neumonía viral pero que no había evidencia clara de transmisión de persona a persona. Luego de 12 días recién logra compartir las secuencias genéticas con la comunidad internacional. Es así como comenzó a expandirse por el mundo,  la noticia sobre una nueva enfermedad viral muy similar al SARS.<br>
    <br>Para nosotros parecía muy lejano que dicha enfermedad llegara, sin embargo para el 3 de marzo de 2020 se confirmó el primer caso importado de COVID-19.  El día anterior, un efector privado de CABA notificó al Sistema Nacional de Vigilancia de la Salud un caso sospechoso que provenía de Europa; se realizó la extracción correspondiente de la muestra y el día 3, el Laboratorio Nacional de Referencia (Servicio de Virosis Respiratorias del INEI-ANLIS 'Carlos Malbrán') notifica el primer positivo en el país.<br>
    <br>Nueve días después del primer caso argentino, el virus llega a la provincia salteña. Y es luego de un poco más de tres meses que logra arribar a la ciudad de Tartagal. Un 24 de junio, la Ministra de Salud (Dra. Josefina Medrano) llega al municipio para una conferencia de prensa anunciando el primer infectado.</justify>"
  })
  
  output$db <- renderText({
    
    "<br><center><dfn>Los datos fueron recopilados de la página de la municipalidad de Tartagal y contienen información hasta el día 4 de noviembre de 2021</dfn></center>"
  })

  #Analisis
  
  output$total_ac <- renderValueBox({
    covid1 %>% filter(Fecha==as.Date("2021-11-04")) %>% 
      select(Casos_acumulados) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "Total acumulados", color = "blue")
  })
  
  output$total_a <- renderValueBox({
    covid1 %>% filter(Fecha==as.Date("2021-11-04")) %>% 
      select(Altas_acumuladas) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard:: valueBox(subtitle = "Altas totales", color = "green")
  })
  
  output$total_f <- renderValueBox({
    covid1 %>% filter(Fecha==as.Date("2021-11-04")) %>% 
      select(Fallecidos_acumulados) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "Total fallecidos", color = "red")
  })
  
  output$total_ac_20 <- renderValueBox({
    covid1 %>% filter(Fecha==as.Date("2020-12-31")) %>% 
      select(Casos_acumulados) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "2020", color = "blue")
  })
  
  output$total_ac_21 <- renderValueBox({
    c_21 %>% 
      select(casos_21) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "2021", color = "blue")
  })
  
  output$total_a_20 <- renderValueBox({
    covid1 %>% filter(Fecha==as.Date("2020-12-31")) %>% 
      select(Altas_acumuladas) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard:: valueBox(subtitle = "2020", color = "green")
  })
  
  
  output$total_a_21 <- renderValueBox({
    c_21 %>% 
      select(altas_21) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard:: valueBox(subtitle = "2021", color = "green")
  })
  
  output$total_f_20 <- renderValueBox({
    covid1 %>% filter(Fecha==as.Date("2020-12-31")) %>% 
      select(Fallecidos_acumulados) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "2020", color = "red")
  })
  
  output$total_f_21 <- renderValueBox({
    c_21 %>% 
      select(mortandad_21) %>% 
      pull() %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "2021", color = "red")
  })
  

  
  output$vs_nvos <-plotly::renderPlotly({
    covid1 %>% filter(Periodo=="2020") %>% ggplot(aes(x=Fecha, y=Casos_nvos))+
      geom_line(color="#7fcdbb")+
      geom_point(color="#225ea8")+
      facet_grid(~Periodo)+
      ggthemes::theme_stata()+
      labs(title = "Comparación de casos positivos año 2020",
           y = "Casos nuevos")+
      theme(axis.text.x = element_text(angle = 90,size = rel(0.1)))
  })
  
  output$nvos_21 <-plotly::renderPlotly({
    covid1 %>% filter(Periodo=="2021") %>%  ggplot(aes(x=Fecha, y=Casos_nvos))+
      geom_line(color="#7fcdbb")+
      geom_point(color="#225ea8")+
      facet_grid(~Periodo)+
      ggthemes::theme_stata()+
      labs(title = "Comparación de casos positivos año 2021",
           y = "Casos nuevos")+
      theme(axis.text.x = element_text(angle = 90,size = rel(0.1)))
  })
  
  output$vs_fallecidos <- plotly::renderPlotly({
    ggplot(covid1, aes(x=Periodo, y=Fallecidos_nvos))+
      geom_col(aes(fill=Mes))+
      ggthemes::theme_stata()+
      labs(title = "Variación de fallecidos año a año y por meses",
           y = "Casos nuevos")+
      scale_fill_manual(values = c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5","#ffed6f"))
  })
  
  re1 <- reactive({
    re <- relacion %>% filter(Medida==input$medida)
    re
  })
  
  
  
 tabla_relacion <-  eventReactive(input$ver,{
   re1()%>% filter(X.U.FEFF.Fecha==input$fecha) %>% 
     select(Medida, Nivel, Relacion)
 })
  
 output$tabla <- renderDataTable({
    tabla_relacion()
  })
 
 
  
})
