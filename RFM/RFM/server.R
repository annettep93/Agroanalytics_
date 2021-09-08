
# Paquete -----------------------------------------------------------------

library(tidyverse)
library(shiny)
library(ggthemes)
library(treemapify)

paleta= c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5","#ffed6f","#33a02c","#ff7f00","#fb9a99","#e31a1c","#6a3d9a","#1f78b4","#ce1256","#980043","#67001f","#238443","#006837",
          "#d53e4f","#f46d43","#fdae61","#fee08b","#ffffbf","#e6f598","#abdda4","#66c2a5","#3288bd",
          "#810f7c","#4d004b","#ce1256","#980043","#67001f","#6a51a3","#54278f","#3f007d")

# Server ------------------------------------------------------------------

shinyServer(function(input,output, session){
  
  #Reactive para box
  gral_re<- reactive({
    re1 <- Supertienda_rfm %>% filter(segment==input$segment)
    re1
  })
  
  #Filtro de segmentación por provincia
  segmento <- reactive({
    re3 <- Supertienda_rfm %>% filter(Estado==input$id_prov) %>% 
      select(segment) %>% 
      unique() %>% 
      pull()
    
    re3
  })
  
  output$id_seg <- renderUI({
    selectInput(inputId = "id_seg",
                label = "Segmento:",
                choices = segmento(),
                selected = "At Risk")
  })
  
  #Reactive box por país y segmento
  pais_re <- reactive({
    re2 <- Supertienda_rfm %>% filter(Estado==input$id_prov & segment==input$id_seg  )
    re2
  })
  
  #Cliente
  
  client_seg <- reactive({
    re3 <- Supertienda_rfm %>% filter(Estado==input$cliente_prov) %>% 
      select(segment) %>% 
      unique() %>% 
      pull()
    
    re3
  })
  
  output$client_seg <- renderUI({
    radioButtons(inputId = "client_seg",
                label = "Segmento:",
                choices = client_seg(),
                selected = "At Risk")
  })
  

  #General
  
  output$titulo <- renderText({
    "<h2><strong><center>ANÁLISIS RFM</center></strong></h2>"
  })
  
  output$fig1 <- renderPlot({
    
    ggplot(Supertienda_rfm, aes(x=segment))+
      geom_bar(aes(fill=Value), show.legend = T)+
      coord_flip()+
      scale_fill_manual(values = paleta)+
      ggthemes::theme_solarized()+
      labs(title = "Segmentación de clientes",
           subtitle = "Supertienda",
           x="",
           y="Cantidad",
           caption = "Agro Analytics")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            axis.text.x = element_text(size = rel(0.9)),
            axis.text.y = element_text(size = rel(0.9)),
            axis.title.x = element_text(size = rel(0.9)),
            legend.text = element_text(size = rel(0.7)),
            legend.title = element_text(size = rel(0.7)),
            legend.position = "bottom")
  })
  
  output$fig2 <- renderPlot({
    
    Supertienda_rfm %>% group_by(segment) %>% 
      summarise(Total_venta=sum(Total),
                Total_ganancia=sum(Ganancia)) %>% 
      ggplot(aes(x=segment))+
      geom_col(aes(y=Total_venta), fill="#80b1d3")+
      geom_col(aes(y=Total_ganancia), fill="#fdb462")+
      
      scale_fill_manual(values = c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5","#ffed6f","#33a02c","#ff7f00","#fb9a99","#e31a1c","#6a3d9a","#1f78b4"))+
      ggthemes::theme_solarized()+
      labs(title = "Ventas y ganancias totales por",
           subtitle = "segmentación de clientes",
           x="",
           y="$",
           caption = "Ventas (celeste). Ganancias(amarillo) ")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(0.9), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            plot.caption = element_text(hjust = 0.2),
            axis.text.x = element_text(size = rel(0.8), angle = 90),
            axis.text.y = element_text(size = rel(0.8)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8)))
  })
  
  output$fig3 <- renderPlot({
    
    Supertienda_rfm %>% filter(segment==input$segment) %>% 
      ggplot()+
      geom_bar(aes(x=Pais, fill=Pais), show.legend = F)+
      coord_flip()+
      scale_fill_manual(values = paleta)+
      ggthemes::theme_solarized()+
      labs(title = "Distribución de los clientes",
           subtitle = "Supertienda",
           x="País",
           y="Cantidad",
           caption = "Agro Analytics")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            axis.text.x = element_text(size = rel(0.9)),
            axis.text.y = element_text(size = rel(0.9)),
            axis.title.x = element_text(size = rel(0.9)),
            axis.title.y = element_text(size = rel(0.9)))
  })
  
  output$fig4 <- renderPlot({
    
   Supertienda_rfm %>% filter(segment==input$segment) %>% 
      ggplot()+
      geom_bar(aes(x=Subcategoria, fill=Subcategoria), show.legend = F)+
      
      coord_flip()+
      scale_fill_manual(values = c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd","#ccebc5","#ffed6f","#33a02c","#ff7f00","#fb9a99","#e31a1c","#6a3d9a","#1f78b4"))+
      ggthemes::theme_solarized()+
      labs(title = "Productos más requieridos según ",
           subtitle = "segmentación de clientes",
           x="País",
           y="Cantidad",
           caption = "Agro Analytics")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(0.9), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            axis.text.x = element_text(size = rel(0.8)),
            axis.text.y = element_text(size = rel(0.8)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8)))
  })
  
  
  
  output$box1 <- shinydashboard::renderValueBox({
    sum(gral_re()$Total) %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "Total de Ventas ($)", color = "blue")
  })
  
  output$box2 <- shinydashboard::renderValueBox({
    sum(gral_re()$Ganancia) %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "Total de ganancias ($)", color = "yellow")
  })
  
  output$fig5 <- renderPlot({
    Supertienda_rfm %>% filter(segment==input$segment) %>% 
      group_by(Semestre) %>%
      summarise(Total_venta=sum(Total),
                Total_ganancia=sum(Ganancia)) %>% 
      ggplot(aes(x=Semestre))+
      geom_point(aes(y=Total_venta),color="#80b1d3", size=4)+
      geom_text(aes(y= Total_venta, label=round(Total_venta)), size=5, vjust=-2)+
      geom_point(aes(y=Total_ganancia),color="#fdb462", size=4)+
      geom_text(aes(y= Total_ganancia, label=round(Total_ganancia)), size=5, vjust=-1.5)+
      ggthemes::theme_solarized()+
      labs(title = "Ventas y ganancias totales por",
           subtitle = "segmentación de clientes",
           x="",
           y="$",
           caption = "Ventas (celeste). Ganancias(amarillo) ")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(0.9), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            plot.caption = element_text(hjust = 0.2),
            axis.text.x = element_text(size = rel(0.8), angle = 90),
            axis.text.y = element_text(size = rel(0.8)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8)))
    
  })
  
  output$fig6 <- renderPlot({
    Supertienda_rfm %>% filter(segment==input$segment) %>% 
      ggplot(aes(x=Semestre, y=Total))+
      geom_hex(stat="binhex", position = "identity", show.legend = T)+
      scale_fill_gradient(low = "#a6bddb", high = "#02818a")+
      ggthemes::theme_solarized()+
      labs(title = "Evolución de las ventas por",
           subtitle = "segmentación de clientes",
           x="",
           y="$",
           caption = "Agro Analytics")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(0.9), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            plot.caption = element_text(hjust = 0.2),
            axis.text.x = element_text(size = rel(0.8), angle = 90),
            axis.text.y = element_text(size = rel(0.8)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8)),
            legend.text = element_text(size = rel(0.7)),
            legend.title = element_text(size = rel(0.8)),
            legend.position = "right")
  })
  
  
  output$fig7 <- renderPlot({
    Supertienda_rfm %>% filter(segment==input$segment) %>% 
      ggplot(aes(x=Semestre, y=Ganancia))+
      geom_hex(stat="binhex", position = "identity", show.legend = T)+
      scale_fill_gradient(low = "#fdbb84", high =  "#fc4e2a")+
      ggthemes::theme_solarized()+
      labs(title = "Evolución de las ganancias por",
           subtitle = "segmentación de clientes",
           x="",
           y="$",
           caption = "Agro Analytics")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(0.9), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            plot.caption = element_text(hjust = 0.2),
            axis.text.x = element_text(size = rel(0.8), angle = 90),
            axis.text.y = element_text(size = rel(0.8)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8)),
            legend.text = element_text(size = rel(0.7)),
            legend.title = element_text(size = rel(0.8)),
            legend.position = "right")
  })
  #País
 
  
  output$prov <- renderTable({
    Supertienda_rfm %>% filter(Pais==input$pais) %>% 
      select(Estado) %>%
      unique() %>% 
      pull()
  })
  output$fig8 <- renderPlot({
    Supertienda_rfm %>% filter(Pais==input$pais) %>% 
    ggplot( aes(x=segment))+
      geom_bar(aes(fill=Value), show.legend = T)+
      coord_flip()+
      scale_fill_manual(values = paleta)+
      ggthemes::theme_solarized()+
      labs(title = "Segmentación de clientes por país",
           subtitle = "Supertienda",
           x="",
           y="Cantidad",
           caption = "Agro Analytics")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            axis.text.x = element_text(size = rel(0.9)),
            axis.text.y = element_text(size = rel(0.9)),
            axis.title.x = element_text(size = rel(0.9)),
            legend.text = element_text(size = rel(0.7)),
            legend.title = element_text(size = rel(0.7)),
            legend.position = "bottom")
  })
  
  output$fig9 <- renderPlot({
    Supertienda_rfm %>% filter(Pais==input$pais) %>% 
      ggplot(aes(y=Estado, x=segment))+
      geom_jitter(aes(color=Estado), show.legend = F, alpha=0.7)+
      scale_color_manual(values = paleta)+
      ggthemes::theme_solarized()+
      labs(title = "Distribución de los clientes",
           subtitle = "por provincia/estado",
           x="",
           y="",
           caption = "Agro Analytics")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(1), face = "bold"),
            plot.subtitle = element_text(size = rel(1), face = "bold"),
            axis.text.x = element_text(size = rel(0.9), angle = 90),
            axis.text.y = element_text(size = rel(0.9)),
            axis.title.x = element_text(size = rel(0.9)),
            axis.title.y = element_text(size = rel(0.9)))
  })
  
 
  
  output$fig10 <- renderPlot({
    Supertienda_rfm %>% filter(Pais==input$pais & segment==input$pais_seg) %>% 
      group_by(Semestre) %>% 
      summarise(Total_venta=sum(Total),
                Total_ganancia=sum(Ganancia)) %>% 
      ggplot(aes(x=Semestre))+
      geom_col(aes(y=Total_venta), fill="#80b1d3")+
      geom_col(aes(y=Total_ganancia), fill="#fdb462")+
      ggthemes::theme_solarized()+
      labs(title = "Ventas y ganancias totales por",
           subtitle = "segmentación de clientes y país",
           x="",
           y="$",
           caption = "Ventas (celeste). Ganancias(amarillo) ")+
      theme(plot.background = element_rect(size = 1, color = 1),
            plot.title = element_text(size = rel(0.9), face = "bold"),
            plot.subtitle = element_text(size = rel(0.9), face = "bold"),
            plot.caption = element_text(hjust = 0.2),
            axis.text.x = element_text(size = rel(0.8), angle = 90),
            axis.text.y = element_text(size = rel(0.8)),
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_text(size = rel(0.8)))
  })
 
  
  output$box3 <- shinydashboard::renderValueBox({
     
      sum(pais_re()$Total) %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "Total de Ventas", color = "blue")
  })
  
  output$box4 <- shinydashboard::renderValueBox({
     
      sum(pais_re()$Ganancia) %>% 
      as.integer() %>% 
      shinydashboard::valueBox(subtitle = "Total de Ganancias", color = "yellow")
  })
  
  

    
    output$fig11 <- renderPlot({
      pais_re() %>% 
        ggplot(aes(x=Semestre))+
        geom_col(aes(y=Total), fill="#80b1d3")+
        geom_col(aes(y=Ganancia), fill="#fdb462")+
        geom_hline(aes(yintercept=mean(Supertienda_rfm$Ganancia)), color="#fc4e2a")+
        geom_hline(aes(yintercept=mean(Supertienda_rfm$Total)), color="#023858")+
        facet_grid(~Value)+
        ggthemes::theme_solarized()+
        labs(title = "Ventas y ganancias totales por",
             subtitle = "segmentación de clientes y provincia",
             x="",
             y="$",
             caption = "Ventas (celeste). Ganancias(naranja) ")+
        theme(plot.background = element_rect(size = 1, color = 1),
              plot.title = element_text(size = rel(0.9), face = "bold"),
              plot.subtitle = element_text(size = rel(0.9), face = "bold"),
              plot.caption = element_text(hjust = 0.2),
              axis.text.x = element_text(size = rel(0.8), angle = 90),
              axis.text.y = element_text(size = rel(0.8)),
              axis.title.x = element_text(size = rel(0.8)),
              axis.title.y = element_text(size = rel(0.8)))
    })
  
    #Cliente
    
    estados_re <- reactive({
      re4 <- estados %>% filter(Pais==input$cliente_pais)
    })
    
    output$fig12 <- renderPlot({
      
      estados_re()%>% 
        ggplot2::ggplot(aes(x=reorder(Estado, -Total_ganancias), y=Total_ganancias))+
        geom_segment(aes(x = reorder(Estado, -Total_ganancias),
                         xend = reorder(Estado, -Total_ganancias),
                         y = 0, yend = Total_ganancias),
                     color = "gray", lwd = 1) +
        geom_point(size = 4, pch = 21, bg = 7, col = 6)+
        coord_flip()+
        ggthemes::theme_solarized()+
        labs(title = "Ganancias por provincias",
             x="",
             y="",
             caption = "Agro Analytics")+
        theme(plot.background = element_rect(size = 1, color = 1),
              plot.title = element_text(size = rel(1), face = "bold"),
              plot.subtitle = element_text(size = rel(0.9), face = "bold"),
              axis.text.x = element_text(size = rel(0.9)),
              axis.text.y = element_text(size = rel(0.9)),
              axis.title.x = element_text(size = rel(0.9)),
              axis.title.y = element_text(size = rel(0.9)))
        
    })
    
    output$fig13 <- renderPlot({
      
      estados_re()%>% 
        ggplot2::ggplot(aes(x=reorder(Estado, -Total_clientes), y=Total_clientes))+
        geom_segment(aes(x = reorder(Estado, -Total_clientes),
                         xend = reorder(Estado, -Total_clientes),
                         y = 0, yend = Total_clientes),
                     color = "gray", lwd = 1) +
        geom_point(size = 4, pch = 21, bg = 7, col = 6)+
        coord_flip()+
        ggthemes::theme_solarized()+
        labs(title = "Número de clientes por provincias",
             x="",
             y="",
             caption = "Agro Analytics")+
        theme(plot.background = element_rect(size = 1, color = 1),
              plot.title = element_text(size = rel(1), face = "bold"),
              plot.subtitle = element_text(size = rel(0.9), face = "bold"),
              axis.text.x = element_text(size = rel(0.9)),
              axis.text.y = element_text(size = rel(0.9)),
              axis.title.x = element_text(size = rel(0.9)),
              axis.title.y = element_text(size = rel(0.9)))
      
    })
    
    output$fig14 <- renderPlot({
      
      estados_re()%>% 
        ggplot(aes(x=Total_clientes, y=Total_ganancias))+
        geom_point(color="purple", alpha=0.6, size=2)+
        geom_line(color="purple", linetype="dashed")+
        ggthemes::theme_solarized()+
        labs(title = "Número de clientes por provincias",
             x="Número de clientes",
             y="Ganancias",
             caption = "Agro Analytics")+
        theme(plot.background = element_rect(size = 1, color = 1),
              plot.title = element_text(size = rel(1), face = "bold"),
              plot.subtitle = element_text(size = rel(0.9), face = "bold"),
              axis.text.x = element_text(size = rel(0.9)),
              axis.text.y = element_text(size = rel(0.9)),
              axis.title.x = element_text(size = rel(0.9)),
              axis.title.y = element_text(size = rel(0.9)))
    })
    
    output$fig15 <- renderPlot({
      
      Supertienda_rfm %>% filter(Estado==input$cliente_prov) %>% 
        group_by(segment) %>% 
        count() %>% 
        ggplot(aes(area = n, fill = segment,
                   label = n)) +
        geom_treemap() +
        scale_fill_manual(values = paleta)+
        geom_treemap_text(colour = "white", place = "centre",
                          size = 10)+
        labs(title = "Segmentación de clientes por provincia")+
        ggthemes::theme_solarized()+
        theme(plot.background = element_rect(size = 1, color = 1),
              plot.title = element_text(size = rel(1), face = "bold"),
              plot.subtitle = element_text(size = rel(0.9), face = "bold"),
              axis.text.x = element_text(size = rel(0.9)),
              axis.text.y = element_text(size = rel(0.9)),
              axis.title.x = element_text(size = rel(0.9)),
              axis.title.y = element_text(size = rel(0.9)))
      
    })
    
    output$fig16 <- plotly::renderPlotly({
      
      Supertienda_rfm %>% filter(Estado==input$cliente_prov & segment==input$client_seg) %>% 
        ggplot(aes(x=Subcategoria, y=Total))+
        geom_col(aes(fill=Semestre), show.legend = F, position = "dodge")+
        coord_flip()+
        scale_fill_manual(values = paleta)+
        ggthemes::theme_solarized()+
        labs(title = "Ventas de subcategorías por provincia",
             subtitle="y por segmentación",
             x="",
             y="",
             caption = "Agro Analytics")+
        theme(plot.background = element_rect(size = 1, color = 1),
              plot.title = element_text(size = rel(1), face = "bold"),
              plot.subtitle = element_text(size = rel(0.9), face = "bold"),
              axis.text.x = element_text(size = rel(0.9)),
              axis.text.y = element_text(size = rel(0.9)),
              axis.title.x = element_text(size = rel(0.9)),
              axis.title.y = element_text(size = rel(0.9)))
    })
    
    output$importar <- downloadHandler(
      filename = function() {
        paste(input$importar, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(cliente(), file, row.names = FALSE)
      }
    ) 
    
    cliente <- reactive({
      re5 <- Supertienda_rfm %>% filter(Estado==input$cliente_prov & segment==input$client_seg) %>% 
        select(Id_cliente, segment, Value, Subcategoria, Semestre, Total, Ganancia)
      re5
    })
    
    output$table_client <- renderTable({
     
      cliente() 
      
    })
    
    #Datos
  output$table <- renderDataTable({
    Supertienda_rfm %>% 
      select(-Id_fila, -rfm,-Id_pedido,-Categoria,-Descuento, -Estado, -Id_producto, -Nombre_producto, -Region, -Cantidad, -Semestre, -Segmento, -Fecha_envio, -Forma_envio)
  })
})
