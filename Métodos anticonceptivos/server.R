
# PAQUETE -----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinythemes)
library(bslib)
require(treemapify)
library(ggalluvial)



# CALCULOS ----------------------------------------------------------------

r_condon <- ac %>% filter(Tratamiento=="Condon_latex") %>% 
  summarise (Media=mean(Relacion_pobl)) %>% pull()

r_media_eti_lev <- ac %>% filter(Tratamiento=="Etinilestradiol+levonorgestrel(comp:0.03-0.15mg)") %>% 
  summarise (Media=mean(Relacion_pobl)) %>% pull()

r_media_n_e <- ac %>% filter(Tratamiento=="Noretisterona+estradiol(amp:50-5mg)") %>% 
  summarise (Media=mean(Relacion_pobl)) %>% pull()

# SHINY -------------------------------------------------------------------

shinyServer(function(input, output){
  #Presentación
  
  output$intro <- renderTable({
    desc
  })
  
  #Natalidad
  
  
  output$tab1 <- renderTable({
    nac %>% group_by(jurisdicion_nombre) %>% 
      summarise(Total=sum(nacimientos_cantidad)) %>% 
      arrange(desc(Total)) %>% 
      top_n(17, Total)
  })
  
  output$fig1 <- renderPlot({
    nac %>% ggplot(aes(x=nacimientos_cantidad, y=jurisdicion_nombre))+
      geom_col(fill="#fb8072")+
      ggthemes::theme_gdocs()+
      labs(y="",
           x="Cantidad de nacimientos")+
      theme(axis.text.x = element_text(size = rel(0.8)),
            legend.text = element_text(size = rel(0.8)),
            title = element_text(size = rel(0.7)),
            axis.text.y = element_text(size = rel(0.8)),
            plot.background = element_rect(color = "white"))
  })
  
  output$fig2 <- renderPlot({
    nac %>% 
      ggplot( aes(x=edad_madre, y=nacimientos_cantidad))+
      geom_col(fill="#fb8072")+
      ggthemes::theme_gdocs()+
      scale_y_continuous(name = "Total de nacimientos", breaks = c(250000,500000,1000000,1250000,1500000,2000000))+
      labs(y="Total de nacimientos",
           x="Rango etario")+
      theme(axis.text.x = element_text(size = rel(0.8)),
            axis.text.y = element_text(size = rel(0.8)),
            plot.background = element_rect(color = "white"))
  })
  
  output$fig3 <- renderPlot({
    nac %>%
      ggplot(aes(x=educacion, y=nacimientos_cantidad))+
      geom_col(fill="#fb8072")+
      ggthemes::theme_gdocs()+
      coord_flip()+
      scale_y_continuous(name = "Total de nacimientos", breaks = c(250000,500000,1000000,1250000,1500000,2000000))+
      labs(y="Total de nacimientos",
           x="")+
      theme(axis.text.x = element_text(size = rel(0.7)),
            axis.text.y = element_text(size = rel(0.8)),
            plot.background = element_rect(color = "white"))
  })
  
  output$fig4 <- renderPlot({
    nac %>% filter(jurisdicion_nombre==input$prov) %>% 
      group_by(edad_madre, educacion) %>% 
      summarise(Total=sum(nacimientos_cantidad)) %>% 
      ggplot(aes(axis1 = edad_madre, axis2 = educacion, y = Total)) +
      geom_alluvium(aes(fill = educacion), show.legend = F) +
      geom_stratum() +
      geom_text(stat = "stratum",
                aes(label = after_stat(stratum)), size=3.5) +
      theme_void()
  })
  
  output$tab3 <- renderTable({
    nac %>% filter(edad_madre=="Menor de 15") %>% 
      group_by(jurisdicion_nombre) %>% 
      summarise(Total=sum(nacimientos_cantidad)) %>% 
      arrange(desc(Total))
  })
  
  output$fig5 <- renderPlot({
    nac %>% filter(edad_madre=="Menor de 15")  %>% 
      ggplot(aes(x=jurisdicion_nombre, y=educacion, fill=nacimientos_cantidad)) +
      geom_tile()+
      scale_fill_gradient(low="#c994c7" , high = "#980043")+
      ggthemes::theme_gdocs()+
      labs(title = "Relación de natalidad total según",
           subtitle = "nivel educativo y provinicas en menores de 15 años",
           y="",
           x="")+
      theme(axis.text.x = element_text(angle = 90, size = rel(0.8)),
            axis.text.y = element_text(size = rel(0.7)),
            legend.text = element_text(size = rel(0.6)),
            legend.title = element_text(size = rel(0.7)),
            legend.background = element_rect(color = "grey", size = 0.5),
            title = element_text(size = rel(0.65)),
            plot.background = element_rect(color = "white"))
  })
  
  #Tratamiento
  
 
  
  output$fig6 <- renderPlot({
    ac %>% ggplot(aes(x=Cantidad, y=jurisdicion_nombre))+
      geom_col(aes(fill=Tipo), position = "fill")+
      scale_fill_manual(values = gama)+
      ggthemes::theme_gdocs()+
      labs(y="",
           x="")+
      theme(axis.text.x = element_text(size = rel(0.8)),
            legend.text = element_text(size = rel(0.8)),
            legend.title = element_text(size = rel(0.8)),
            legend.background = element_rect(colour = "grey", size = 0.5),
            axis.text.y = element_text(size = rel(0.8)),
            plot.background = element_rect(color = "white"))
  })
  
  output$tab4 <- renderTable({
    ac %>% group_by(Tipo) %>% 
      summarise(Total=sum(Cantidad)) %>% 
      arrange(desc(Total))
  })
  
  output$fig7 <- renderPlot({
    ac %>% filter(jurisdicion_nombre==input$loc) %>% 
      ggplot(aes(area=Cantidad, fill=Tratamiento, label=Cantidad))+
      geom_treemap()+
      geom_treemap_text(colour="white",
                        place= "bottom",
                        size=8)+
      ggthemes::theme_gdocs() +
      scale_fill_manual(values = c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9",
                                   "#bc80bd","#ccebc5","#ffed6f","#ff7f00","#1f78b4"))+
      theme(legend.text = element_text(size = rel(0.6)),
            legend.title = element_text(size = rel(0.7)),
            legend.background = element_rect(colour = "grey", size = 0.5),
            axis.title = element_text( face = "bold", size = rel(0.8)),
            plot.background = element_rect(color = "white"))
    
  })
  
  
  output$fig8 <- renderPlot({
    ac %>% filter(jurisdicion_nombre==input$loc) %>% 
      group_by(Tratamiento) %>% 
      summarise(Total=mean(Relacion_pobl)) %>% 
      arrange(desc(Total)) %>% 
      top_n(3, Total) %>% 
      ggplot(aes(y=Tratamiento, x=Total))+
      geom_col(aes(fill=Tratamiento), show.legend = F)+
      scale_fill_manual(values = c("#8dd3c7", "#fdb462", "#b3de69"))+
      geom_vline(xintercept = r_media_eti_lev, color="#fdb462")+
      geom_vline(xintercept = r_condon, color="#8dd3c7")+
      geom_vline(xintercept = r_media_n_e, color="#b3de69")+
      ggthemes::theme_gdocs()+
      labs(y="",
           caption= "*Las rectas representan la media nacional")+
      theme(axis.text.y = element_text(size = rel(0.8)),
            axis.text.x = element_text(size = rel(0.8)),
            plot.background = element_rect(color = "white"))
  })
  
  output$tab5 <- renderDataTable({
    ac %>% filter(Tratamiento=="Misoprostol(200mcg)") %>% 
      group_by(jurisdicion_nombre) %>% 
      summarise(Total=max(Relacion_pobl)) %>% 
      arrange(desc(Total))
  })
  
  output$tab6 <- renderDataTable({
    ac %>% filter(Tratamiento=="Levonorgestrel(comp:1.5mg)") %>% 
      group_by(jurisdicion_nombre) %>% 
      summarise(Total=max(Relacion_pobl)) %>% 
      arrange(desc(Total))
  })
  
  output$fig10 <- renderPlot({
    ac %>% filter(jurisdicion_nombre %in% c("Buenos Aires", "Chaco", "Misiones", "Santa Fe", "Salta")) %>% 
      ggplot(aes(x=jurisdicion_nombre, y=Relacion_pobl))+
      geom_col(aes(fill=Tratamiento))+
      ggthemes::theme_gdocs()+
      scale_fill_manual(values = gama)+
      labs(x="",
           y="Relación n° tratamientos/población sexualmente activa")+
      theme(axis.text.x = element_text(size = rel(0.8)),
            legend.text = element_text(size = rel(0.8)),
            legend.title = element_text(size = rel(0.8)),
            legend.background = element_rect(colour = "grey", size = 0.5),
            axis.text.y = element_text(size = rel(0.8)),
            plot.background = element_rect(color = "white"))
  })
  
})
