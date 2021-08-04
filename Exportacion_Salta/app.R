
# Paquetes ----------------------------------------------------------------

library(tidyverse)

library(dplyr)

library(ggplot2)

library(shiny)

library(shinythemes)

# Base de datos -----------------------------------------------------------

path_expo <- here::here("expo-2020-vegetales-certificados-senasa.csv")

df_expo <-utils:: read.csv(path_expo,
                           header = TRUE,
                           sep = ",", 
                           encoding = "UTF-8")



# Limpieza ----------------------------------------------------------------

expo_salta <-  df_expo %>% dplyr::filter(provincia=="Salta") %>% 
  mutate(X.U.FEFF.fecha=as.Date(expo_salta$X.U.FEFF.fecha))

lista_top <- expo_salta %>% group_by(mercaderia_certificada) %>% 
  summarise(cant=sum(tn)) %>% 
  arrange(desc(cant)) %>% 
  top_n(10, cant) %>% 
  select(mercaderia_certificada) %>% pull()



# UI ----------------------------------------------------------------------


ui <- fluidPage(theme=shinytheme("united"),
                titlePanel("Exportaciones en Salta"),
                tabsetPanel(
                  tabPanel("Exportaciones", icon = icon("fas fa-globe"),
                           div(
                             h3("Campaña 2019/20"),
                             hr(),
                             p(em("Diseñado por"), strong("Agro Analytics")),
                             br(),
                             a(href="https://datos.gob.ar/dataset/agroindustria-senasa---exportacion-productos-origen-vegetal-certificados", "Exportaciones SENASA"),
                             hr(),
                             p("Las actividades económicas de la provincia representan, en conjunto, aproximadamente el 1% del PBI de la Argentina. Pero si se considera que la población de Salta es algo superior al 3% de la población del país, ello significa que el PBI per cápita de la provincia está muy por debajo de la media nacional."),
                             
                             hr(),
                             wordcloud2::wordcloud2Output("wc1"),
                             br(),
                             p("Los principales productos exportados y su país destino se puede observar en la siguiente gráfica."),
                             
                             dateInput(inputId="fecha",
                                       label = "Seleccione fecha: ",
                                       value = as.Date("2020-01-03"),
                                       min = min(expo_salta$X.U.FEFF.fecha),
                                       max = max(expo_salta$X.U.FEFF.fecha)),
                             
                             br(),
                             
                             plotly::plotlyOutput("fig1")
                             )),
                  tabPanel("Productos", icon = icon("fas fa-pagelines"),
                           div(
                             fluidPage(fluidRow(
                               column(width=6, 
                                      selectInput(inputId="prod",
                                                  label = "Seleccione un producto: ",
                                                  choices = unique(expo_salta$mercaderia_certificada),
                                                  multiple = F,
                                                  selected = "Grano de Poroto"),
                                      
                                      selectInput(inputId="cont",
                                                  label = "Seleccione un continente: ",
                                                  choices = unique(expo_salta$continente),
                                                  multiple = F,
                                                  selected = "Asia"))
                             )),
                             br(),
                             plotOutput("fig2"),
                             hr(),
                             plotOutput("fig3"),
                             hr(),
                             plotOutput("fig4"),
                             br(),
                             hr(),
                             p("En la siguiente tabla se puede explorar los productos exportados por la provincia"),
                             DT::dataTableOutput("salta_prod")
                             
                           )),#tabpanel2
                  tabPanel("Top 10", icon = icon("fas fa-star"),
                           div(
                             p("Los diez productos más exporados durante la campaña 2019/20 en la provincia de Salta"),
                             fluidPage(fluidRow(
                               column(width=2, selectInput(inputId="top",
                                                           label = "Seleccione un producto:",
                                                           choices = lista_top,
                                                           multiple = F,
                                                           selected = "Azucar"
                                                           
                               )),
                               column(width=10, 
                                      plotOutput("fig6"),
                                      plotly::plotlyOutput("fig7"))
                             ))
                             
                           ))#tabpanel3
                  
                )
)

# Server ------------------------------------------------------------------


server <- function(input, output){
  
  #wordcloud de productos exportados
  
  output$wc1 <- wordcloud2::renderWordcloud2({
    
    expo_salta %>% group_by(mercaderia_certificada) %>% 
      count() %>% 
      arrange(desc(n)) %>% 
      wordcloud2::wordcloud2(size = 6,
                             color = "random-dark",
                             backgroundColor = "white")
  })
  
  #fig1:graf de barra prod/tn
  
  output$fig1 <- plotly::renderPlotly({
    
    expo_salta %>% dplyr::filter(X.U.FEFF.fecha==input$fecha) %>% 
      ggplot2::ggplot(aes(x=mercaderia_certificada, y=tn, fill=pais_destino))+
      geom_col()+
      coord_flip()+
      labs(title = "Productos exportados según países destinos",
           subtitle = "Campaña 19/20",
           x="",
           y="Tonelada",
           caption = "Agro Analytics")+
      ggplot2:: theme( legend.title = element_text(size = rel(0.7)),
                       legend.text = element_text(size = rel(0.5)),
                       legend.position = "bottom")+
      theme_minimal()+
      scale_fill_brewer(type = "qual", palette = "Set3")
  })
  
  
  
  #tabpanel productos
  #An por prod y continente:fig2
  
  output$fig2 <-renderPlot({
    
    expo_salta %>% dplyr::filter(mercaderia_certificada==input$prod, continente==input$cont) %>% 
      ggplot2:: ggplot(aes(x=pais_destino, y=tn))+
      geom_jitter(aes(color=pais_destino),size=3, show.legend = FALSE)+
      coord_flip()+
      labs(title = "Exportación en toneladas",
           subtitle = "Campaña 19/20",
           y="Toneladas",
           x="",
           caption = "Agro Analytics")+
      theme_minimal()+
      scale_fill_brewer(type = "qual", palette = "Set3")
  })
  
  #fig3:An por producto en funcion de la fecha
  output$fig3 <- renderPlot({
    
    expo_salta %>% dplyr::filter(mercaderia_certificada==input$prod & continente==input$cont) %>%
      ggplot2:: ggplot(aes(x=X.U.FEFF.fecha, y=tn))+
      geom_line(aes(color=pais_destino))+
      labs(title = "Distribución temporal de las exportaciones",
           x="Periodo",
           y="Tonelada",
           caption = "Agro Analytics")+
      ggplot2:: theme( legend.title = element_text(size = rel(0.7)),
                       legend.text = element_text(size = rel(0.5)))+
      theme_minimal()+
      scale_fill_brewer(type = "qual", palette = "Set3")
  })
  #cuadro
  output$salta_prod <- DT::renderDataTable({
    expo_salta %>% select(mercaderia_certificada, continente, pais_destino, tn)
    
  })
  
  #Ppal forma de exportacion
  output$fig4 <- renderPlot({
    expo_salta %>% group_by(transporte) %>% 
      count() %>% 
      arrange(desc(n)) %>% 
      ggplot2::ggplot(aes(x=transporte, y=n))+
      geom_col(aes(fill=transporte), show.legend = F)+
      coord_flip()+
      labs(title = "Tipos de transporte",
           x="",
           y="Cantidad",
           caption = "Agro Analytics")+
      theme_minimal()+
      scale_fill_brewer(type = "qual", palette = "Set3")
  })
  
  
  
  #tabpanel top 10
  #tn exportadas segun transporte:fig6
  output$fig6 <- renderPlot({
    
    expo_salta %>% dplyr::filter(mercaderia_certificada==input$top) %>% 
      ggplot2::ggplot(aes(x=pais_destino, y=tn, fill=transporte))+
      geom_col()+
      coord_flip()+
      ggplot2::theme(axis.text.x=element_text(size=rel(0.7), angle=90),
                     legend.title = element_text(size = rel(0.7)),
                     legend.text = element_text(size = rel(0.5)))+
      labs(title = "Toneladas exportadas según transporte",
           x="",
           y="",
           caption = "Agro Analytics")+
      theme_minimal()+
      scale_fill_brewer(type = "qual", palette = "Set3")
  })
  
  #epocas de exportacion
  output$fig7 <- plotly::renderPlotly({
    
    expo_salta %>% dplyr::filter(mercaderia_certificada==input$top) %>% 
      ggplot2::ggplot(aes(x=X.U.FEFF.fecha, y=tn, color=pais_destino))+
      geom_line()+
      facet_grid(~continente)+
      labs(title = "Épocas de exportación",
           x="",
           y="Toneladas",
           caption = "Agro Analytics")+
      ggplot2:: theme(legend.position = "bottom",
                      legend.title = element_text(size = rel(0.7)),
                      legend.text = element_text(size = rel(0.5)))+
      theme_minimal()+
      scale_fill_brewer(type = "qual", palette = "Set3")
  })
  
  
}



shinyApp(ui = ui, server = server)

