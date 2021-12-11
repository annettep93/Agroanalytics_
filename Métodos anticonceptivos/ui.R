

# PAQUETE -----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinythemes)
library(bslib)
require(treemapify)
library(ggalluvial)

# DATOS -------------------------------------------------------------------
gama <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9",
          "#bc80bd","#ccebc5","#ffed6f","#ff7f00","#1f78b4")

#Métodos:descripción
desc <- read.csv(here::here("DATA", "tratamientos-embarazo.csv"),
                 header = T,
                 encoding = "UTF-8",
                 sep = ";")

colnames(desc) <- c("Tratamientos", "Descripción")

#Métodos anticonceptivos 
ac <- read.csv(here::here("DATA", "tratamientos_ac.csv"),
                 header = T,
                 sep = ",")

ac$Cantidad <- as.numeric(ac$Cantidad)

#Nacimientos
nac <- read.csv(here::here("DATA", "nacimientos.csv"),
                 header = T,
                 sep = ",")

nac$nacimientos_cantidad <- as.numeric(nac$nacimientos_cantidad)
nac <- nac %>% mutate(jurisdicion_nombre=case_when(jurisdicion_nombre=="Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ "Tierra del Fuego",
                                            T~ as.character(jurisdicion_nombre) ))

# SHINY -------------------------------------------------------------------


shinyUI( fluidPage(
  includeCSS("www/bootstrap.min.css"),

          
  tabsetPanel(
  type = "pills",
    #Intro
    tabPanel("Presentación",
             hr(),
             tags$img(src = "portada1.png", height="450", width="900"),
             hr(),
             br(),
             h4("Los principales métodos anticonceptivos brindados por el Estado se detallan en la siguiente tabla."),
             p(),
             tableOutput("intro")
             
             
            
             
      
    ),
    #Natalidad
    tabPanel("Natalidad",
             tags$img(src = "natalidad1.png", height="450", width="900"),
             
             hr(),
             br(),
              fluidRow(
               column(width = 3,
                      tableOutput("tab1")),
               column(width = 8,
                      tagList(div(class="text-danger",
                                  h3("Distribución de nacimientos por provincia durante 2019")),
                              div(class="text-muted",
                                  p("Los datos fueron recopilados de la página de Datos Abiertos Argentina"))),
                      hr(),
                      plotOutput("fig1"))
             ),
             hr(),
             br(),
             fluidRow(
               column(width = 6,
                      h3("Distribución según rango etario"),
                      p("Argentina- Año 2019"),
                      plotOutput("fig2")),
               column(width = 6,
                      h3("Distribución según nivel educativo"),
                      p("Argentina- Año 2019"),
                      plotOutput("fig3"))
             ),
             hr(),
             fluidRow(
               column(width = 6,
                      h3("Relación entre edad y nivel educativo por provincia"),
                      p("¿Qué relación tiene el número de nacimientos con respecto a la edad y estudios realizados por la gestante? ")),
               column(width = 6,
                      selectInput(inputId = "prov",
                                  label = "Seleccione una provincia:",
                                  choices = unique(nac$jurisdicion_nombre),
                                  selected = "Salta",                 
                                  multiple = F)),
               column(width = 12,
                      plotOutput("fig4"))
             ),
             hr(),
             br(),
             tags$blockquote("¿Cómo es el caso en menores de 15 años?"),
             ##<figure class="text-end">
             ##<blockquote class="blockquote">
               ##<p class="mb-0">¿Cómo es el caso en las gestantes menores de 15 años?</p>
               
               ##</blockquote>
               ##</figure>
             hr(),
             br(),
             
             fluidRow(
               column(width = 3,
                      tableOutput("tab3")),
               column(width = 9,
                      plotOutput("fig5"))
             )
             ),
    
    
    #Método anticonceptivo
    tabPanel("Tratamientos",
             tags$img(src = "tratamiento.png", height="450", width="900"),
             hr(),
             p("Los datos visualizados son del primer y segundo trimestre 2021."),
             hr(),
             tags$blockquote("¿Cuál es el método anticonceptivo más utilizado en Argentina?"),
             ##<figure class="text-end">
             ##<blockquote class="blockquote">
             ##<h4 class="mb-0">¿Cuál es el método anticonceptivo más utilizado en cada provincia?</h4>
             
             ##</blockquote>
             ##</figure>
             
             hr(),
             p("Análisis en función del número de habitantes sexualmente activos (15-45 años)"),
             br(),
             fluidRow(
               column(width = 9,
                      plotOutput("fig6")),
               column(width = 3,
                      tableOutput("tab4"))
             ),
             hr(),
             br(),
             fluidRow(
               hr(),
               column(width = 12,
                      selectInput(inputId = "loc",
                                  label = "Seleccione provincia:",
                                  choices = unique(ac$jurisdicion_nombre),
                                  selected = "Salta",
                                  multiple = F)),
               column(width = 6,
                      br(),
                      h4("Cantidad de tratamientos empleados según provincia"),
                      plotOutput("fig7")),
               column(width = 6,
                      h4("Relación de los principales 3 métodos en función de la población sexualmente activa"),
                      plotOutput("fig8"))
             ),
             hr(),
             br(),
             tags$blockquote("El Misoprostol es utilizado para la detención del embarazo durante las primeras semanas. Mientras que el Levonogestrel es conocido por 'la pastilla del día después'. Muchas mujeres mal utilizan este método como tratamiento anticonceptivo."),
             ##<figure class="text-end">
             ##<blockquote class="blockquote">
             ##<h4 class="mb-0">El Misoprostol es utilizado para la detención del embarazo durante las primeras semanas. Mientras que el Levonorgestrel(comp:1.5mg) es conocido por 'la pastilla del día después'. Muchas mujeres mal utilizan este método como tratamiento anticonceptivo.</h4>
             
             ##</blockquote>
             ##</figure>
             
             h4("¿En qué lugar hay mayor relación de uso de dichos tratamientos en función del número de habitantes sexualmente activos?"),
            hr(),
             fluidRow(
               column(width = 6,
                      tags$h4("Misoprostol (200mcg)"),
                      ##<h4 class="text-danger">Misoprostol (200 mcg)</h4>
                      dataTableOutput("tab5")),
               column(width = 6,
                      tags$h4("Levonorgestrel(comp:1.5mg)"),
                      ##<h4 class="text-danger">Levonorgestrel(comp:1.5mg)</h4>
                      dataTableOutput("tab6"))
             ),
             hr(),
            
             
             tags$h4("Las principales cinco provincias con mayor cantidad de nacimientos en menores de 15 años fueron: Buenos Aires, Chaco, Misiones, Santa Fe y Salta. "),
             ##<h4 class="text-primary">Las principales cinco provincias con mayor cantidad de nacimientos en menores de 15 años fueron: Buenos Aires, Chaco, Misiones, Santa Fe y Salta. </h4>
             tags$h3("¿Cuáles son los tratamientos que prevalecen en ellas?"),
             ##<h3 class="text-success">¿Cuáles son los tratamientos que prevalecen en ellas?</h3>
             
             hr(),
             
             fluidRow(
               column(width = 12,
                      plotOutput("fig10"))
             )
             
             ),#Tratamiento
  
  #Info
  tabPanel("Información",
           tags$img(src = "info.png", height="450", width="900"),
           
           hr(),
           br(),
           
           
           fluidRow(
             column(width = 4,
                    tags$img(src="agro.png", height="560", width="280")),
             column(width = 8,
                    h1("Base de datos:"),
                    hr(),
                    br(),
                    a("Misoprostol", href="https://bancos.salud.gob.ar/sites/default/files/2020-12/Folleto-ILE-con-medicamentos2.pdf#:~:text=Es%20un%20medicamento%20que%20produce%20contracciones%20enel%20%C3%BAtero.,producci%C3%B3n%20p%C3%BAblica%20y%20se%20llama%20Misoprostol%20200%20mcg.
"),
                    br(),
                    a("Levonogestrel 1.5mg",  href="https://www.argentina.gob.ar/justicia/derechofacil/leysimple/pildora-del-dia-despues"),
                    br(),
                    a("Etonogestrel", href="https://ar.prvademecum.com/medicamento/implanon-nxt-22846/
"),
                    br(),
                    a("Levonogestrel 52 mg",  href="http://amada.org.ar/index.php/revista/numeros-anteriores/volumen-14-n-1-2018/237-tasas-de-embarazo-asociadas-con-el-uso-extendido-de-sistema-uterino-liberador-de-levonorgestrel-de-52-mg-20-ug-dia-mas-alla-de-los-5-anos-una-revision-de-historias-clinicas-de-776-mujeres-en-brasil"),
                    br(),
                    a("DIUT-DIUM",  href="https://www.argentina.gob.ar/salud/sexual/diu"),
                    br(),
                    a("Condon",  href="https://www.argentina.gob.ar/salud/sexual/preservativos"),
                    br(),
                    a("Minipildora",  href="https://www.argentina.gob.ar/salud/sexual/pastillas-una-hormona"),
                    br(),
                    a("Medroxiprogesterona- Estradiol amp",  href="https://www.argentina.gob.ar/salud/sexual/anticonceptivos-inyectables"),
                    br(),
                    a("Pastillas",  href="https://www.argentina.gob.ar/salud/sexual/pastillas-combinadas"),
                    br(),
                    a("Natalidad",  href="http://www.deis.msal.gov.ar/wp-content/uploads/2019/07/Poblacion-adolescente-2.pdf"),
                    br(),
                    a("Dato censal",  href="https://static.ign.gob.ar/anida/fasciculos/fasc_composicion_poblacion.pdf#:~:text=Para%20la%20poblaci%C3%B3n%20de%20Argentina%20en%20su%20conjunto%2C,y%2019.523.766%20varones%2C%20que%20corresponde%20al%2048%2C67%25%20restante.")
                    
                    )
           )
           )#Info
    
    
    )
  )

)
