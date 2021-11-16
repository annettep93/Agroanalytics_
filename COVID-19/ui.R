# Paquetes ----------------------------------------------------------------

require(tidyverse)
require(shiny)
require(shinydashboard)

# Datos -------------------------------------------------------------------

covid1 <- read.csv(here::here("covid_tgal.csv"),
                   header = T,
                   encoding = "UTF-8",
                   sep = ";")
relacion <-read.csv(here::here("relaciones_hab.csv"),
                    header = T,
                    encoding = "UTF-8",
                    sep = ";")

relacion$X.U.FEFF.Fecha <- lubridate::dmy_hm(relacion$X.U.FEFF.Fecha)
relacion <- relacion %>% mutate(Latitud=str_replace_all(relacion$Latitud, ",", "\\."),
                    Longitud=str_replace_all(relacion$Longitud, ",", "\\."))



# UI ----------------------------------------------------------------------

ui <- shinydashboard::dashboardPage(skin = "blue",
                                    shinydashboard::dashboardHeader(title = "COVID-19 en Tartagal"),
                                    shinydashboard::dashboardSidebar(
                        sidebarMenu(id="sidebar",
                                    menuItem("Introducción", tabName = "intro"),
                                    menuItem("Análisis", tabName = "data")
                                    
                                    )),
                        shinydashboard::dashboardBody(tabItems(
                                #Intro
                                tabItem(
                                    tabName = "intro",
                                    fluidRow(
                                        uiOutput("titulo"),
                                        uiOutput("introduccion"),
                                        uiOutput("db"))
                                    
                                ),
                                #Analytic
                                tabItem(tabName = "data",
                                        fluidRow(
                                          shinydashboard::valueBoxOutput("total_ac",
                                                           width = 4),
                                          shinydashboard::valueBoxOutput("total_a",
                                                           width = 4),
                                          shinydashboard::valueBoxOutput("total_f",
                                                           width = 4)),
                                        fluidRow(
                                          shinydashboard:: valueBoxOutput("total_ac_20",
                                                           width = 4),
                                          shinydashboard::valueBoxOutput("total_a_20",
                                                           width = 4),
                                          shinydashboard::valueBoxOutput("total_f_20",
                                                           width = 4),
                                          shinydashboard::valueBoxOutput("total_ac_21",
                                                           width = 4),
                                          shinydashboard:: valueBoxOutput("total_a_21",
                                                           width = 4),
                                          shinydashboard::valueBoxOutput("total_f_21",
                                                           width = 4)
                                        ),
                                        
                                        fluidRow(
                                            shinydashboard::box(width = 12, 
                                                              plotly:: plotlyOutput("vs_nvos")),
                                            shinydashboard::box(width = 12, 
                                                                plotly::plotlyOutput("nvos_21")),
                                            shinydashboard::box(width = 12, 
                                                                plotly::plotlyOutput("vs_fallecidos"))
                                        ),
                                        fluidRow(
                                          shinydashboard::box(width = 12,
                                                              background = "navy",
                                                              title = "Relación de n° de casos (positivos o fallecidos) con el n° de habitantes"),
                                            shinydashboard::box(width = 6,
                                                                background = "navy",
                                                                selectInput(inputId = "medida",
                                                                            label = "Elija el tipo de relación a observar:",
                                                                            choices = unique(relacion$Medida),
                                                                            selected = "Casos_acumulados"
                                                                )),
                                            shinydashboard::box(width = 6,
                                                                background = "navy",
                                                                dateInput(inputId = "fecha",
                                                                               label = "Seleccione la fecha a visualizar",
                                                                               value = relacion$X.U.FEFF.Fecha,
                                                                          min = as.Date("2020-04-06"),
                                                                          max = as.Date("2021-11-04")
                                                                          
                                                                )),
                                            shinydashboard::box(width = 12,
                                                                background = "navy",
                                                                actionButton("ver", label = "Mostrar relación")),
                                            shinydashboard::box(width = 12,
                                                                dataTableOutput("tabla"))
                                        )
                                        
                                        )#data
                            ))#B
                        
                    )#P




