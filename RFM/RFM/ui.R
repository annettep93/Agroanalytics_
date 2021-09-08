
# Paquete -----------------------------------------------------------------

library(tidyverse)
library(shiny)
library(ggthemes)


# Datos -------------------------------------------------------------------
Supertienda_rfm <- read.csv(here::here("Supertienda_rfm.csv"),
                            header = T,
                            sep = ",",
                            encoding = "UTF-8")

Supertienda_rfm <-  Supertienda_rfm %>% mutate(Estado=case_when(
  Estado=="Ciudad Autónoma de Buenos Aires" ~ "CABA",
  Estado=="Provincia de Buenos Aires" ~ "Buenos Aires",
  T ~ as.character(Estado)
))


estados <- read.csv(here::here("estados.csv"),
                               header=T,
                               sep=",",
                               encoding="UTF-8")
# UI ----------------------------------------------------------------------

ui <- shinydashboard::dashboardPage(skin = "purple",
                                    shinydashboard::dashboardHeader(title = "Supertienda"),
                                    shinydashboard::dashboardSidebar(
                                      shinydashboard::sidebarMenu(id="sidebar",
                                                                  shinydashboard::menuItem("General", tabName = "grl"),
                                                                  conditionalPanel('input.sidebar=="grl"',
                                                                                   selectInput(inputId = "segment",
                                                                                               label = "Segmentos:",
                                                                                               choices = unique(Supertienda_rfm$segment),
                                                                                               selected = "Champions")),#conditionalPanel
                                                                  shinydashboard::menuItem("País", tabName = "pais"),
                                                                  conditionalPanel('input.sidebar=="pais"',
                                                                                   selectInput(inputId = "pais",
                                                                                               label = "País:",
                                                                                               choices = unique(Supertienda_rfm$Pais),
                                                                                               selected = "Argentina"),
                                                                                   tableOutput("prov"),
                                                                                   selectInput(inputId = "pais_seg",
                                                                                               label = "Segmento:",
                                                                                               choices = unique(Supertienda_rfm$segment),
                                                                                               selected = "Champions")),
                                                                  shinydashboard::menuItem("Clientes", tabName = "client"),
                                                                  shinydashboard::menuItem("Datos", tabName = "data")
                                                                  )),#sidebarMenu
                                      shinydashboard::dashboardBody(
                                        shinydashboard::tabItems(
                                          shinydashboard::tabItem(tabName = "grl",
                                                                  fluidRow(
                                                                    shinydashboard::box( background = "purple",
                                                                                         width = 12,
                                                                                         uiOutput("titulo"))
                                                                  ),
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 12,
                                                                                          plotOutput("fig1")),
                                                                    
                                                                    shinydashboard::box(width = 12,
                                                                                          plotOutput("fig2"))
                                                                  ),
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 6,
                                                                                        plotOutput("fig3")),
                                                                    shinydashboard::box(width = 6,
                                                                                        plotOutput("fig4")),
                                                                    
                                                                    shinydashboard::valueBoxOutput("box1",
                                                                                                   width = 6),
                                                                    
                                                                    shinydashboard::valueBoxOutput("box2",
                                                                                                   width = 6),
                                                                    shinydashboard::box(width = 12,
                                                                                        plotOutput("fig5")),
                                                                    shinydashboard::box(width = 6,
                                                                                        plotOutput("fig6")),
                                                                    shinydashboard::box(width = 6,
                                                                                        plotOutput("fig7"))
                                                                  )
                                                                  ),#General
                                          shinydashboard::tabItem(tabName = "pais",
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 12,
                                                                                        plotOutput("fig8")),
                                                                    shinydashboard::box(width = 6,
                                                                                        plotOutput("fig9")),
                                                                    shinydashboard::box(width = 6,
                                                                                        plotOutput("fig10"))
                                                                  ),
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 6,
                                                                                        background = "purple",
                                                                                        selectInput(inputId = "id_prov",
                                                                                                    label = "Provincia:",
                                                                                                    choices = unique(Supertienda_rfm$Estado),
                                                                                                    selected = "CABA")),
                                                                    shinydashboard::box(width = 6,
                                                                                        background = "purple",
                                                                                        uiOutput("id_seg"))
                                                                                                    
                                                                  ),
                                                                  fluidRow(
                                                                    shinydashboard::valueBoxOutput("box3",
                                                                                                   width = 6),
                                                                    shinydashboard::valueBoxOutput("box4",
                                                                                                   width = 6),
                                                                    shinydashboard::box(width = 12,
                                                                                        plotOutput("fig11"))
                                                                  )
                                                                  ),#Pais
                                          shinydashboard::tabItem(tabName = "client",
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 12,
                                                                                        background = "purple",
                                                                                        selectInput(inputId="cliente_pais",
                                                                                                    label = "País:",
                                                                                                    choices = unique(Supertienda_rfm$Pais),
                                                                                                    selected = "Argentina"))
                                                                  ),
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 4,
                                                                                        plotOutput("fig12")),
                                                                    shinydashboard::box(width = 4,
                                                                                        plotOutput("fig13")),
                                                                    shinydashboard::box(width = 4,
                                                                                        plotOutput("fig14"))
                                                                  ),
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 12,
                                                                                        background = "purple",
                                                                                        selectInput(inputId="cliente_prov",
                                                                                                    label = "Provincia/Estado:",
                                                                                                    choices = unique(Supertienda_rfm$Estado),
                                                                                                    selected = "Salta")),
                                                                    shinydashboard::box(width = 12,
                                                                                        plotOutput("fig15"))
                                                                  ),
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 3,
                                                                                        background = "purple",
                                                                                        uiOutput("client_seg")),
                                                                    shinydashboard::box(width = 9,
                                                                                        plotly::plotlyOutput("fig16"))
                                                                  ),
                                                                  fluidRow(
                                                                    shinydashboard::box(width = 12,
                                                                                        background = "purple",
                                                                                      downloadButton("importar",
                                                                                                     label = "Importar a .csv")),
                                                                    shinydashboard::box(width = 12,
                                                                                        tableOutput("table_client"))
                                                                  )
                                                                  ),#Cliente
                                      shinydashboard::tabItem(tabName = "data",
                                                              fluidRow(
                                                                shinydashboard::box(width = 12,
                                                                                    dataTableOutput("table"))
                                                              )
                                                              )#Datos
                                        )#tabItems
                                      )#dashBody
                                    )#Page
