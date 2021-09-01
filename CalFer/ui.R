# Paquetes ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)


# Base de Datos -----------------------------------------------------------


#http://www.ipni.net/publication/ia-lacs.nsf/0/D0F05E377CB382B68525799500757379/$FILE/21.pdf
path_extraccion <- here::here("extraccion_nutrientes.csv")
df_extraccion <- read.csv(path_extraccion,
                          header = T,
                          sep = ";",
                          encoding = "UTF-8")

path_ej <- here::here("ejemplo_fertilizantes.csv")
df_ej <- read.csv(path_ej,
                  header = T,
                  sep = ";",
                  encoding = "UTF-8")

unique(df_ej$X.U.FEFF.Fertilizantes)
# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Nutrientes del Suelo"),
                    dashboardSidebar(sidebarMenu(id="sidebar",
                                                
                                                 menuItem("Fertilizantes", tabName = "fert", icon = icon("fas fa-plus-square")),
                                                 conditionalPanel("input.sidebar=='fert'",
                                                                  radioButtons(inputId = "ej",
                                                                               label ="Seleccione un tipo de fertilizante",
                                                                               choices = c("Nitrogenados","Fosforados sólidos","Potásicos","Azufrados"),
                                                                               selected = "Nitrogenados"
                                                                               ))
                                                 )),#dashboardSidebar
                    dashboardBody(tabItems(
                      
                                
                      #Fertilizante
                      tabItem(tabName = "fert",
                              
                              fluidRow(
                                shinydashboard::box(width = 12,
                                                    background ="yellow" ,
                                                    solidHeader = T,
                                                    uiOutput("title"))),
                              fluidRow(
                                shinydashboard::box(width = 6,
                                                    background = "teal",
                                                    selectInput(inputId = "nomb",
                                                                label = "Elija un fertilizante",
                                                                choices = unique(df_ej$Ejemplos),
                                                                selected = "Nitro doble")),
                                
                                shinydashboard::box(width = 6,
                                                    background = "teal",
                                                    textInput(inputId = "num",
                                                              label = "Cantidad de nutriente principal a incorporar (kg/ha)",
                                                              value = "0"))
                              ),
                              fluidRow(
                                shinydashboard::box(width = 6,
                                                    textOutput("nppal")),
                                shinydashboard::box(width = 6,
                                                    background = "yellow",
                                                    solidHeader = T,
                                                    textOutput("resultado"))),
                              fluidRow(
                                shinydashboard::box(width = 6,
                                                    textOutput("n1")),
                                shinydashboard::box(width = 6,
                                                    textOutput("adicional1"))),
                              fluidRow(
                                shinydashboard::box(width = 6,
                                                    textOutput("n2")),
                                shinydashboard::box(width = 6,
                                                    textOutput("adicional2"))
                                
                              ),
                              
                              fluidRow(
                                shinydashboard::box(width = 6,
                                                    textOutput("n3")),
                                shinydashboard::box(width = 6,
                                                    textOutput("adicional3"))),
                                fluidRow(
                                  shinydashboard::box(width = 12,
                                                      tableOutput("ejemplo"))
                                  
                                ),
                                fluidRow(
                                  shinydashboard::box(width = 12,
                                                      textOutput("desc")),
                                  shinydashboard::box(width = 12,
                                                      tableOutput("extraccion"))
                                )
                              ))#fertilizante
                     
                    ))#dashboardBody 
#dashboardPage


