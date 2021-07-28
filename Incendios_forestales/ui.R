
# Base de datos -----------------------------------------------------------
#Incendios en Parques Nacionales
path_inciendios_pn <- here::here("Incendio_forestal", "incendios-cantidad-causas-parques-nacionales.csv")
df_pn <- read.csv(path_inciendios_pn,
                  sep = ";",
                  header = TRUE,
                  encoding = "UTF-8")

#Incendios por provincia
path_incendios_prov <- here::here("Incendio_forestal", "incendios-cantidad-causas-provincia.csv")
df_prov <- read.csv(path_incendios_prov,
                    header = TRUE,
                    sep = ";")


# Limpieza ----------------------------------------------------------------
#Incendios Totales

df_pivot <- pivot_longer(df_pn, cols = c("incendio_total_numero", "incendio_negligencia_numero", "incendio_intencional_numero", "incendio_natural_numero", "incendio_desconocida_numero"), names_to="tipo_incendio", values_to="value") 
df_pivot <- df_pivot %>% mutate(tipo_incendio=stringr::str_remove_all(df_pivot$tipo_incendio, "incendio_"))
df_pivot <- df_pivot %>% mutate(tipo_incendio=stringr::str_remove_all(tipo_incendio,"_numero"))

#Incendios por provincia
columnas <- df_prov %>% select(-incendio_anio, -incendio_provincia) %>% colnames()
df_pivot_prov <- pivot_longer(df_prov, cols =columnas, names_to = "tipo_incendio", values_to = "value" )
df_pivot_prov <-  df_pivot_prov %>% mutate(tipo_incendio=stringr::str_remove_all(df_pivot_prov$tipo_incendio, "incendio_"),
                                           tipo_incendio=stringr::str_remove_all(tipo_incendio,"_numero"))
df_pivot_prov <-  df_pivot_prov %>% mutate(incendio_provincia=stringr::str_replace_all(incendio_provincia, "Ciudad Autónoma de Buenos Aires", "CABA"))

periodo <- unique(df_pivot_prov$incendio_anio)
provincia <- unique(df_pivot_prov$incendio_provincia)


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Incendios forestales"),
                    dashboardSidebar(
                      sidebarMenu(id="sidebar",
                                  menuItem("Introducción",tabName = "intro", icon = icon("fas fa-sticky-note")),
                                  menuItem("Argentina", tabName = "arg", icon = icon("fas fa-fire")),
                                  conditionalPanel('input.sidebar == "arg"',
                                                   selectInput(inputId = 'periodo', 
                                                               label = 'Seleccione un período: ',
                                                               choices = periodo,
                                                               selected = 2019,
                                                               multiple = FALSE,
                                                               selectize = FALSE),
                                                   selectInput(inputId = "prov",
                                                               label = "Seleccione una provincia: ",
                                                               choices = provincia,
                                                               selected = "Salta",
                                                               multiple = FALSE)),
                                  menuItem("Datos", tabName = "dato", icon = icon("fas fa-database")))
                    ),
                    dashboardBody(
                      tabItems(
                        #Intro
                        tabItem(tabName = "intro",
                                fluidRow(uiOutput("definicion"),
                                         uiOutput("causas"),
                                         uiOutput("prevencion"))),
                        #Arg
                        tabItem(tabName = "arg",
                                fluidRow(
                                  valueBoxOutput("total_i",
                                                 width = 12),
                                  valueBoxOutput("desc_i",
                                                 width = 3),
                                  valueBoxOutput("negligencia_i",
                                                 width = 3),
                                  valueBoxOutput("intencional_i",
                                                 width = 3),
                                  valueBoxOutput("natural_i",
                                                 width = 3)
                                ),
                                fluidRow(
                                  shinydashboard::box(width = 6,
                                                      plotlyOutput('ev_incendios')),
                                  shinydashboard::box(width = 6,
                                                      plotOutput('heatmap_incendios')),
                                  shinydashboard::box(width = 6,
                                                      plotlyOutput('box_tipo')),
                                  shinydashboard::box(width = 6,
                                                      plotlyOutput('ev_prov'))
                                )),
                        #Datos
                        tabItem(tabName = "dato",
                                fluidRow(
                                  uiOutput("data")))
                      )
                    )
)

