
# Paquetes ----------------------------------------------------------------

require(tidyverse)
require(rvest)



# Base de Datos -----------------------------------------------------------

#Establecimientos vigentes
path_estab_2018 <- here::here("data/INASE", "estableciemientos-vigentes-inscriptos-rncyfs-2018-septiembre.csv")

df_establ18 <- read.csv(path_estab_2018,
         header = T,
         sep = ",",
         encoding = "UTF-8")

path_estab_2017 <- here::here("data/INASE", "estableciemientos-vigentes-inscriptos-rncyfs-2017-diciembre.csv")

df_establ17 <- read.csv(path_estab_2017,
                        header = T,
                        sep = ",",
                        encoding = "UTF-8")

#Producción Nacional (año 2018)
path_prod <- here::here("data/INASE", "produccionnacionalporespecie2018-09-28.csv")

df_prod <- read.csv(path_prod,
                       header = T,
                       sep = ",",
                       encoding = "UTF-8")

#Producción por provincia (2018)
path_prod_prov <- here::here("data/INASE", "produccion-nacional-especie-provincia-y-localidad-2017-2018-.csv")

df_prod_prov <- read.csv(path_prod_prov,
                         header = T,
                         sep = ",",
                         encoding = "UTF-8")

#Superficie productiva
path_supf <- here::here("data/INASE", "superficie-nacional-fiscalizada-por-especie-y-ubicacion-inase-2017-2018-.csv")

df_supf <- read.csv(path_supf,
                    header = T,
                    sep = ",",
                    encoding = "UTF-8")


# Establecimientos --------------------------------------------------------

df_establ17$periodo <- "2017"
df_establ18$periodo <- "2018"

colnames(df_establ17) <- colnames(df_establ18)

establecimientos <- rbind(df_establ17,df_establ18)

establecimientos <- establecimientos %>% mutate(provincia=case_when(provincia=="SGO. DEL ESTERO" ~ "SGO.DEL ESTERO",
                                                T ~ as.character(provincia)))

#Se incrementaron los establecimientos?

establecimientos %>% group_by(periodo) %>% count()

#En qué localidad

establecimientos %>% group_by(periodo, provincia) %>% 
  count() %>% 
  ggplot(aes(y=provincia, x=n))+
  geom_col(aes(fill=periodo), position = "dodge")+
  scale_fill_brewer(type = "qual", palette = "Set2")+
  theme_classic()+
  labs(title = "Establecimientos productivos",
       subtitle = "por provincias",
       x="",
       y="",
       caption = "Agro Analytics")+
  theme(plot.background = element_rect(color=1, size = 1),
        axis.text.x = element_text(size = rel(0.9)),
        axis.text.y = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.7)))


# Producción Nacional -----------------------------------------------------
#Los principlaes 30

df_prod %>% arrange(desc(Unidades)) %>% 
  top_n(30, Unidades) 



# Producción por provincias -----------------------------------------------

#Producción por provincia 

df_prod_prov %>% group_by(provincia) %>% 
  summarise(total=sum(Unidades)) %>% 
  arrange(desc(total))
        
#Prov de Salta
df_prod_prov %>% filter(provincia=="SALTA") %>% 
  ggplot(aes(y=localidad, x=nombreVulgar, size=Unidades/1000))+
  geom_point(color="#fdb462", shape=17, show.legend = F)+
  geom_text(aes(label=Unidades, size=rel(1)))+
  theme_classic()+
  theme(plot.background = element_rect(color=1, size = 1),
        axis.text.x = element_text(angle = 90, size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.7)))+
  labs(title = "Producción de semillas",
       subtitle = "en provincia de Salta",
       x="",
       y="",
       caption = "Agro Anlaytics")

#Prov de Santa Fe

df_prod_prov %>% filter(provincia=="SANTA FE") %>% 
  ggplot(aes(y=localidad, x=nombreVulgar, size=Unidades/1000))+
  geom_point(color="#fdb462", shape=17, show.legend = F)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.7)))+
  labs(title = "Producción de semillas",
       subtitle = "en provincia de Santa Fe",
       x="",
       y="",
       caption = "Agro Anlaytics")

#Prov de Entre Ríos
df_prod_prov %>% filter(provincia=="ENTRE RIOS") %>% 
  ggplot(aes(y=localidad, x=nombreVulgar, size=Unidades/1000))+
  geom_point(color="#fdb462", shape=17, show.legend = F)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.7)))+
  labs(title = "Producción de semillas",
       subtitle = "en provincia de Entre Ríos",
       x="",
       y="",
       caption = "Agro Anlaytics")

#Prov de Chaco
df_prod_prov %>% filter(provincia=="CHACO") %>% 
  ggplot(aes(y=localidad, x=nombreVulgar, size=Unidades/1000))+
  geom_point(color="#fdb462", shape=17, show.legend = F)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.7)))+
  labs(title = "Producción de semillas",
       subtitle = "en provincia de Chaco",
       x="",
       y="",
       caption = "Agro Anlaytics")

#Prov de Bs As y Córdoba
df_prod_prov %>% filter(provincia==c("CORDOBA","BUENOS AIRES")) %>%
  select(provincia, localidad,nombreVulgar,Unidades)
 
#Cuánto se produce de cada especie
df_prod_prov %>% group_by(nombreVulgar) %>% 
  summarise(total=sum(Unidades)) %>% 
  arrange(desc(total))

#El top 10
df_prod_prov %>% group_by(nombreVulgar) %>% 
  summarise(total=sum(Unidades)) %>% 
  arrange(desc(total)) %>% top_n(10, total) %>% 
  ggplot(aes(y=nombreVulgar, x=total))+
  geom_col(aes(fill=nombreVulgar))+
  coord_polar()+
  theme_minimal()+
  scale_fill_brewer(type = "qual", palette = "Set3")+
  theme(legend.position = "right",
        legend.text = element_text(size = rel(0.5)),
        legend.title = element_text(size = rel(0.5)),
        axis.text.y = element_text(colour = "white", size = rel(0.1)))+
  labs(title = "Principales semillas producidas",
       x="",
       y="",
       caption = "Agro Analytics")


# Superficie --------------------------------------------------------------
df_supf <- df_supf %>% mutate(superficie=case_when(is.na(superficie) ~ "0",
                                        TRUE ~ as.character(superficie)))
df_supf <- df_supf %>% mutate(superficie=as.numeric(superficie))

ggplot(df_supf %>% filter(superficie<5000), aes(x=superficie))+
  geom_histogram(aes(fill=nom_provincia), stat = "bin", bins=30, na.rm = T)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = rel(0.5)),
        legend.title = element_text(size = rel(0.6)),
        legend.justification = "right",
        axis.title.x= element_text(size = rel(0.9)))+
  labs(title="Superficie de producción",
       subtitle = "por provincias",
       x="Hectáreas",
       y="",
       caption = "Agro Analytics")


#Las provincias con mayor superficie
df_supf %>% group_by(nom_provincia) %>% 
  summarise(total=sum(superficie)) %>% 
  arrange(desc(total))

#Las principales 10
df_supf %>% group_by(nom_provincia) %>% 
  summarise(total=sum(superficie)) %>% 
  arrange(desc(total)) %>% 
  top_n(10, total)

#Qué semillas producen los top10

top_10_supf <- df_supf %>% group_by(nom_provincia) %>% 
  summarise(total=sum(superficie)) %>% 
  arrange(desc(total)) %>% 
  top_n(10, total) %>% 
  select(nom_provincia) %>% pull()

df_prod_prov %>% dplyr::filter(provincia==top_10_supf) %>% 
  select(provincia, nombreVulgar, Unidades) %>% 
  ggplot(aes(x=Unidades/1000, y=nombreVulgar))+
  geom_point(aes(color=provincia))+
  scale_color_brewer(type = "qual", palette = "Set3")+
  theme_minimal()+
  ggtitle("Especies porducidas por las principales provincias",
          subtitle = "con mayor superficie productiva")+
  theme(legend.position = "right",
        legend.justification = "right",
        legend.text = element_text(size = rel(0.6)),
        legend.title = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.9)),
        axis.title.x = element_text(size = rel(0.9)),
        axis.text.x = element_text(size = rel(0.9), angle = 60),
        plot.title =  element_text(hjust = 0.5),
        plot.subtitle = element_text(face = "bold", hjust = 0.5))+
  labs(x="Toneladas",
       y="",
       caption = "Agro Analytics")
  

# INASE -------------------------------------------------------------------
path_inase <- "https://www.argentina.gob.ar/inase/certificacionsemillas"

certificacion <- read_html(path_inase, encoding = "UTF-8")

#Qué es la certificación
cc_inase <- certificacion %>% html_nodes("p")

cc_inase[2:4]

path_sp_fiscal <- "https://www.argentina.gob.ar/inase/certificacionsemillas/que-es-la-certificacion-nacional-de-semillas/especies-de-fiscalizacion-obligatoria-e-identificadas"

sp_fiscal <- read_html(path_sp_fiscal, encoding = "UTF-8")

#Cómo se clasifican las especies fiscalizadas

cuadros <- sp_fiscal %>% html_nodes("img")

sp_obigatoria <- cuadros[2]

sp_acreditacion <- cuadros[3]
sp_identificada <- cuadros[4]
sp_nominada <- cuadros[5]


# Tres variedades más producidas ------------------------------------------

top_3 <- df_prod_prov %>% group_by(nombreVulgar) %>% 
  summarise(total=sum(Unidades)) %>% 
  arrange(desc(total)) %>% 
  top_n(3, total) %>% select(nombreVulgar) %>% pull()

#qué porvincias las producen?
df_prod_prov %>% filter(nombreVulgar==top_3) %>% 
  ggplot(aes(x=provincia, y=nombreVulgar))+
  geom_tile(aes(fill=Unidades))+
  scale_fill_gradient(low="#fed976", high = "#e31a1c")+
  theme_classic()+
  theme(plot.background = element_rect(color = 1, size = 1),
        legend.position = "bottom",
        legend.title = element_text(size = rel(0.7)),
        legend.text = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(0.8), angle = 90),
        axis.text.y = element_text(size = rel(0.9)),
        plot.subtitle = element_text(face = "bold", hjust = 0.5)
        )+
  labs(title = "Provincias productoras de",
       subtitle = "las tres principlaes especies",
       x="",
       y="",
       caption = "Agro Analytics")



# Trigo Pan ---------------------------------------------------------------
path_trigo <- here::here("data", "serie-de-tiempo-trigo.csv")
df_trigo <- read.csv(path_trigo,
                     header = T,
                     sep = ",",
                     encoding = "UTF-8")

df_trigo$cultivo="Trigo"
colnames(df_trigo) <- c("indice_tiempo", "supf_sembrada", "supf_cosechada", "produccion_tn", "rendimiento_kgha","cultivo")


# Soja --------------------------------------------------------------------


path_soja <- here::here("data", "soja-anual-1969-2020.csv")
df_soja <- read.csv(path_soja,
                     header = T,
                     sep = ",",
                     encoding = "UTF-8")
df_soja$cultivo="Soja"
colnames(df_soja) <- c("indice_tiempo", "supf_sembrada", "supf_cosechada", "produccion_tn", "rendimiento_kgha","cultivo")


df_ev_cultivos <- rbind(df_trigo,df_soja)


ggplot(df_ev_cultivos,aes(x=indice_tiempo, y=supf_sembrada/1000))+
  geom_area(aes(fill=cultivo))+
  scale_fill_brewer(type = "qual", palette = "Set3")+
  theme_classic()+
  theme(plot.background = element_rect(size = 1, color = 1),
        legend.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        axis.title.y = element_text(size = rel(0.9)))+
  labs(title = "Evolución de superficie sembrada",
       x="",
       y="Mil hectáreas",
       caption = "Agro Analytics")


cant_semillas <- function(superficie, densidad){
  cant_semillas_ha <- superficie*densidad
  
  return(cant_semillas_ha)
}

superficie=10000
densidad_soja=20
densidad_trigo=150
cant_semillas(superficie, densidad_trigo)


df_ev_cultivos %>% group_by(cultivo) %>% 
  summarise(mediana=median(produccion_tn))


168193736/98.76543
157519785/41.97531
