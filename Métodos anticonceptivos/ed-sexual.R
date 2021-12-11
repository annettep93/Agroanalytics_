
# DATOS -------------------------------------------------------------------


ac_2 <- read.csv(here::here("DATA", "cantidad-metodos-anticonceptivos-insumos-distribuidos-enia-segundo-trimestre-2021-1_1.csv"),
                 header = T,
                 sep = ";")

ac_1 <- read.csv(here::here("DATA", "cantidad-metodos-anticonceptivos-insumos-distribuidos-pnssr-primer-trimestre-2021-1_1.csv"),
                 header = T,
                 sep = ";")

ed_sex <- readxl::read_xlsx(here::here("DATA", "capacitacion-salud-sexual-reproductiva-dnssr-primer-trimestre-y-segundo-trimestre-2021.xlsx"))

natalidad <- read.csv(here::here("DATA", "nacidos-vivos-registrados-en-la-republica-argentina-entre-los-anos-2005-2019.csv"),
                      header = T,
                      sep = ",")

desc <- read.csv(here::here("DATA", "tratamientos-embarazo.csv"),
                 header = T,
                 encoding = "UTF-8",
                 sep = ";")

pobl <- read.csv(here::here("DATA", "poblacion_sex.csv"),
                 header = T,
                 encoding = "UTF-8",
                 sep = ";")

# PAQUETES ----------------------------------------------------------------

install.packages("treemapify")
install.packages("ggalluvial")
require(tidyverse)
require(treemapify)
library(ggalluvial)
# LIMPIEZA ----------------------------------------------------------------

colnames(ac_1)

colnames(ac_1) <- c("jurisdiccion_id","jurisdicion_nombre",                                                   
                    "Etinilestradiol+levonorgestrel(comp:0.03-0.15mg)",
                    "Etinilestradiol+gestodeno(comp:0.02-0.075mg)",    
                    "Levonorgestrel(comp:0.03mg)",                     
                    "Desogestrel(comp:0.075mg)",                       
                    "Noretisterona+estradiol(amp:50-5mg)",             
                    "Medroxiprogesterona(amp:150mg/ml)",               
                    "DIUT(cobre_380mm)",                                   
                    "DIUM(385mm)",                                         
                    "Levonorgestrel(52mg)",                                
                    "Etonogestrel(68mg)",                                  
                    "Condon_latex",                                       
                    "Levonorgestrel(comp:1.5mg)",                      
                    "Misoprostol(200mcg)",                             
                    "Test_embarazo",                                      
                    "trimestre",                                                            
                    "anio",                                                                 
                    "tipo_dato" )

ac1 <- pivot_longer(ac_1, cols = 3:16, names_to = "Tratamiento", values_to = "Cantidad")
ac1 <- ac1 %>% mutate(Tipo=case_when(Tratamiento=="Etinilestradiol+levonorgestrel(comp:0.03-0.15mg)" ~ "Pastilla combinada",
                              Tratamiento=="Etinilestradiol+gestodeno(comp:0.02-0.075mg)" ~ "Pastilla combinada",
                              Tratamiento=="Levonorgestrel(comp:0.03mg)" ~ "Minipildora",
                              Tratamiento=="Desogestrel(comp:0.075mg)" ~ "Minipildora",
                              Tratamiento=="Noretisterona+estradiol(amp:50-5mg)" ~ "Anticonceptivo inyectable",
                              Tratamiento=="Medroxiprogesterona(amp:150mg/ml)" ~ "Anticonceptivo inyectable",
                              Tratamiento=="DIUT(cobre_380mm)" ~ "DIU",
                              Tratamiento=="DIUM(385mm)" ~ "DIU",
                              Tratamiento=="Levonorgestrel(52mg)" ~ "Sist. de liberación intrauterina",
                              Tratamiento=="Etonogestrel(68mg)" ~ "Implante",
                              Tratamiento=="Condon_latex" ~ "Preservativo",
                              Tratamiento=="Levonorgestrel(comp:1.5mg)" ~ "Pastilla del día después",
                              Tratamiento=="Misoprostol(200mcg)" ~ "Tratamiento abortivo",
                              Tratamiento=="Test_embarazo" ~ "Test de embarazo"))


colnames(ac_2) <- c("jurisdiccion_id","jurisdicion_nombre",
                    "DIUT(cobre_380mm)","Etonogestrel(68mg)",
                    "trimestre","anio",                           
                    "tipo_dato"  )
ac_2 <- ac_2 %>% select(-`DIUT(cobre_380mm)`)
ac2 <- pivot_longer(ac_2, cols = 3, names_to = "Tratamiento", values_to = "Cantidad")
ac2 <- ac2 %>%  mutate(Tipo=case_when(Tratamiento=="Etonogestrel(68mg)" ~ "Implante"))

ac <- bind_rows(ac1, ac2)




colnames(desc) <- c("Tratamientos", "Descripción")

colnames(pobl) <- c("jurisdicion_nombre", "Personas")
pobl$Personas <- as.numeric(pobl$Personas)

ac <- inner_join(ac, pobl, by="jurisdicion_nombre")
ac <- ac %>% mutate(Relacion_pobl=ac$Cantidad/ac$Personas)

ac <- ac %>% mutate(jurisdicion_nombre=case_when(jurisdicion_nombre=="Ciudad Autónoma de Buenos Aires" ~ "CABA",
                                   T ~ as.character(jurisdicion_nombre)))


write.csv(ac, here::here("DATA", "tratamientos_ac.csv"))

gama <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9",
          "#bc80bd","#ccebc5","#ffed6f","#ff7f00","#1f78b4")


#Natalidad

nac <- natalidad %>% mutate(edad_madre_grupo=substr(natalidad$edad_madre_grupo, 3,str_length(natalidad$edad_madre_grupo)),
                     semana_gestacion=substr(natalidad$semana_gestacion, 3, 13),
                     educacion=substr(natalidad$instruccion_madre, 3, str_length(natalidad$instruccion_madre)))


nac <- nac %>% select(-jurisdiccion_de_residencia_id, -tipo_de_parto_id, -tipo_de_parto_nombre, -intervalo_peso_al_nacer, -sexo_id)
colnames(nac) <- c("anio", "jurisdicion_nombre", "edad_madre", "educacion_", "semana_gestacion", "sexo", "nacimientos_cantidad", "educacion")
nac <- nac %>% select(-educacion_)

nac <- nac %>% filter(anio==2019 & edad_madre!="Sin especificar" & educacion!="Sin especificar")

nac <- nac %>% mutate(jurisdicion_nombre=case_when(jurisdicion_nombre=="Ciudad Autónoma de Buenos Aires" ~ "CABA",
                                           T ~ as.character(jurisdicion_nombre)))

class(nac$anio)
write.csv(nac, here::here("DATA", "nacimientos.csv"))
# PREGUNTAS ---------------------------------------------------------------
ac$Cantidad <- as.numeric(ac$Cantidad)

#MÉTODO MÁS UTILIZADO


ac %>% group_by(Tipo) %>% 
  summarise(Total=sum(Cantidad)) %>% 
  arrange(desc(Total))

ac %>% ggplot(aes(x=Cantidad, y=jurisdicion_nombre))+
  geom_col(aes(fill=Tipo), position = "fill")+
  scale_fill_manual(values = gama)+
  ggthemes::theme_gdocs()+
  labs(title = "Tratamientos utilizados por provincia",
       y="",
       x="")+
  theme(axis.text.x = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.background = element_rect(colour = "grey", size = 0.5),
        title = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.8)))

ac %>% filter(jurisdicion_nombre=="Salta") %>% 
  ggplot(aes(area=Cantidad, fill=Tipo, label=Cantidad))+
  geom_treemap()+
  geom_treemap_text(colour="white",
                    place= "bottom",
                    size=8)+
  ggthemes::theme_gdocs() +
  scale_fill_manual(values = c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9",
                    "#bc80bd","#ccebc5","#ffed6f","#ff7f00","#1f78b4"))+
  labs(title = "Cantidad de tratamientos anticonceptivos",
       subtitle = "Año 2021 ")+
  theme(legend.text = element_text(size = rel(0.6)),
        legend.title = element_text(size = rel(0.7)),
        legend.background = element_rect(colour = "grey", size = 0.5),
        axis.title = element_text( face = "bold", size = rel(0.8)))


ggplot(ac, aes(x=Tratamiento, y=Cantidad))+
  geom_boxplot()+
  coord_flip()


#Cómo se distribuye a nivel país el condon (los distintos tratamientos)

ac %>% filter(Tratamiento=="Condon_latex") %>% 
  group_by(jurisdicion_nombre) %>% 
  summarise(Total=sum(Cantidad)) %>% 
  arrange(desc(Total))


ac %>% filter(Tratamiento=="Condon_latex") %>% 
  group_by(jurisdicion_nombre) %>% 
  summarise(Total=sum(Cantidad)) %>% 
  arrange(desc(Total)) %>% 
ggplot(aes(y = Total, x =reorder(jurisdicion_nombre, -Total))) +
  geom_segment(aes(y = 0, yend = Total, x =reorder(jurisdicion_nombre, -Total), xend =reorder(jurisdicion_nombre, -Total)),
               color = "gray", lwd = 1.5) +
  geom_point(size = 4, pch = 21, bg = 6, col = 1) +
  geom_text(aes(label=Total), size=2.5, color="black", hjust=-0.3)+
  coord_flip()+
  ggthemes::theme_gdocs()+
  theme(axis.text.x = element_text(size = rel(0.5)),
        axis.text.y = element_text(size = rel(0.7)),
        axis.title = element_text(size = rel(0.9)))+
  labs(title = "N° de preservativos ",
       subtitle = "entregados por provincias (Año 2021)",
       y="",
       x="")


#Donde se usa mas el misoprostol

ac %>% filter(Tratamiento=="Misoprostol(200mcg)") %>% 
  group_by(jurisdicion_nombre) %>% 
  summarise(Total=sum(Cantidad)) %>% 
  arrange(desc(Total))

ac %>% filter(Tratamiento=="Misoprostol(200mcg)") %>% 
  group_by(jurisdicion_nombre) %>% 
  summarise(Total=max(Relacion_pobl)) %>% 
  arrange(desc(Total))

#Y la Levonogestol 1.5mg

ac %>% filter(Tratamiento=="Levonorgestrel(comp:1.5mg)") %>% 
  group_by(jurisdicion_nombre) %>% 
  summarise(Total=sum(Cantidad)) %>% 
  arrange(desc(Total))

ac %>% filter(Tratamiento=="Levonorgestrel(comp:1.5mg)") %>% 
  group_by(jurisdicion_nombre) %>% 
  summarise(Total=max(Relacion_pobl)) %>% 
  arrange(desc(Total))

#PROVINICA DE SALTA


ac %>% filter(jurisdicion_nombre=="Salta") %>% 
  group_by(Tratamiento) %>% 
  summarise(Total=sum(Cantidad)) %>% 
  arrange(desc(Total))



#Los tres principales

ac %>% filter(jurisdicion_nombre=="Salta") %>% 
  group_by(Tratamiento) %>% 
  summarise(Total=sum(Cantidad)) %>% 
  arrange(desc(Total)) %>% 
  top_n(3, Total)

media_condon <- ac %>% filter(Tratamiento=="Condon_latex") %>% 
  summarise (Media=mean(Cantidad)) %>% pull()

r_condon <- ac %>% filter(Tratamiento=="Condon_latex") %>% 
  summarise (Media=mean(Relacion_pobl)) %>% pull()


media_eti_lev <- ac %>% filter(Tratamiento=="Etinilestradiol+levonorgestrel(comp:0.03-0.15mg)") %>% 
  summarise (Media=mean(Cantidad)) %>% pull()

r_media_eti_lev <- ac %>% filter(Tratamiento=="Etinilestradiol+levonorgestrel(comp:0.03-0.15mg)") %>% 
  summarise (Media=mean(Relacion_pobl)) %>% pull()


media_n_e <- ac %>% filter(Tratamiento=="Noretisterona+estradiol(amp:50-5mg)") %>% 
  summarise (Media=mean(Cantidad)) %>% pull()

r_media_n_e <- ac %>% filter(Tratamiento=="Noretisterona+estradiol(amp:50-5mg)") %>% 
  summarise (Media=mean(Relacion_pobl)) %>% pull()

ac %>% filter(jurisdicion_nombre=="Salta") %>% 
  group_by(Tratamiento) %>% 
  summarise(Total=sum(Cantidad)) %>% 
  arrange(desc(Total)) %>% 
  top_n(3, Total) %>% 
  ggplot(aes(y=Tratamiento, x=Total))+
  geom_col(aes(fill=Tratamiento), show.legend = F)+
  scale_fill_manual(values = c("#8dd3c7", "#fdb462", "#b3de69"))+
  geom_vline(xintercept = media_eti_lev, color="#fdb462")+
  geom_vline(xintercept = media_condon, color="#8dd3c7")+
  geom_vline(xintercept = media_n_e, color="#b3de69")+
  ggthemes::theme_gdocs()+
  labs(title = "Principales tratamientos en Salta",
       y="",
       caption= "*Las rectas representan la media nacional")+
  theme(axis.text.y = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        title = element_text(size = rel(0.9)))
  

ac %>% filter(jurisdicion_nombre=="Salta") %>% 
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
  labs(title = "Principales tratamientos en Salta",
       y="",
       caption= "*Las rectas representan la media nacional")+
  theme(axis.text.y = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        title = element_text(size = rel(0.9)))

#RELACIÓN CON POBLACIÓN SEXUALMENTE ACTIVA

ac %>% group_by(jurisdicion_nombre, Tratamiento) %>% 
  summarise(Promedio=mean(Relacion_pobl))


ac %>% group_by(jurisdicion_nombre, Tipo)%>% 
  summarise(Promedio=mean(Relacion_pobl)) %>% 
  ggplot(aes(x=jurisdicion_nombre, y=Promedio))+
  geom_point(aes(color=Tipo))+
  coord_flip()+
  ggthemes::theme_gdocs()+
  labs(title = "Tratamientos en función de habitantes sexualmente activos ",
       y="Relación promedio",
       x="")+
  scale_color_manual(values = gama)+
  theme(axis.text.x = element_text(size = rel(0.8), angle=90),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.background = element_rect(colour = "grey", size = 0.5),
        title = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.8)))

#NATALIDAD
nac$nacimientos_cantidad <- as.numeric(nac$nacimientos_cantidad)

#Qué rango etareoo presenta mayores nacimientos?

nac %>% 
ggplot( aes(x=edad_madre, y=nacimientos_cantidad))+
  geom_col(fill="#fdb462")+
  ggthemes::theme_gdocs()+
  scale_y_continuous(name = "Total de nacimientos", breaks = c(250000,500000,1000000,1250000,1500000,2000000))+
  labs(y="Total de nacimientos",
       x="Rango etario")+
  theme(axis.text.x = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.8)))

#Y según nivel de enseñanza?

nac %>%
ggplot(aes(x=educacion, y=nacimientos_cantidad))+
  geom_col(fill="#fdb462")+
  ggthemes::theme_gdocs()+
  coord_flip()+
  scale_y_continuous(name = "Total de nacimientos", breaks = c(250000,500000,1000000,1250000,1500000,2000000))+
  labs(title = "Total de nacimientos según nivel educativo",
       subtitle = "Año 2019",
       y="Total de nacimientos",
       x="")+
  theme(axis.text.x = element_text(size = rel(0.7)),
        title = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.8)))

#Cuál es la relación entre ambas?

nac %>% filter(jurisdicion_nombre=="Salta") %>% 
  group_by(edad_madre, educacion) %>% 
  summarise(Total=sum(nacimientos_cantidad)) %>% 
ggplot(aes(axis1 = edad_madre, axis2 = educacion, y = Total)) +
  geom_alluvium(aes(fill = educacion), show.legend = F) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size=2) +
theme_void()+
  labs(title = "Relación edad:nivel educativo")+
  theme(title = element_text(color = "#4d4d4d"))


#Dónde se da más nacimiento en menores de 15?

nac %>% filter(edad_madre=="Menor de 15") %>% 
  group_by(jurisdicion_nombre) %>% 
  summarise(Total=sum(nacimientos_cantidad)) %>% 
  arrange(desc(Total))


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
        text = element_text())


#Cuáles son los principlaes métodos para las primero 5 provincias?

ac %>% filter(jurisdicion_nombre %in% c("Buenos Aires", "Chaco", "Misiones", "Santa Fe", "Salta")) %>% 
  ggplot(aes(x=jurisdicion_nombre, y=Relacion_pobl))+
  geom_col(aes(fill=Tratamiento))+
  ggthemes::theme_gdocs()+
  scale_fill_manual(values = gama)+
  labs(title = "Tratamientos empleados en las principales 5 provincias",
       subtitle = "con mayor número de embarazos en menores de 15 años",
       x="",
       y="Relación n° tratamientos/población sexualmente activa")+
  theme(axis.text.x = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.background = element_rect(colour = "grey", size = 0.5),
        title = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.8)))

