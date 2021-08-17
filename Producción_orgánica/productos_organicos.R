path <- here::here("data", "padron-de-operadores-organicos-certificados.csv")
df <- read.csv(path,
               header = T,
               sep = ",")



# Paquetes ----------------------------------------------------------------

require(tidyverse)

library(ggthemes)

library(treemapify)

require(tidytext)


# Selección de df ---------------------------------------------------------

df_salta <- df %>% filter(provincia=="SALTA") %>% 
  select(-provincia_id, -pais, -pais_id, -categoria_id, -Certificadora_id)

df_salta <- df_salta %>% mutate(departamento=case_when(departamento=="SALTA CAPITAL" ~ "SALTA",
                                                       T~ as.character(departamento)))

df_salta <- df_salta %>% mutate(rubro=abbreviate(rubro, 37))

df_salta <- df_salta %>% mutate(rubro=case_when(rubro=="" ~ "SIN DEFINIR",
                                                rubro=="PROCESAMIENTODECEREALESYOLEAGINOSAS." ~ "PROCESAMIENTO CEREALES Y OLEAGINOSAS",
                                                rubro=="BODEGAVITIVINICOLA.ELABORACIONDEMOCDU" ~ "BODEGA VITIVINICOLA",
                                                       T~ as.character(rubro)))


# Análisis ----------------------------------------------------------------

df_salta %>% group_by(departamento) %>% 
  count() %>% 
  ggplot(aes(x=departamento, y=n))+
  geom_col(fill="#41ae76")+
  theme_calc()+
  theme(plot.background = element_rect(size = 1, color = 1),
        axis.text.x = element_text(angle = 90, size = rel(0.8)),
        plot.title = element_text(size = rel(0.9), face = "bold"),
        plot.subtitle = element_text(size = rel(0.9), face = "bold"))+
  labs(title = "Establecimientos con certificación de producción orgánica",
       subtitle = "por departamentos en provincia de Salta",
       x="",
       y="",
       caption = "Agro Analytics")

df_salta %>% group_by(departamento, categoria_desc) %>% 
  filter(departamento!="GENERAL SAN MARTIN") %>% 
  count(departamento) %>% 
  ggplot(aes(x=departamento, y=n, fill=categoria_desc))+
  geom_col()+
  scale_fill_brewer(type = "qual", palette = "Set2")+
  theme_calc()+
  theme(plot.background = element_rect(size = 1, color = 1),
        legend.background = element_rect(size = 0.3, color = 1),
        legend.text = element_text(size = rel(0.6)),
        legend.title = element_text(size = rel(0.7)),
        axis.text.x = element_text(size = rel(0.8), angle = 90),
        plot.title = element_text(size = rel(0.8), face = "bold"),
        plot.subtitle = element_text(size = rel(0.8), face = "bold"))+
  labs(title = "Número de establecimientos con certificación de producción ",
       subtitle = "orgánica y por categoría en provincia de Salta",
       x="",
       y="",
       caption = "Agro Analytics")


df_salta %>% group_by(rubro) %>% 
  count() %>% 
  ggplot(aes(area=n, fill=rubro, label=n))+
  geom_treemap()+
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 8)+
  scale_fill_brewer(type = "qual", palette = "Set3")+
  theme(plot.background = element_rect(size = 1, color = 1),
        legend.position = "right",
        legend.background = element_rect(size = 0.3, color = 1),
        legend.text = element_text(size = rel(0.5)),
        legend.box.margin = margin(t = 40,  # Margen superior
                                   r = 10,  # Margen derecho
                                   b = 40,  # Margen inferior
                                   l = 40),
        
        legend.title = element_text(size = rel(0.8)))+
  labs(title = "Rubros certificados en provincia de Salta",
       caption = "Agro Analytics")


# Productos ---------------------------------------------------------------
noword <- data.frame(tipo_prod=c("salvia", "hispanica", "negro", "y", "l", "monte", "inculto", "descanso", "campo", "caña", "de", "natural", "maiz", "e", "implantados", "naturales"))


df_prod <- df_salta %>% 
  select(productos) %>% 
  unnest_tokens(tipo_prod, productos, drop = F)

df_prod <- df_prod %>% anti_join(noword, by="tipo_prod")

df_prod_salta <- left_join(df_salta, df_prod, by="productos")

df_prod_salta <- df_prod_salta %>% mutate(tipo_prod=case_when(tipo_prod=="vid" ~ "uvas",
                                             T ~ as.character(tipo_prod)))
ggplot(df_prod_salta %>% filter(!is.na(tipo_prod)), aes(x=certificadora_deno, y=tipo_prod))+
  geom_point(aes(color=certificadora_deno), show.legend = F)+
  theme_calc()+
  theme(plot.background = element_rect(size = 1, color = 1),
        axis.text.x = element_text(size = rel(0.9), angle = 90),
        axis.text.y = element_text(size = rel(0.9)),
        plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1), face = "bold"))+
  labs(title = "Productos certificados en Salta",
       subtitle = "según certificadoras",
       x="",
       y="",
       caption = "Agro Analytics")
