
# Paquete -----------------------------------------------------------------

library(tidyverse)


# Datos -------------------------------------------------------------------
path_segmento <- here::here("data", "segmentos_rfm.csv")
segmentos <- readr::read_tsv(path_segmento)

path_tienda <- here::here("data", "Supertienda.xlsx")
Supertienda <- readxl::read_xlsx(path_tienda)

colnames(Supertienda) <- c("Id_fila","Id_pedido", "Fecha_pedido","Fecha_envio","Forma_envio","Id_cliente","Nombre_cliente","Segmento","Ciudad","Estado","Pais","Region","Id_producto","Categoria","Subcategoria","Nombre_producto","Total","Cantidad","Descuento","Ganancia")


# Análisis RFM ------------------------------------------------------------

#Recency

ultday <- lubridate::ymd("2020-01-01")


Recency <- Supertienda %>% group_by(Id_cliente) %>% 
  summarise(Ultimo_dia=max(Fecha_pedido))

Recency$Ultimo_dia <- lubridate::ymd(Recency$Ultimo_dia)

Recency <- Recency %>% mutate(Reciente=ultday- Recency$Ultimo_dia) 

Recency$Reciente <- as.numeric(Recency$Reciente)

class(Recency$Reciente)

colnames(Recency) <- c("Id_cliente", "Ultimo_dia", "Reciente")

#Frequency

Frequency <- Supertienda %>% select(Id_cliente,Id_pedido)


Frequency <- Frequency %>% group_by(Id_cliente, Id_pedido) %>% 
  count(Id_pedido)

Frequency <- Frequency %>% group_by(Id_cliente) %>% 
  count() %>% 
  rename(Frequencia=n)

#Monetary

Monetary <- Supertienda %>% group_by(Id_cliente) %>% 
  summarise(Moneto=sum(Total))



#Data Frame

RFM <- left_join(Recency, Frequency, by="Id_cliente")
RFM <- left_join(RFM, Monetary, by="Id_cliente")
RFM <- RFM %>% select(-Ultimo_dia)

#Asignar score
r_labels = 5:1
m_labels = 1:5
f_labels = 1:5

p=seq(0, 1, by = 0.20)

RFM$score_r= cut(RFM$Reciente, 
                      breaks=c(quantile(RFM$Reciente, probs =p) ), 
                      labels=r_labels, 
                      include.lowest=TRUE) %>% as.integer()


RFM$score_f= cut(RFM$Frequencia, 
                      breaks=c(quantile(RFM$Frequencia, probs =p) ), 
                      labels=f_labels, 
                      include.lowest=TRUE)%>% as.integer()

RFM$score_m= cut(RFM$Monto, 
                      breaks=c(quantile(RFM$Monto, probs =p) ), 
                      labels=m_labels, 
                      include.lowest=TRUE)%>% as.integer()

RFM <- RFM %>% mutate(score_r=case_when(
  score_r==1 ~ 5,
  score_r==2 ~ 4,
  score_r==3 ~ 3,
  score_r==4 ~ 2,
  score_r==5 ~ 1))

RFM <- RFM %>% mutate(rfm=paste0(score_r,score_f,score_m))

RFM$rfm <- as.numeric(RFM$rfm)

rfm_clientes <- left_join(RFM, segmentos, by=c("rfm"="rfm_score"))

readr::write_csv(rfm_clientes,here::here("data", "rfm_clientes.csv"))


# Unificar tablas------------------------------------------------------------------

Supertienda_rfm <- rfm_clientes %>% select(-Reciente,-Frequencia, -Monto, -score_r, -score_f, -score_m) %>% 
  right_join(Supertienda, by="Id_cliente")



# Intervalos --------------------------------------------------------------

unique(Supertienda_rfm$Fecha_pedido)
min(Supertienda_rfm$Fecha_pedido)


Supertienda_rfm<- Supertienda_rfm %>% mutate(Semestre=case_when(
  lubridate::ymd(Fecha_pedido) %in% (lubridate::ymd("2016-01-01"):lubridate::ymd("2016-06-30"))   ~ "2016_S1",
  lubridate::ymd(Fecha_pedido) %in% (lubridate::ymd("2016-07-01"):lubridate::ymd("2016-12-31")) ~ "2016_S2",
  lubridate::ymd(Fecha_pedido)%in% (lubridate::ymd("2017-01-01"):lubridate::ymd("2017-06-30")) ~ "2017_S1",
  lubridate::ymd(Fecha_pedido) %in% (lubridate::ymd("2017-07-01"):lubridate::ymd("2017-12-31")) ~ "2017_S2",
  lubridate::ymd(Fecha_pedido) %in% (lubridate::ymd("2018-01-01"):lubridate::ymd("2018-06-30")) ~ "2018_S1",
  lubridate::ymd(Fecha_pedido) %in% (lubridate::ymd("2018-07-01"):lubridate::ymd("2018-12-31"))~ "2018_S2",
  lubridate::ymd(Fecha_pedido)  %in% (lubridate::ymd("2019-01-01"):lubridate::ymd("2019-06-30"))~ "2019_S1",
  lubridate::ymd(Fecha_pedido)  %in% (lubridate::ymd("2019-07-01"):lubridate::ymd("2019-12-31"))~ "2019_S2"))


Supertienda_rfm <- Supertienda_rfm %>% mutate(Estado=case_when(Estado=="Ciudad Autónoma de Buenos Aires" ~ "CABA",
                                            Estado=="Provincia de Buenos Aires" ~ "Buenos Aires",
                                            T ~ as.character(Estado)))

readr::write_csv(Supertienda_rfm, here::here("data", "Supertienda_rfm.csv"))


# País y segmentos --------------------------------------------------------

estados <- Supertienda_rfm %>%
  group_by(Estado, Pais) %>% 
    summarise(Total_ganancias=sum(Ganancia),
            Total_clientes=length(unique(Id_cliente)))

estados$Total_ganancias <- as.numeric(estados$Total_ganancias)
estados$Total_clientes <- as.numeric(estados$Total_clientes)

readr::write_csv(estados, here::here("data", "estados.csv"))





