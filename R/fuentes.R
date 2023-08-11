pacman::p_load(tidyselect, tidyverse, dplyr, glue, knitr, stringr, magrittr, 
               sf, ggmap, ggplot2, flextable, data.table, sp, readxl, janitor,
               scales, devtools, tinytex, writexl, webshot, readr)
require(stringr)
require(sf)
setwd("D:/catalogo_datamet_flujo_de_capas")

### FUENTE 1: Read the layers in the GDB ------------------------------------------------------------------------------
gdb<-"catalogo_v4.gdb"
shps1 <-st_layers(glue("{gdb}"))$name
shps1<-sort(shps1)
# There is an extra capa that they have eliminated but which is still in the GDB
shps1<-shps1[-23]  
shps<-shps1 %>% tibble()
names(shps)<-"nombre_capa"
shps<-shps[order(shps$nombre_capa), ]


### FUENTE 2: Read the fichas, which contains all capas and their columns ---------------------------------------------
fichas<-read.csv("data/fichas_capas.csv")

### FUENTE 3: Read the catálogo, which contains ambito, grupo, descriptions, fuentes, calidad, etc. -------------------
cata<- read.csv("data/catalogo_limpio.csv")

## Join them ---
cata_ficha<-left_join(cata,fichas, by="nombre_shp_l")
cata_ficha<-cata_ficha[order(cata_ficha$nombre_capa), ]

### FUENTE 4: Read the etiquetas --------------------------------------------------------------------------------------
etiquetas<- read.csv("data/etiquetas.csv")
combinacion<-left_join(cata_ficha,etiquetas, by="nombre_shp_l")

combinacion<-combinacion[,c(1, 26, 23:25, 22, 2:21,27:56)]
names(combinacion)[4]<-"nombre_shp_fichas"
combinacion<-combinacion[,c(3,5:56, 4,1,2)]

##### EXPORT ----------------------------------------------------------------
# write_csv(cata_ficha, "C:/Users/Elite Center/OneDrive/Documents/OCUC/procesamiento_catalogo/catalogo_fichas.csv")
write_csv(combinacion, "data/combinacion_clave.csv")
rm(cata,cata_ficha, fichas, etiquetas)

