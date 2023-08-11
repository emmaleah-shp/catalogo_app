pacman::p_load(tidyselect, tidyverse, dplyr, glue, knitr, stringr, magrittr, 
               sf, ggmap, ggplot2, flextable, data.table, sp, 
               scales, devtools, tinytex, writexl, webshot, readr)
library(arcgisbinding)
arc.check_product()
options(scipen=999)


combinacion<-read.csv("combinacion_clave.csv")

# whitespace
wsp1<-function(x){
  num<-str_count(x, "\\s+") - str_count(x, "\\w")
  c<-sum(num>0)
  c
}

whitespacedf<-function(df){
  dfx<- df %>% as.data.frame() %>% dplyr::select(where(is.character))%>% na.exclude() %>%
    dplyr::summarise(across(.cols = everything(), .fns = ~ wsp1(.x), .names = "{.col}"))
  dff<-dfx %>% dplyr::summarise(across(.cols = everything(), .fns = ~ sum(.x!=0), .names = "{.col}")) %>% t()
  dff<-as.data.frame(dff) %>% rownames_to_column()
  names(dff)[c(2)]<-"whitespace"
  dff
}

# unique 
n_distinct<- function(x){
  length(unique(x))
}

# Diagnosticar
diagnosticar<- function(df){
  dfx<-df %>% as.data.frame() %>% dplyr::select(!last_col()) # CAN also use select(!c("geometry"))
  tip<-data.frame(sapply(dfx,typeof)) %>%
    rownames_to_column()
  vac<- dfx %>% dplyr::summarise(across(.cols = everything(), 
                                        .fns = ~ sum(is.na(.x)), .names = "{.col}")) %>% t()
  vac<- as.data.frame(vac) %>% rownames_to_column()
  stch<- merge(tip,vac, by="rowname") %>% as.data.frame()
  ws<-whitespacedf(dfx)
  stch<-merge(stch, ws, by="rowname", all.x=TRUE)
  uniq<- dfx %>% dplyr::summarise(across(everything(), .fns = ~ n_distinct(.x), .names ="{.col}")) %>% 
    t() %>% as.data.frame() %>% rownames_to_column()
  stch<-merge(stch, uniq, by="rowname")
  stch[is.na(stch)] <- 0
  stch<- stch %>% dplyr::mutate(pc_completo = (nrow(dfx) - (V1.x + whitespace)) / (nrow(dfx)) *100,
                                pc_repetido=(nrow(dfx) - V1.y) / (nrow(dfx)) *100)
  names(stch)[c(2:5)]<-c("tipo","vacios","whitespace","n_unico")
  stch
}

amb <- str_extract(shps1, "^\\w{3}")
amb <- amb %>% 
  str_replace_all("MOV", "movilidad") %>% 
  str_replace_all("BAS", "informacion_base") %>% 
  str_replace_all("SEG", "seguridad") %>% 
  str_replace_all("RIE", "riesgos_y_emergencias") %>% 
  str_replace_all("PLA", "planificacion") %>% 
  str_replace_all("MED", "medio_ambiente")

### HACER PDFs --------------------------------------------------------------------
# for(i in 1:length(shps1)){
#   titulo_capa<-shps1[i]
#   capa<-
#     titulo <- stringr::str_c(str_replace_all(titulo_capa, '_', ' '), 'Diagnosis', sep=' ')
#   output_file  <- str_glue("{titulo_capa}_diagnosis.pdf")
#   rmarkdown::render(
#     input         = "plantilla.Rmd", # pathname de la plantilla 
#     output_format = "pdf_document",
#     output_file   = output_file,
#     output_dir    = "datos_pdf/",
#     params        = list(
#       titulo = titulo,
#       show_code      = FALSE
#     )
#   )
# }


### HACER HTMLs --------------------------------------------------------------------
library(stringr)
library(rmarkdown)

for (i in 1:5) { #length(shps1)
  titulo_capa <- shps1[i]
  titulo_sitio <- stringr::str_to_title(stringr::str_replace_all(titulo_capa, '_', ' '))
  titulo <- substring(titulo_sitio, first = 4) %>% str_squish()
  nombre<- substring(titulo_capa, first = 5) %>% str_squish()
  amb_title <-stringr::str_replace_all(amb[i], '_', ' ') %>% str_to_title()
  od <- "~/html"
  
  rmarkdown::render(
    input = "~/OCUC/PROYECTOS/catalogo_book/_book/skele_edit.Rmd",
    output_format = "all",
    output_dir = od,
    output_file= str_glue("{amb[i]}_{nombre}"),
    # output_options = list(
    #   lib_dir = "~/OCUC/PROYECTOS/catalogo_book/_book/_book/libs",
    #   css = "~/OCUC/PROYECTOS/catalogo_book/_book/_book/libs/bs4_book-1.0.0/bs4_book.css"
    # ),
    params = list(title = titulo, 
                  titulo_capa= titulo_capa, 
                  amb_title= amb_title)
  )
}
