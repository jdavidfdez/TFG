# Carga del conjunto de datos objeto de estudio. 
# Se crean dos nuevas variables: RANGO_PRECIO Y RANGO_PTOS
carga_datos<-function()
{
  todos<-read.csv("DATOS/VINOS/TODOS.csv",header=T,sep=",",dec=".",encoding="UTF-8")
  todos<-todos[,c(2,7,8,13,14,4,3,5,6)]
  colnames(todos)<-c("PAIS","PROVINCIA","REGION","VARIEDAD","BODEGA","NOMBRE","DESCRIPCION","PUNTOS","PRECIO")
  todos<-todos[!is.na(todos$PRECIO) & todos$PAIS!="",]
  cuantiles<-todos %>% group_by(PUNTOS) %>% 
    summarize(q1=quantile(PRECIO,.25),q2=median(PRECIO),q3=quantile(PRECIO,.75))
  todos<-merge(todos,cuantiles)
  todos$RANGO_PRECIO<-ifelse(todos$PRECIO<=todos$q1,"BARATO",ifelse(todos$PRECIO>todos$q3,"CARO","PRECIO_MED"))
  todos$RANGO_PTOS<-ifelse(todos$PUNTOS>91,"Q4_PTOS",ifelse(todos$PUNTOS<=86,"Q1_PTOS","PTOS_MED"))
  todos<-todos[,c(2,5,13:14,1,9,8)]
}
# Prepara el conjunto de datos para el análisis de la variable textual
carga_datos_textual<-function(todos,n)
{
  todos<-trata_txt(todos,n)
  todos$PAIS<-as.character(todos$PAIS)
  todos$PAIS<-factor(todos$PAIS)
  todos$VARIEDAD<-as.character(todos$VARIEDAD)
  todos$VARIEDAD<-factor(todos$VARIEDAD)
  return(todos)
}
# Realiza el PREPROCESAMIENTO de la variable DESCRIPCION
trata_txt<-function(vinos,n)
{
  vinos$DESCRIPCION<-as.character(vinos$DESCRIPCION)
  
  vinos$DESCRIPCION<-gsub("[[:cntrl:]]", " ", vinos$DESCRIPCION)
  vinos$DESCRIPCION<-removeWords(tolower(vinos$DESCRIPCION), words = stopwords("english"))
  vinos$DESCRIPCION<-removeWords(vinos$DESCRIPCION,"wine")
  vinos$DESCRIPCION<-removePunctuation(vinos$DESCRIPCION)
  vinos$DESCRIPCION<-removeNumbers(vinos$DESCRIPCION)
  vinos$DESCRIPCION<-stripWhitespace(vinos$DESCRIPCION)
  # vinos$DESCRIPCION<-stemDocument(vinos$DESCRIPCION,language="english")
  if (n==1)
    tidy_vinos <- vinos %>%
      unnest_tokens(word, DESCRIPCION)
  else
    tidy_vinos <- vinos %>%
    unnest_tokens(word, DESCRIPCION, token = "ngrams", n = 2)
  
  return(tidy_vinos)
}
# Cuenta la frecuencia de aparición de los términos en un texto y devuelve una lista de los mas frecuentes
# recibe como parámetros el texto a analizar y el número de términos a incluir en la lista
terminos_f<-function(ti,numero)
{
  nuevo<-ti %>%
    count(word, sort = TRUE) %>%
    mutate(n=n,word = reorder(word, n))
  if (numero<1)
  {
    limite<-round(nrow(ti)*numero,0)    
    return(data.frame(TODOS=as.character(nuevo[nuevo$n>=limite,]$word),N=nuevo[nuevo$n>=limite,]$n))
  }
  else
    return(data.frame(TODOS=as.character(nuevo[1:numero,]$word),N=nuevo[1:numero,,]$n))
}
g_term_freq<-function(ti,txt,frec,opc,tam)
{
  nuevo<-ti %>%
    count(word, sort = TRUE) %>%
    mutate(n=n,word = reorder(word, n))
  if (opc==0)
  {
    g<-ggplot(nuevo[1:tam,],aes(word,n,fill=nuevo[1:tam,]$word)) +
      geom_col() +
      ggtitle(txt) + 
      xlab(NULL) +
      theme(legend.position = "none") +
      coord_flip()
  }
  else
  {
    paleta<-brewer.pal(3, "Paired")[c(2:1,3)]
    colores<-as.character(nuevo[1:tam,]$word)
    # porc<-round(100*(length(colores[!colores %in% frec$TODOS])/tam),0)
    lista<-colores %in% frec[1:tam,]$TODOS
    colores[lista]<-as.character(paleta[1])
    colores[!lista]<-as.character(paleta[2])
    switch(opc,colores_1<-colores,colores_2<-colores,colores_3<-colores)
    g<-ggplot(nuevo[1:tam,],aes(word,n,fill=switch(opc,colores_1,colores_2,colores_3))) + 
      geom_col() +
      scale_fill_brewer(palette="Paired") +
      ggtitle(txt) +
      xlab(NULL) +
      theme(legend.position = "none") +
      coord_flip()
  }
  # ggsave(paste("term_freq_",txt,".png",sep=""),g,path="graficos/textual")
  return(g)
}
#calcula la distribuci?n de frecuencias
dist_freq_terminos<-function(tt,op)
{
  if (op==1)
  {
    words<- tt %>%
      count(RANGO_PTOS, word, sort = TRUE)
    total_words <- words %>% 
      group_by(RANGO_PTOS) %>% 
      summarize(total = sum(n))
  }
  else
  {
    words<- tt %>%
      count(RANGO_PRECIO, word, sort = TRUE)
    total_words <- words %>% 
      group_by(RANGO_PRECIO) %>% 
      summarize(total = sum(n))
  }
  
  words <- left_join(words, total_words)
}
buscar_asociados<-function(termino,dtm,fr)
{
  asociados<-findAssocs(dtm,as.character(termino[1]),.6)
  if (length(asociados)>0)
  {
    return(data.frame(TERMINO=rep(termino,cuantos),ASOCIADOS=names(asociados[[1]])))
    # encontrados<-asociados[[1]][!names(asociados[[1]]) %in% fr$TODOS]
    # cuantos<-length(encontrados)
    # if (cuantos>0)
    #   return(data.frame(TERMINO=rep(termino,cuantos),ASOCIADOS=encontrados))
  }
  # else
  #   return(termino)
}
terminos_no_f<-function(ti,numero)
{
  nuevo<-ti %>%
    count(word, sort = TRUE) %>%
    mutate(n=n,word = reorder(word, n))
  return(data.frame(TODOS=as.character(nuevo[(numero+1):nrow(nuevo),]$word)))
}

haz_tf_idf<-function(mi)
{
  mi <- mi %>%
    bind_tf_idf(word, PUNTOS, n)
  mi
  
  mi %>% select(-total) %>% arrange(desc(tf_idf))
  
  return(mi %>%
           arrange(desc(tf_idf)) %>%
           mutate(word = factor(word, levels = rev(unique(word)))) %>% 
           group_by(PUNTOS) %>% 
           top_n(15) %>% 
           ungroup() %>%
           ggplot(aes(word, tf_idf, fill = PUNTOS)) +
           geom_col(show.legend = FALSE) +
           labs(x = NULL, y = "tf-idf") +
           facet_wrap(~PUNTOS, ncol = 3, scales = "free") +
           coord_flip())
}

# instalar<-function(x)
# {
#   if(!is.element(x, installed.packages()))
#     install.packages(x,dependencies=T)
#   library(x)
# }
# muestrea<-function(df)
# {
#   Estratos<- df %>%
#     select(doc_id,PUNTOS,PRECIO,text) %>%
#     group_by(PUNTOS) %>%
#     summarise(n=n(),
#               s=sd(PRECIO)) %>%
#     mutate(p=n/sum(n))
#   df[order(df$PUNTOS),]
#   tams<-round(100*(Estratos$n/length(df$doc_id)),0)
#   for(i in 1:length(tams))
#     if(tams[i]==0)
#       tams[i]<-1
#   st<-strata(df,"PUNTOS",tams)
#   return(getdata(df,st))
# }
guarda<-function(grafico,nombre,carpeta)
{
  ggsave(nombre,grafico,path=carpeta)
}
