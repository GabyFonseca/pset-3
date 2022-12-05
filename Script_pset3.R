##Gabriela Fonseca Rodríguez
##Codigo:202012093

##PROBLEM SET 2

#Limpiar el entorno
rm(list=ls())

#Version de R
R.version.string

## llamar y/o instalar las librerias para este punto
require(pacman)
p_load(tidyverse, arrow, rio ,broom, modelsummary, stargazer )  

##Importar la base de datos
base <- import(file="input/data_regresiones.rds")

##-------------------------REGRESIONES-----------------------------
##1.1 Estimaciones
modelo_1 <- lm(price ~ surface_total + property_type, base)
modelo_2 <- lm(price ~ property_type + surface_total + rooms + bathrooms, base)
modelo_3 <- lm(price ~ property_type + surface_total + dist_cole + dist_park, base)

##1.2 Presentar resultados
##Tabla de los tres modelos y exportarlo
msummary(list(modelo_1, modelo_2, modelo_3))
stargazer(modelo_1, modelo_2, modelo_3,
          type= 'text',
          title = "Comparacion de modelos",
          df = FALSE,
          digits = 1, 
          out = paste0('output/resultado_regresiones.xls'))

##Presentar gráfico y exportarlo
modelos = list('Modelo 1' = modelo_1 , 'Modelo 2' = modelo_2, 'Modelo 3' = modelo_3 )

png(filename = "output/plot_regresiones.png", width = 1200, height = 500)
modelplot(modelos) + coord_flip() + 
  labs(title = "Precio de las Casas" , subtitle = "Comparacion modelos")
dev.off()


##--------------------------DATOS ESPACIALES-------------------------
#Limpiar el entorno
rm(list=ls())

## llamar y/o instalar librerias para este punto
require(pacman)
p_load(tidyverse,rio,skimr,ggmap,sf, leaflet, tmaptools,ggsn, osmdata) 

##2.1 Descargar datos de la ciudad
cartagena= getbb("Cartagena, Colombia")
opq(bbox = getbb("Cartagena, Colombia"))

## Descargar datos de los restaurantes de la ciudad
osm = opq(bbox = getbb("Cartagena, Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") 
class(osm)
osm_sf = osm %>% osmdata_sf()
osm_sf
restaurantes = osm_sf$osm_points

## Descargar datos de los parques de la ciudad
parques <- opq(bbox = getbb("Cartagena, Colombia")) %>% add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)



##2.2 Visualizaciones 
leaflet() %>% addTiles() %>% addPolygons(data=parques)

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurantes, col="#9ACD32")

##2.3 Geocodificar direcciones
castillo=geocode_OSM("Castillo de San Felipe, Cartagena", as.sf=T)

##2.4 Exportar mapa
carta<- opq(bbox = getbb("Cartagena, Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
carta<- carta$osm_multipolygons %>% subset(admin_level==6)

parques <- parques[carta,] 
restaurantes <- restaurantes[carta,]
osm_layer <- get_stamenmap(bbox= as.vector(st_bbox(carta)), maptype="toner", source="osm", zoom=8) 


##guardamos el mapa usando ggmap
map<-ggplot() + geom_sf(data=carta) +
  geom_sf(data=restaurantes, aes(color="restaurantes"), inherit.aes = F)+
  geom_sf(data=parques, aes(color="parques"), inherit.aes = F)+
  geom_sf(data=parques, aes(color="B"), inherit.aes = F)+
  geom_sf(data=castillo, aes(color="C"),inherit.aes = F)+
  scale_color_manual(labels=c("A"="Restaurantes","B"="Parques" , "C"="Castillo de San Felipe"),
                     values=c("A"="red","B"="green" , "C"="blue")) + theme_test()
map
ggsave("output/mapa.png", map)


##----------------------WEB-SCRAPING Y PROCESAMIENTO DE TEXTO-------------------
#Limpiar el entorno
rm(list=ls())

## llamar y/o instalar librerias para este punto
require(pacman)
p_load(rvest, rio, stringi,tm,cluster,wordcloud, wordcloud2,
       RColorBrewer, RCurl, XML)

##3.1 Crear objeto que tenga el HTML
url = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia" 
html = read_html(url)
class(html) ##Se comprueba que si se guardo bien 

##3.2 Extraer titulo de la pagina
html %>% html_nodes(xpath = '//*[@id="firstHeading"]/span')  %>% 
  html_text()

##3.3 Extraer tabla de la pagina web y exportarla
tablas = html %>% html_table()
length(tablas) ##Necesitamos saber cuantas tablas de extrajeron

tablas[[4]] ##Se encuentra que es la tabla numero 4 la que necesitamos

tabla_departamentos = tablas[[4]]
export(x=tabla_departamentos , file="output/tabla_departamento.xlsx")

##3.4 Extraer parrafos y crear nube de palabras
html %>% html_elements("p") ##Los parrafos se definen como elemento p

parrafos=html %>% html_elements("p") %>% html_text()

parrafos_p <- Corpus(VectorSource(html))

#Creo una funcion para poder modificar caracteres que no necesito
funcion <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

parrafos_p<- tm_map(parrafos_p, funcion, "/")    #remplazar / con espacio
parrafos_p<- tm_map(parrafos_p, funcion, "@")    #remplazar @ con espacio
parrafos_p<- tm_map(parrafos_p, funcion, "\\|")  #remplazar | con espacio

#Empiezo a modificar el texto ya que solo necesito palabras
#Modifico a minisculas
parrafos_p <- tm_map(parrafos_p, content_transformer(tolower))

#Remuevo numeros
parrafos_p <- tm_map(parrafos_p, removeNumbers) 
  
#Remuevo puntuacion
parrafos_p<- tm_map(docs, removePunctuation) 

#Remuevo espacion en blanco extras
parrafos_p <-  tm_map(parrafos_p, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

  
##Remuevo palabras comunes como pronombres y demas para obtener palabras solo del tema 
stopwords <-  tm_map(parrafos_p, removeWords, stopwords('spanish'))
set.seed(500)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.25, 
            colors=brewer.pal(8, "Dark2"))


