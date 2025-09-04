#setwd("~/Library/CloudStorage/OneDrive-SubsecretaríadeEducaciónMediaSuperior/Documentos/SEP/SEMS/Mapas1/mun21gw")
#setwd("C:\\Users\\Sergio Nava\\Documentos\\GitHub\\TT\\Mapas1\\mun21gwgw")

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

# Lista de paquetes necesarios
packages <-c("readxl", "ggplot2", "tidyverse", "psych","GGally","dplyr","tidyverse","groupdata2",
             "spData" ,"spdep","mapview","colorRamps","sf","foreign","sp","tibble","tidygeocoder")

#Aplica función para cargar paquetes
install_if_missing(packages)

#install.packages("spDataLarge") este no jalo
#install.packages(c("tibble","tidygeocoder"))
#library(spDataLarge)

puntos<-read_excel("BaseparaDol.xlsx")
puntos<-puntos[,c(1,3,2)]
colnames(puntos)<-c("DIRECCION","longitude","latitude")
my.sf.point <- st_as_sf(x = puntos, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")



#Lee shapefile municipal que está en la subcarpeta mun21gw
shapefile_municipios <- st_read("C:/Users/Sergio Nava/Documentos/GitHub/TT/Mapas1/mun21gw/mun21gw.shp")

shapefile_estados<-st_read("C:/Users/Sergio Nava/Documentos/GitHub/TT/heatmap/estatal/estatal/estatal.shp") 
### este shapefile tiene otras variables que no me sirven 
shapefile_estados<-shapefile_estados %>%select(CVEGEO,NOMGEO,OID,geometry)

entidades<-c("01","09","11","13","15","16","21","22","29")

shapefile_entidad <- shapefile_estados %>% filter(CVEGEO %in% entidades)
shapefile_municipal<-shapefile_municipios %>% filter(CVE_ENT %in% entidades)



# Asegurarse de que ambos objetos sf tengan el mismo CRS antes de la intersección
my.sf.point <- st_transform(my.sf.point, st_crs(shapefile_municipal))


library(scales)
# Grafica todos los municipios

ggplot(data = shapefile_municipal) +
  geom_sf(fill = "lightblue", color = "white") +
  geom_sf(data=shapefile_entidad, color="black", fill=NA)+
  geom_sf(data=my.sf.point,size=0.5)+
  ggtitle("Municipios") +
  theme_minimal()


# Realizar la intersección espacial para encontrar qué puntos caen en qué municipio
points_in_municipios <- st_join(my.sf.point, shapefile_municipal, join = st_intersects)

# Contar el número de puntos por municipio
conteo_puntos_por_municipio <- points_in_municipios %>%
  filter(!is.na(CVE_MUN)) %>% # Elimina puntos que no cayeron en ningún municipio (si los hubiera)
  group_by(CVE_MUN, NOM_MUN) %>% # Agrupar por clave y nombre del municipio
  summarise(numero_puntos = n(), .groups = 'drop') %>%
  st_drop_geometry() # <--- ¡MANTENER ESTA LÍNEA! FUERZA QUE SEA UN DATA FRAME NORMAL



# Unir el conteo de puntos con el shapefile municipal (¡USAR left_join de dplyr!)
shapefile_municipal_con_puntos <- shapefile_municipal %>%
  left_join(conteo_puntos_por_municipio, by = c("CVE_MUN", "NOM_MUN")) %>% 
  mutate(numero_puntos = replace_na(numero_puntos, 0)) # Reemplazar NA (municipios sin puntos) con 0


# --- Graficar el Heatmap ---


ggplot(data = shapefile_municipal_con_puntos) +
  geom_sf(aes(fill = numero_puntos), color = "white", lwd = 0.1) + # Rellenar por numero_puntos
  geom_sf(data = shapefile_entidad, color = "black", fill = NA, lwd = 0.5) + # Fronteras de estados
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Número de Puntos") + # Escala de color
  ggtitle("Heatmap del Número de Puntos por Municipio") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar el título

###################################


# --- Graficar Heatmap con Estimación de Densidad por Kernel ---

# Reproyectar los puntos para el cálculo de densidad
# Es crucial trabajar con un CRS proyectado (unidades métricas) para KDE
my.sf.point_proj <- st_transform(my.sf.point, crs = 3857) # Web Mercator, unidades en metros (común para web maps)

# Extraer las coordenadas X e Y de los puntos proyectados
# geom_density_2d_filled() necesita columnas X e Y explícitas.
# st_coordinates() devuelve una matriz con columnas X e Y.
coords_for_density <- as.data.frame(st_coordinates(my.sf.point_proj)) # Convertir a data.frame

ggplot() +
  # Capa base de municipios (para contexto)
  geom_sf(data = shapefile_municipal, fill = "gray95", color = "white", lwd = 0.1) +
  
  # Capa de estimación de densidad por Kernel
  # Usamos el nuevo data frame 'coords_for_density' con las columnas 'X' e 'Y'
  geom_density_2d_filled(data = coords_for_density, 
                         aes(x = X, y = Y, fill = after_stat(level)), # ¡Usar X e Y!
                         alpha = 0.6,  # Transparencia del heatmap
                         contour_var = "ndensity", # Normaliza la densidad para que el máximo sea 1
                         h = 5000, # Ancho de banda en unidades del CRS (ej. metros si es EPSG:3857). Ajusta según necesidad.
                         n = 100 # Número de cuadrículas para la estimación (mayor = más detallado)
  ) +
  
  # Contornos de los estados (para contexto)
  geom_sf(data = shapefile_entidad, color = "black", fill = NA, lwd = 0.5) +
  
  # Puedes añadir los puntos originales si quieres verlos sobre el heatmap
  geom_sf(data = my.sf.point, size = 0.5, color = "blue", alpha = 0.3) + # Puntos originales (en CRS original)
  
  # Escala de color para la densidad
  scale_fill_viridis_d(option = "plasma", name = "Densidad") + # viridis_d para el factor 'level' generado
  
  ggtitle("Heatmap de Densidad de Puntos (Estimación Kernel)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + # Centrar el título
  coord_sf() # Asegura que las coordenadas se manejen correctamente para objetos sf  
  
### Hasta aqui llegué con el mapa
##Los puntos que están en estados disstintos a CDMX, Mexico, 
## Puebla e Hidalgo, están mal geolocalizados



####### PARA SACAR LAS COORDENADAS DE LA DIRECCION HAY QUE SEPARAR LA DIRECCION Y QUITAR LOS ESPACIOS
###### AL PRINCIPIO DE CADA CADENA. EN EL EXCEL LO HICE A MANO PARA ASIGNAR LAA ENTIDAD 
##


install_if_missing(c("tibble","dplyr","tidygeocoder","stringr"))
calcula<-read_excel("direcciones.xlsx")


calcula$CP<-str_pad(calcula$CP, 5, pad = "0")  ## Pone cero al principio del cp para que queden 5 dìgitos
calcula$STATE[calcula$STATE=="CDMX"]<-"MEXICO CITY"
calcula$STATE[calcula$STATE=="MEXICO"]<-"STATE OF MEXICO"
calcula$CITY<-trimws(calcula$CITY,which="left") ## quita el espacio en blanco al principio de la frase
calcula$STREET<-trimws(calcula$STREET,which="left")

##toma 1 segundo por registro aprox  ~ aprox 1 hora 15 minutos para 260 mil registros
df <- calcula %>%
  geocode(street = STREET, city = CITY, county=MUNICIPALITY,country = COUNTRY,postalcode=CP,state=STATE, 
          method = "osm")


# loads the package
library("writexl") 

# saves the dataframe at the specified
# path
write_xlsx(df,"df_geocoder_coord.xlsx")
