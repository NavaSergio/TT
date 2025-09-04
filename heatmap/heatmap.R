# Asegúrate de tener los paquetes necesarios
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

packages <-c("readxl", "ggplot2", "tidyverse", "sf")
install_if_missing(packages)

puntos<-read_xlsx("BaseparaDol.xlsx")
puntos<-puntos[,c(1,3,2)]
colnames(puntos)<-c("DIRECCION","longitude","latitude")
my.sf.point <- st_as_sf(x = puntos, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

shapefile_municipios <- st_read("C:/Users/Sergio Nava/Documentos/GitHub/TT/Mapas1/mun21gw/mun21gw.shp")
shapefile_estados<-st_read("C:/Users/Sergio Nava/Documentos/GitHub/TT/heatmap/estatal/estatal/estatal.shp") 

entidades<-c("01","09","11","13","15","16","21","22","29")

shapefile_entidad <- shapefile_estados %>% filter(CVEGEO %in% entidades)
shapefile_municipal<-shapefile_municipios %>% filter(CVE_ENT %in% entidades)


# --- TRANSFORMAR TODAS LAS CAPAS AL MISMO CRS PROYECTADO ---
# Elegimos EPSG:3857 (Web Mercator) como CRS común
target_crs <- 3857 # O considera un CRS UTM específico para México si la precisión es crítica

my.sf.point_proj <- st_transform(my.sf.point, crs = target_crs)
shapefile_municipal_proj <- st_transform(shapefile_municipal, crs = target_crs)
shapefile_entidad_proj <- st_transform(shapefile_entidad, crs = target_crs)

# Extraer las coordenadas X e Y de los puntos proyectados
coords_for_density <- as.data.frame(st_coordinates(my.sf.point_proj))

# --- Graficar Heatmap con Estimación de Densidad por Kernel ---

ggplot() +
  # Capa base de municipios (usar el objeto proyectado)
  geom_sf(data = shapefile_municipal_proj, fill = "gray95", color = "white", lwd = 0.1) +
  
  # Capa de estimación de densidad por Kernel
  geom_density_2d_filled(data = coords_for_density, 
                         aes(x = X, y = Y, fill = after_stat(level)),
                         alpha = 0.3,
                         contour_var = "ndensity",
                         h = 5000, # Ancho de banda en unidades del CRS (metros). Ajusta según necesidad.
                         n = 100
  ) +
  
  # Contornos de los estados (usar el objeto proyectado)
  geom_sf(data = shapefile_entidad_proj, color = "black", fill = NA, lwd = 0.5) +

  # Escala de color para la densidad
  scale_fill_viridis_d(option = "plasma", name = "Densidad") + 
  
  ggtitle("Heatmap de Densidad de Puntos (Estimación Kernel)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(crs = target_crs) # Asegura que coord_sf también use el mismo CRS de destino
