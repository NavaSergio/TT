# Example from Farhauer/Kroell (2013):
locq (1714, 79006, 879213, 15593224)
# returns the location quotient (0.3847623)

# Location quotients for Goettingen 2017:
data(Goettingen)
locq (Goettingen$Goettingen2017[2:16], Goettingen$Goettingen2017[1], 
      Goettingen$BRD2017[2:16], Goettingen$BRD2017[1],  plot.results=TRUE)



# Verificar si el paquete rstudioapi está instalado y cargarlo
if (!require(rstudioapi)) install.packages("rstudioapi")

# Obtener la versión de R
version_r <- R.version$version.string

# Obtener la versión de RStudio (si estás usando RStudio)
version_rstudio <- if (rstudioapi::isAvailable()) {
  rstudioapi::versionInfo()$version
} else {
  "No estás usando RStudio"
}

# Obtener las versiones de los paquetes instalados
versiones_paquetes <- as.data.frame(installed.packages()[, c("Package", "Version")])

# Guardar los resultados en un archivo
archivo_salida <- "versiones_R_RStudio_paquetes.txt"

writeLines(c(
  paste("Versión de R:", version_r),
  paste("Versión de RStudio:", version_rstudio),
  "",
  "Paquetes instalados y sus versiones:"
), archivo_salida)

write.table(
  versiones_paquetes,
  file = archivo_salida,
  append = TRUE,
  row.names = FALSE,
  col.names = TRUE
)

# Mensaje de confirmación
cat("Información guardada en", archivo_salida, "\n")
