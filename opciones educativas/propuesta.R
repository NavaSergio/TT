# Cargar bibliotecas necesarias

packages<-c("dplyr","openxlsx")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)

# Cargar los datos
altadem_folios <- read.csv("altadem_folios.csv")
alta <- read.csv("alta.csv")
demalta_gen1 <- read.csv("demalta_gen1.csv")
oferalt_gen1long <- read.csv("oferalt_gen1long.csv")

# Fijar la semilla para fines de reproducibilidad
set.seed(731)

# Filtrar opciones de alta demanda
altas_opciones <- alta$x
altadem_folios <- altadem_folios %>%
  filter(OPC_ED01 %in% altas_opciones)

# Unir demanda y oferta por X, opción y sexo 
demof_altgen1 <- demalta_gen1 %>%
  inner_join(oferalt_gen1long, by = c("X","CVE_OPC", "SEXO"))

# Crear lista para almacenar resultados
resultados <- list()

# Ciclo para cada opción educativa
for (opcion in unique(demof_altgen1$CVE_OPC)) {
  
  print("Opción:")
  print(opcion)
  # Filtrar datos por opción
  datos_opcion <- demof_altgen1 %>%
    filter(CVE_OPC == opcion)
  print(datos_opcion)
  # Aspirantes por opción
  aspirantes_opcion <- altadem_folios %>%
    filter(OPC_ED01 == opcion)
  
  # Separar por género
  hombres <- aspirantes_opcion %>% filter(SEXO == "H")
  mujeres <- aspirantes_opcion %>% filter(SEXO == "M")
  
  # Obtener oferta por género
  oferta_hombres <- datos_opcion %>% filter(SEXO == "H") %>% pull(OFERTA)
  oferta_mujeres <- datos_opcion %>% filter(SEXO == "M") %>% pull(OFERTA)
  
  print("Oferta Hombres y Mujeres")
  print(oferta_hombres)
  print(oferta_mujeres)
  
  # Caso 1: Ambos géneros exceden la oferta
  asignados_hombres <- sample(hombres$FOLIO, min(nrow(hombres), oferta_hombres))
  asignados_mujeres <- sample(mujeres$FOLIO, min(nrow(mujeres), oferta_mujeres))
  
  print("Primeros 5 asignados Hombres y Mujeres")
  print(head(asignados_hombres))
  print(head(asignados_mujeres))  
  
  # Caso 2: Mujeres caben y sobran lugares
  if (nrow(mujeres) <= oferta_mujeres) {
    asignados_mujeres <- mujeres$FOLIO
    sobrante_mujeres <- oferta_mujeres - nrow(mujeres)
    asignados_hombres <- c(
      asignados_hombres,
      sample(hombres$FOLIO[!hombres$FOLIO %in% asignados_hombres], 
             min(nrow(hombres) - length(asignados_hombres), sobrante_mujeres))
    )
  }
  
  # Caso 3: Hombres caben y sobran lugares
  if (nrow(hombres) <= oferta_hombres) {
    asignados_hombres <- hombres$FOLIO
    sobrante_hombres <- oferta_hombres - nrow(hombres)
    asignados_mujeres <- c(
      asignados_mujeres,
      sample(mujeres$FOLIO[!mujeres$FOLIO %in% asignados_mujeres], 
             min(nrow(mujeres) - length(asignados_mujeres), sobrante_hombres))
    )
  }
  
  # Guardar resultados
  resultados[[opcion]] <- data.frame(
    FOLIO = c(asignados_hombres, asignados_mujeres),
    SEXO = c(rep("H", length(asignados_hombres)), rep("M", length(asignados_mujeres))),
    OPC_ED01 = opcion
  )
}

# Combinar resultados
asignacion_final <- do.call(rbind, resultados)

# Generar resumen
resumen <- demof_altgen1 %>%
  group_by(CVE_OPC, SEXO) %>%
  summarize(
    gen_demanda = sum(gen_demanda),
    OFERTA = sum(OFERTA),
    caben = ifelse(gen_demanda <= OFERTA, "asigna", "sortea"),
    dif = OFERTA - gen_demanda ,
    .groups = "drop"  # Elimina el agrupamiento tras el resumen
    )

# Exportar el resumen a un archivo Excel
write.xlsx(resumen, "resumen_asignacion.xlsx")


# Guardar resultados en archivo
write.csv(asignacion_final, "asignacion_final.csv", row.names = FALSE)
