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

# Conteo de altadem_folios

altadem_folios %>%
  group_by(SEXO) %>%
  summarize(
    total_no_asignados = n(),
    .groups = "drop"
    )

# Fijar la semilla para fines de reproducibilidad
set.seed(7317)

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
  mujeres <- aspirantes_opcion %>% filter(SEXO == "M")
  hombres <- aspirantes_opcion %>% filter(SEXO == "H")

  
  # Obtener oferta por género
  oferta_mujeres <- datos_opcion %>% filter(SEXO == "M") %>% pull(OFERTA)  
  oferta_hombres <- datos_opcion %>% filter(SEXO == "H") %>% pull(OFERTA)

  
  print("Oferta Hombres y Mujeres")
  print(oferta_hombres)
  print(oferta_mujeres)
  

  # Caso 1: Ambos géneros exceden los lugares
  if (nrow(hombres) > oferta_hombres && nrow(mujeres) > oferta_mujeres) {
    # Sorteo de lugares para mujeres
    asignados_mujeres <- sample(mujeres$FOLIO, oferta_mujeres)
    
    # Mujeres no asignadas
    mujeres_restantes <- mujeres$FOLIO[!mujeres$FOLIO %in% asignados_mujeres]
    
    # Juntar mujeres no asignadas con hombres
    todos_restantes <- c(mujeres_restantes, hombres$FOLIO)
    
    # Sorteo de lugares restantes entre hombres y mujeres no asignadas
    asignados_restantes <- sample(todos_restantes, oferta_hombres)
    
    # Dividir los asignados restantes entre hombres y mujeres
    asignados_hombres <- intersect(asignados_restantes, hombres$FOLIO)
    asignados_mujeres_restantes <- intersect(asignados_restantes, mujeres_restantes)
    
    # Actualizar asignación final
    asignados_mujeres <- c(asignados_mujeres, asignados_mujeres_restantes)
  }
  
  
  # Caso 2: Mujeres exceden la mitad de los lugares y hombres no
  else if (nrow(mujeres) > oferta_mujeres && nrow(hombres) <= oferta_hombres) {
      # Sorteo inicial: Asignar lugares a las mujeres
      asignados_mujeres <- sample(mujeres$FOLIO, oferta_mujeres)
      
      # Identificar mujeres no asignadas
      mujeres_restantes <- mujeres$FOLIO[!mujeres$FOLIO %in% asignados_mujeres]
      
      # Juntar mujeres no asignadas con hombres
      todos_restantes <- c(mujeres_restantes, hombres$FOLIO)
      
      # Sorteo de lugares restantes entre todos los restantes
      asignados_restantes <- sample(todos_restantes, oferta_hombres)
      
      # Dividir los asignados restantes entre hombres y mujeres
      asignados_hombres <- intersect(asignados_restantes, hombres$FOLIO)
      asignados_mujeres_restantes <- intersect(asignados_restantes, mujeres_restantes)
      
      # Actualizar asignación final de mujeres
      asignados_mujeres <- c(asignados_mujeres, asignados_mujeres_restantes)
    }
  
  
  
  # Caso 3: Solo hombres exceden los lugares
  else if (nrow(hombres) > oferta_hombres && nrow(mujeres) <= oferta_mujeres) {
    asignados_mujeres <- mujeres$FOLIO
    lugares_sobrantes <- oferta_hombres + oferta_mujeres - nrow(mujeres)
    asignados_hombres <- sample(hombres$FOLIO, lugares_sobrantes)
  }
  
  # Guardar resultados de la opción
  resultados[[opcion]] <- data.frame(
    FOLIO = c(asignados_mujeres, asignados_hombres),
    SEXO = c(rep("M", length(asignados_mujeres)), rep("H", length(asignados_hombres))),
    OPC_ED01 = opcion
  )
  

}

# Combinar resultados
asignacion_final <- do.call(rbind, resultados)

# Generar resumen con totales por género y generales
resumen <- demof_altgen1 %>%
  group_by(CVE_OPC, SEXO) %>%
  summarize(
    Demanda = sum(gen_demanda),
    Oferta = sum(OFERTA),
    .groups = "drop"
  ) %>%
  # Agregar cantidad asignada por sexo
  left_join(
    asignacion_final %>%
      group_by(OPC_ED01, SEXO) %>%
      summarize(Asignado = n(), .groups = "drop"),
    by = c("CVE_OPC" = "OPC_ED01", "SEXO" = "SEXO")
  )

# Calcular totales generales para cada opción
totales <- resumen %>%
  group_by(CVE_OPC) %>%
  summarize(
    SEXO = "TOTAL",
    Demanda = sum(Demanda, na.rm = TRUE),
    Oferta = sum(Oferta, na.rm = TRUE),
    Asignado = sum(Asignado, na.rm = TRUE),
    .groups = "drop"
  )

# Calcular proporción por género
resumen <- resumen %>%
  group_by(CVE_OPC) %>%
  mutate(Proporcion = round(Asignado / sum(Asignado, na.rm = TRUE), 2)) %>%
  ungroup()

# Agregar proporción para los totales (siempre será 1 para "TOTAL")
totales <- totales %>%
  mutate(Proporcion = 1)

# Combinar resumen por sexo y totales generales, luego ordenar por CVE_OPC
resumen_final <- bind_rows(resumen, totales) %>%
  arrange(CVE_OPC)

# Exportar el resumen a un archivo Excel
write.xlsx(resumen_final, "resumen_asignacion_con_totales_y_proporciones.xlsx", overwrite = TRUE)


# Guardar resultados en archivo
write.csv(asignacion_final, "asignacion_final.csv", row.names = FALSE)



# Generar la lista de no asignados
no_asignados <- altadem_folios %>%
  filter(!(FOLIO %in% asignacion_final$FOLIO)) %>%
  select(FOLIO, SEXO, OPC_ED01)

# Crear un resumen con el conteo de no asignados por sexo
resumen_no_asignados <- no_asignados %>%
  group_by(SEXO) %>%
  summarize(
    total_no_asignados = n(),
    .groups = "drop"
  )

# Exportar no asignados y su resumen a un archivo Excel
wb <- createWorkbook()
addWorksheet(wb, "No_Asignados")
writeData(wb, "No_Asignados", no_asignados)

addWorksheet(wb, "Resumen_No_Asignados")
writeData(wb, "Resumen_No_Asignados", resumen_no_asignados)

saveWorkbook(wb, "no_asignados.xlsx", overwrite = TRUE)

