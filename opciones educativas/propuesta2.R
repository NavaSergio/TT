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

# Filtrar opciones de alta demanda
altas_opciones <- alta$x
altadem_folios <- altadem_folios %>%
  filter(OPC_ED01 %in% altas_opciones)

# Unir demanda y oferta

demof_altgen1 <- demalta_gen1 %>%
  inner_join(oferalt_gen1long, by = c("X","CVE_OPC", "SEXO"))

# Crear lista para almacenar resultados
resultados <- list()

# Ciclo para cada opción educativa
for (opcion in unique(demof_altgen1$CVE_OPC)) {
  
  # Filtrar datos por opción
  datos_opcion <- demof_altgen1 %>%
    filter(CVE_OPC == opcion)
  
  # Aspirantes por opción
  aspirantes_opcion <- altadem_folios %>%
    filter(OPC_ED01 == opcion)
  
  # Separar por género
  mujeres <- aspirantes_opcion %>% filter(SEXO == "M")
  hombres <- aspirantes_opcion %>% filter(SEXO == "H")
  
  # Obtener oferta total y calcular proporción de demanda por género
  oferta_total <- sum(datos_opcion$OFERTA)
  demanda_total <- nrow(aspirantes_opcion)
  
  proporcion_mujeres <- nrow(mujeres) / demanda_total
  proporcion_hombres <- nrow(hombres) / demanda_total
  
  oferta_mujeres <- round(oferta_total * proporcion_mujeres)
  oferta_hombres <- oferta_total - oferta_mujeres
  
  # Sorteo de lugares
  asignados_mujeres <- if(nrow(mujeres) > oferta_mujeres) sample(mujeres$FOLIO, oferta_mujeres) else mujeres$FOLIO
  asignados_hombres <- if(nrow(hombres) > oferta_hombres) sample(hombres$FOLIO, oferta_hombres) else hombres$FOLIO
  
  # Guardar resultados de la opción
  resultados[[opcion]] <- data.frame(
    FOLIO = c(asignados_mujeres, asignados_hombres),
    SEXO = c(rep("M", length(asignados_mujeres)), rep("H", length(asignados_hombres))),
    OPC_ED01 = opcion
  )
}

# Combinar resultados
asignacion_final <- do.call(rbind, resultados)

# Exportar asignación final
write.csv(asignacion_final, "asignacion_final.csv", row.names = FALSE)

# Generar la lista de no asignados
no_asignados <- altadem_folios %>%
  filter(!(FOLIO %in% asignacion_final$FOLIO)) %>%
  select(FOLIO, SEXO, OPC_ED01)

# Exportar no asignados
write.csv(no_asignados, "no_asignados.csv", row.names = FALSE)

################################

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


