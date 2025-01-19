#########################################################################################################
#########
#########             CONSTRUCCION DE INDICADORES REGIONALES
#########             SESION 1.1
#########             PORCENTAJE DE PARTICIPACION ESTATAL Y SECTORIAL, COEFICIENTE DE LOCALIZACION
#########             ENERO 2025
#########
#########################################################################################################

#Selecciona Directorio de trabajo. No es necesario si los datos están  en la misma ruta que el portafolio
#setwd("~/Documents/Traducciones/Roldan/Replica Indices/curso indicadores 2025/1.Base de datos/1.base primera sesión")

#setwd("~/Documents/base pasp")

#setwd("~/Documents/Sesion 1")

# Instala y carga librerías
#install.packages(c("REAT","ggplot2","readxl"))
#library(REAT)

packages<-c("REAT","readxl", "readxl","ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","spData" ,"spdep","mapview","colorRamps","fs","foreign","openxlsx")
if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)


##Lee archivo excel
matriz<-read_xlsx("Modif base total secre pobocup.xlsx")
matriz = as.data.frame(matriz)
str(matriz)

#cambia valores vacío por 0
#matriz[is.na(matriz)]<-0


#convierte las variables de caracter a factores 
matriz$Año<-as.factor(matriz$Año)
matriz$Sector<-as.factor(matriz$Sector)
matriz$No_sector <- str_pad(matriz$No_sector, width = 2, pad = "0")

# convierte los vacíos a ceros
matriz[is.na(matriz)]<-0

#Elimina los totales para quedarse solo con los datos estatales-sectoriales
matriz_est<-matriz%>%filter(!(No_sector=="01"))

matriz_est1<-matriz_est[,c(1:35)]
matriz_est1<-matriz_est[,-36]

# Divide la matriz y solo se queda con totales estatales
matriz_total_est<-matriz%>%filter(No_sector=="01")
# Totales sectoriales
matriz_total_sect<-matriz[,c(1:3,36)]

# convierte la matriz en formato largo para realizar los cálculos de manera más fácil 
#dplyr %>% pipes 

matriz_est_long<-matriz_est1%>%
  pivot_longer(
    cols="Aguascalientes":"Zacatecas",
    names_to = "CVE_ENT",
    values_to = "Pers_ocup" )
  
 # Calcula el porcentaje de participación de cada estado por sector
 particip_est<-matriz_est_long%>% 
  group_by(Año,Sector)%>%
  mutate(share_est=round(100*Pers_ocup/sum(Pers_ocup),2))%>%
  ungroup
 
 
 # Convierte la matriz de totales estatales en formato largo
 matriz_totest_long<-matriz_total_est%>% 
   pivot_longer(
     cols="Aguascalientes":"TOTAL_SECTORIAL",
     names_to = "CVE_ENT",
     values_to = "Pers_ocup" )%>%filter(!(CVE_ENT=="TOTAL_SECTORIAL"))
 
 # Calcula el porcentaje de participación total de cada estado por sector
 
 particip_totest<-matriz_totest_long%>% 
   group_by(Año,Sector)%>%
   mutate(share_est=round(100*Pers_ocup/sum(Pers_ocup),2))%>%
   ungroup
 
 ## regresa la base a formato ancho
 participest_wider<-particip_est %>%
   pivot_wider(names_from = c(CVE_ENT), values_from = c(Pers_ocup,share_est))

  participtotest_wider<-particip_totest %>%
   pivot_wider(names_from = c(CVE_ENT), values_from = c(Pers_ocup,share_est))
 
 
#verificar que los renglones de participacion suman 100
verifica<- rowSums(participest_wider[ , c(36:67)])
 verifica
 #no da 100 exacto por los redondeos
 
 # Calcula el porcentaje de participacion sectorial por estado 

 particip_sect<-matriz_est_long%>% 
   group_by(Año,CVE_ENT)%>%
   mutate(share_sect=round(100*Pers_ocup/sum(Pers_ocup),2))%>%
   ungroup()
 
 # Ordena el archivo de participación sectorial para que se vea la contribucion 
 #particip_sect<-particip_sect[order(particip_sect$CVE_ENT,particip_sect$Año,particip_sect$No_sector),]
 

 verif_part_sect <- particip_sect %>% 
   group_by(Año, CVE_ENT) %>%
   summarise(verifica = sum(share_sect), .groups = "drop")
 
 
 verif_part_sect<-verif_part_sect[order(verif_part_sect$Año, verif_part_sect$CVE_ENT),]
 
 ##la variación es por los redondeos. 
 
 # Filtra la matriz de totales sectoriales sin el total global
 
 matriz_totsect_long<-matriz_total_sect%>%filter(!(No_sector=="01"))

 #calcula el total anual de todos los sectores
 
 total<-matriz_totsect_long%>% 
   group_by(Año)%>%
   mutate(gran_total=sum(`TOTAL_SECTORIAL`))

 # calcula la participacion total de cada sector por año
 
 particip_totsect<-total%>% 
   group_by(Año,No_sector,Sector)%>%
   mutate(share_sect=round(100*`TOTAL_SECTORIAL`/gran_total,2))%>%
   ungroup
 
 # verifica que la suma de participaciones sea 100. La variación es por redondeos.
 verif_totsect<-particip_totsect%>% 
   group_by(Año)%>%
   summarise(verfica=sum(share_sect))
 
 
# participsect_wider<-particip_sect %>%
 #  pivot_wider(names_from = c(CVE_ENT), values_from = c(Pers_ocup,share_sect))
participsect_wider<-particip_sect %>%
  pivot_wider(names_from = c(CVE_ENT), 
              values_from = c(Pers_ocup,share_sect))

 ### RECONSTRUYE TABLA DE PORCENTAJES DE PARTICIPACIÓN ESTATAL
 
 #une por renglones los calculos estatales y los totales 
 tabla_share_est<-rbind(participest_wider[,c(1:3,36:67)],participtotest_wider[,c(1:3,36:67)])

 #ordena por año-sector
# tabla_share_est<-tabla_share_est[order(tabla_share_est$Año,tabla_share_est$No_sector),] 
 
 ##cambia nombres de columnas por nombres de los estados
 
 colnames(tabla_share_est)<-c("Año","No_sector","Sector", "Aguascalientes","Baja California", 
                              "Baja California Sur" , 
            "Campeche", "Coahuila","Colima", "Chiapas",
            "Chihuahua","CDMX", "Durango", "Guanajuato",
            "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
            "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
            "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa",
            "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
            "Veracruz", "Yucatan" , "Zacatecas" )     
 
 
 ## Guardar como objeto en R
 save(tabla_share_est,file="Participacion estatal.RData")

 #load("Participacion estatal.RData")
  ## Guardar en archivo excel 
 # deben tener instalado Java , pero no estoy segura si solo funciona en Mac 
 # tambien pueden intentar instalar la  librería openxlsx
 
 # si no pueden guardar en excel intenten usar la librería openslsx en conjunto
 #install.packages(c("openxlsx","xlsx"))
 #library(openxlsx)
 #library(xlsx)
 
 write.xlsx(tabla_share_est,file="Participacion estatal.xlsx")
 
 
 ## Guardar en archivo csv
 write.csv(tabla_share_est,file="Participacion estatal.csv",row.names = FALSE)
 
 
 ### RECONSTRUYE TABLA DE PORCENTAJES DE PARTICIPACIÓN SECTORIAL
 
 #Regresa la base a formato ancho
 participsect_wider<-particip_sect %>%
   pivot_wider(names_from = c(CVE_ENT), values_from = c(Pers_ocup,share_sect))

 # pega la base por columnas de los calculos sectoriales con los totales 
 tabla_share_sect<-cbind(participsect_wider[,c(1:3,36:67)],particip_totsect[,6])
 
 #ordena por año-sector
 tabla_share_sect<-tabla_share_sect[order(tabla_share_sect$Año,tabla_share_sect$No_sector),] 
 
 #cambia nombres de columnas por nombres de los estados
 colnames(tabla_share_sect)<-c("Año","No_sector","Sector", "Aguascalientes","Baja California", 
                              "Baja California Sur" , 
                              "Campeche", "Coahuila","Colima", "Chiapas",
                              "Chihuahua","CDMX", "Durango", "Guanajuato",
                              "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
                              "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
                              "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa",
                              "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                              "Veracruz", "Yucatan" , "Zacatecas" ,"Total sector")     
 
 ## Guardar como objeto en R
 save(tabla_share_sect,file="Participacion sectorial.RData")
 ## Guardar en archivo excel 
 write.xlsx(tabla_share_sect,file="Participacion sectorial.xlsx")
 
 ## Guardar en archivo csv
 write.csv(tabla_share_sect,file="Participacion sectorial.csv")
 
 
 
######################################################  
#############
#############               2a PARTE
#############     COEFICIENTE DE LOCALIZACION
#############     USANDO LA LIBRERIA REAT
######################################################  
 
 
## Usa la matriz original con los totales estatales y sectoriales  

 ### METODO 1 
 ## Usando la librería REAT para calcular por renglones 
 
 colnames(matriz)<-c("Año","No_sector","Sector","Aguascalientes","Baja California", 
                          "Baja California Sur" , 
                          "Campeche", "Coahuila","Colima", "Chiapas",
                          "Chihuahua","CDMX", "Durango", "Guanajuato",
                          "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
                          "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
                          "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa",
                          "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                          "Veracruz", "Yucatan" , "Zacatecas","Total_sectorial") 
 
 ## Para 2018

 coeflocal2018<-NULL
 for(i in 4:35){
   print(locq(unlist(matriz[2:20,i]),unlist(matriz[1,i]),unlist(matriz[2:20,36]),unlist(matriz[1,36])))
   coefloc<-locq(unlist(matriz[2:20,i]),unlist(matriz[1,i]),unlist(matriz[2:20,36]),unlist(matriz[1,36]))
   #print(coefloc)
   coeflocal2018<-cbind(coeflocal2018,coefloc)
 }
 
 #Agrega nombres de columnas  (estados)
 colnames(coeflocal2018) <- colnames(matriz)[4:35]
 #Agrega nombres de renglones (sectores)
 rownames(coeflocal2018)<-matriz$No_sector[2:20]
 
 #Para  2013
 coeflocal2013<-NULL
 for(i in 4:35){
   coefloc<-locq(unlist(matriz[22:40,i]),unlist(matriz[21,i]),unlist(matriz[22:40,36]),unlist(matriz[21,36]),plot.results = TRUE)
   coeflocal2013<-cbind(coeflocal2013,coefloc)
 }
 
 #Agrega nombres de columnas  (estados)
 colnames(coeflocal2013) <- colnames(matriz)[4:35]
 rownames(coeflocal2013)<-matriz$No_sector[22:40]
 
 ### Une las matrices por renglones en un archivo de datos
 
 coeflocal<-data.frame(rbind(coeflocal2018,coeflocal2013))
 
 # agrega nombres de renglon secuenciales
 rownames(coeflocal)<-seq(1,length(coeflocal$Aguascalientes))
 
 #agrega nombres de sector y años como variables 
 coeflocal$Año<-matriz$Año[c(2:20,22:40)]
 coeflocal$No_sector<-matriz$No_sector[c(2:20,22:40)]
 coeflocal$Sector<-matriz$Sector[c(2:20,22:40)]
 
 ## cambia el orden de las columnas para que queden primero Año, No_sector y Sector
 
 coeflocal<-coeflocal[,c(33:35,1:32)]
 
 ## guardar la base como archivo de datos de R
 
 save(coeflocal,file="Cocientelocalizacion.RData")
 
 ## guarda en archivo excel

 write.xlsx(coeflocal,file="Cociente de localizacion.xlsx")
  
write.csv(coeflocal,file="Cociente de localizacion.csv",row.names = FALSE)


######## FIN ###########