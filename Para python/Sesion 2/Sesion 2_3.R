##########################################################################################
#########             CONSTRUCCION DE INDICADORES REGIONALES
#########             SESION 2.3 continuacion
#########             THEIL, ATKINSON, DALTON, COULTER
#########             ENERO 2025
##########################################################################################


rm(list = ls())

#Selecciona Directorio de trabajo. No es necesario si los datos están  en la misma ruta que el portafolio

setwd("~/Documents/Sesion 2")

# Instala y carga librerías
#install.packages(c("REAT","ggplot2","readxl"))
#library(REAT)

packages<-c("REAT","readxl","openxlsx", "ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","spData" ,"spdep","mapview","colorRamps","fs","foreign","xlsx")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)


##Lee archivo excel 
mat_pobocup<-read_xlsx("Modif base total secre pobocup.xlsx")
str(mat_pobocup)

matriz_pob2018<-mat_pobocup[c(1:19),c(4:35)]
matriz_pob2013<-mat_pobocup[c(21:39),c(4:35)]


mat_remun<-read_xlsx("Matriz SECRE Remuneraciones.xlsx",sheet="Sheet1")

##### solo se queda con la matriz de datos

matriz_rem2018<-mat_remun[c(1:19),c(4:35)]
matriz_rem2013<-mat_remun[c(21:39),c(4:35)]



## Cociente de Theil para remuneraciones


## sectorial por renglones
theilrem_sect2018<- round(apply(matriz_rem2018, 1, theil, na.rm=TRUE),4)
theilrem_sect2013<- round(apply(matriz_rem2013, 1, theil, na.rm=TRUE),4)

#une archivos 

labels_sect<-mat_remun[c(1:19),3]
labels_sect

labels_sect<-mat_remun$Sector[c(1:19)]
labels_sect

theil_remsectorial<-data.frame(cbind(Sector=labels_sect,theilrem_sect2013,theilrem_sect2018))

## estatal por columnas

theilrem_est2018<- round(apply(matriz_rem2018, 2, theil, na.rm=TRUE),4)
theilrem_est2013<- round(apply(matriz_rem2013, 2, theil, na.rm=TRUE),4)

theilrem_estatal<-data.frame(cbind(theilrem_est2013,theilrem_est2018))


#### Coeficiente de Atkinson para remuneraciones

## sectorial por columnas 

atkin_2013<-round(apply(matriz_rem2013,1,atkinson,na.rm = TRUE),4)
atkin_2018<-round(apply(matriz_rem2018,1,atkinson,na.rm = TRUE),4)

atkin_sectrial<-data.frame(cbind(Sector=labels_sect),atkin_2013,atkin_2018)

atkinest_2013<-round(apply(matriz_rem2013,2,atkinson,na.rm=TRUE),4)
atkinest_2018<-round(apply(matriz_rem2018,2,atkinson,na.rm=TRUE),4)

atkinest2_2013<-round(apply(matriz_rem2013,2,atkinson,epsilon=1.5,na.rm=TRUE),4)
atkinest2_2018<-round(apply(matriz_rem2018,2,atkinson,epsilon=1.5,na.rm=TRUE),4)

atkin_est2<-data.frame(cbind(atkinest_2013,atkinest2_2013,atkinest_2018,atkinest2_2018))

### Dalton para remuneraciones 

## sectorial 

dalton_2013<-round(apply(matriz_rem2013,1,dalton,na.rm = TRUE),4)
dalton_2018<-round(apply(matriz_rem2018,1,dalton,na.rm = TRUE),4)

##  UNIR ARCHIVOS

dalton_sectrial<-data.frame(cbind(Sector=labels_sect),dalton_2013,dalton_2018)

### estatal 

### Coulter para remuneraciones 

## sectorial 

coulter_2013<-round(apply(matriz_rem2013,1,coulter,na.rm = TRUE),4)
coulter_2018<-round(apply(matriz_rem2018,1,coulter,na.rm = TRUE),4)

##  UNIR ARCHIVOS

coult_sectrial<-data.frame(cbind(Sector=labels_sect),coulter_2013,coulter_2018)


##### FIN ####