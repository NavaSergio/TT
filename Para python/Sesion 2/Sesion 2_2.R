##########################################################################################
#########             CONSTRUCCION DE INDICADORES REGIONALES
#########             SESION 2.2 
#########             COEFICIENTE DE HERFINDAHL, DESVIACIÓN ESTÁNDAR, COEFICIENTE DE VARIACIÓN
#########             ENERO 2025
##########################################################################################
rm(list = ls())

#Selecciona Directorio de trabajo. No es necesario si los datos están  en la misma ruta que el portafolio

setwd("~/Documents/Sesion 2")

# Instala y carga librerías
#install.packages(c("REAT","ggplot2","readxl"))
#library(REAT)

packages<-c("REAT","readxl", "ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","spData" ,"spdep","mapview","colorRamps","fs","foreign","xlsx")

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

#### Coeficiente de Herfindahl-Hirschman
## para las remuneraciones


## sectorial por renglones
HHIrem_sect2018<- round(apply(matriz_rem2018, 1, herf, na.rm=TRUE),4)
HHIrem_sect2013<- round(apply(matriz_rem2013, 1, herf, na.rm=TRUE),4)

#une archivos 
labels_sect<-mat_remun[c(1:19),3]


HHI_remsectorial<-data.frame(cbind(Sector=labels_sect,HHIrem_sect2013,HHIrem_sect2018))

## estatal por columnas
HHIrem_est2018<- round(apply(matriz_rem2018, 2, herf, na.rm=TRUE),4)
HHIrem_est2013<- round(apply(matriz_rem2013, 2, herf, na.rm=TRUE),4)

HHIrem_estatal<-data.frame(cbind(HHIrem_est2013,HHIrem_est2018))


### guardar archivos

## MEDIDAS DE DISPERSION: VARIANZA, DESVIACION ESTANDAR Y COEFICIENTE DE VARIACION =desv est/media=sigma/mu

### Poblacion ocupada  incluyendo los totales estatales pero no los sectoriales

matriz_pob2018<-mat_pobocup[c(1:20),c(4:35)]
matriz_pob2013<-mat_pobocup[c(21:40),c(4:35)]


## sectorial por renglones incluyendo el total sectorial
## varianza

varianza_sect2018<-apply(matriz_pob2018, 1,function(x) var(x, na.rm=T))
varianza_sect2013<-apply(matriz_pob2013, 1,function(x) var(x, na.rm=T))


## desviacion estandar 

desv_sect2018<-apply(matriz_pob2018, 1,function(x) sd(x, na.rm=T))
desv_sect2013<-apply(matriz_pob2013, 1,function(x) sd(x, na.rm=T))


## coeficiente de variacion

cv_sect2018<-apply(matriz_pob2018, 1,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))
cv_sect2013<-apply(matriz_pob2013, 1,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))

## con librería reat
cv_sect2_2018<-apply(matriz_pob2018, 1, cv, na.rm=TRUE)
cv_sect2_2013<-apply(matriz_pob2013, 1, cv, na.rm=TRUE)


labels_sect<-rbind(mat_remun[c(1:19),3],"Total_estatal")
## une archivos

sectorial_pobocup<-data.frame(cbind(Sector=labels_sect, varianza_sect2013,desv_sect2013,
                      cv_sect2013,cv_sect2_2013,varianza_sect2018,desv_sect2018,cv_sect2018,
                      cv_sect2_2018))


## estatal por columnas incluyendo el total estatal pero no el total sectorial

matriz_pob2018<-mat_pobocup[c(1:19),c(4:36)]
matriz_pob2013<-mat_pobocup[c(21:39),c(4:36)]


varianza_est2018<-apply(matriz_pob2018, 2,function(x) var(x, na.rm=T))
varianza_est2013<-apply(matriz_pob2013, 2,function(x) var(x, na.rm=T))


## desviacion estandar 

desv_est2018<-apply(matriz_pob2018, 2,function(x) sd(x, na.rm=T))
desv_est2013<-apply(matriz_pob2013, 2,function(x) sd(x, na.rm=T))

## coeficiente de variacion

cv_est2018<-apply(matriz_pob2018, 2,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))
cv_est2013<-apply(matriz_pob2013, 2,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))

## con librería reat
cv_est2_2018<-apply(matriz_pob2018, 2, cv, na.rm=TRUE)
cv_est2_2013<-apply(matriz_pob2013, 2, cv, na.rm=TRUE)

## une archivos
estatal_pobocup<-data.frame( varianza_est2013,desv_est2013,
                              cv_est2013,cv_est2_2013,varianza_est2018,desv_est2018,cv_est2018,
                              cv_est2_2018)

### Vacb   
### replicar de la misma forma con la matriz vacb

mat_vacb<-read_xlsx("Matriz SECRE vacb.xlsx",sheet="Sheet1")

##### solo se queda con la matriz de datos


matriz_vacb2018<-mat_vacb[c(1:20),c(4:35)]
matriz_vacb2013<-mat_vacb[c(21:40),c(4:35)]


## sectorial por renglones incluyendo el total Estatal
## varianza

varianza_sect2018<-apply(matriz_vacb2018, 1,function(x) var(x, na.rm=T))
varianza_sect2013<-apply(matriz_vacb2013, 1,function(x) var(x, na.rm=T))

## desviacion estandar 

desv_sect2018<-apply(matriz_vacb2018, 1,function(x) sd(x, na.rm=T))
desv_sect2013<-apply(matriz_vacb2013, 1,function(x) sd(x, na.rm=T))

## coeficiente de variacion

cv_sect2018<-apply(matriz_vacb2018, 1,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))
cv_sect2013<-apply(matriz_vacb2013, 1,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))

## con librería reat
cv_sect2_2018<-apply(matriz_vacb2018, 1, cv, na.rm=TRUE)
cv_sect2_2013<-apply(matriz_vacb2013, 1, cv, na.rm=TRUE)


## une archivos
sectorial_vacb<-data.frame(Sector=labels_sect, varianza_sect2013,desv_sect2013,
                              cv_sect2013,cv_sect2_2013,varianza_sect2018,desv_sect2018,cv_sect2018,
                              cv_sect2_2018)


## estatal por columnas incluyendo el total sectorial

matriz_vacb2018<-mat_vacb[c(1:19),c(4:36)]
matriz_vacb2013<-mat_vacb[c(21:39),c(4:36)]


varianza_est2018<-apply(matriz_vacb2018, 2,function(x) var(x, na.rm=T))
varianza_est2013<-apply(matriz_vacb2013, 2,function(x) var(x, na.rm=T))


## desviacion estandar 

desv_est2018<-apply(matriz_vacb2018, 2,function(x) sd(x, na.rm=T))
desv_est2013<-apply(matriz_vacb2013, 2,function(x) sd(x, na.rm=T))

## coeficiente de variacion

cv_est2018<-apply(matriz_vacb2018, 2,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))
cv_est2013<-apply(matriz_vacb2013, 2,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))

## con librería reat
cv_est2_2018<-apply(matriz_vacb2018, 2, cv, na.rm=TRUE)
cv_est2_2013<-apply(matriz_vacb2013, 2, cv, na.rm=TRUE)

## une archivos
estatal_vacb<-data.frame( varianza_est2013,desv_est2013,
                             cv_est2013,cv_est2_2013,varianza_est2018,desv_est2018,cv_est2018,
                             cv_est2_2018)
### guardar archivos sectorial_vacb y estatal_vacb


### Remuneraciones

### replicar de la misma forma con la matriz vacb
matriz_rem2018<-mat_remun[c(1:20),c(4:35)]
matriz_rem2013<-mat_remun[c(21:40),c(4:35)]


## sectorial por renglones incluyendo el total sectorial
## varianza

varianza_sect2018<-apply(matriz_rem2018, 1,function(x) var(x, na.rm=T))
varianza_sect2013<-apply(matriz_rem2013, 1,function(x) var(x, na.rm=T))

## desviacion estandar 

desv_sect2018<-apply(matriz_rem2018, 1,function(x) sd(x, na.rm=T))
desv_sect2013<-apply(matriz_rem2013, 1,function(x) sd(x, na.rm=T))

## coeficiente de variacion

cv_sect2018<-apply(matriz_rem2018, 1,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))
cv_sect2013<-apply(matriz_rem2013, 1,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))

## con librería reat
cv_sect2_2018<-apply(matriz_rem2018, 1, cv, na.rm=TRUE)
cv_sect2_2013<-apply(matriz_rem2013, 1, cv, na.rm=TRUE)


## une archivos
sectorial_rem<-data.frame(Sector=labels_sect, varianza_sect2013,desv_sect2013,
                           cv_sect2013,cv_sect2_2013,varianza_sect2018,desv_sect2018,cv_sect2018,
                           cv_sect2_2018)


## estatal por columnas incluyendo el total sectorial

matriz_rem2018<-mat_remun[c(1:19),c(4:36)]
matriz_rem2013<-mat_remun[c(21:19),c(4:36)]

varianza_est2018<-apply(matriz_rem2018, 2,function(x) var(x, na.rm=T))
varianza_est2013<-apply(matriz_rem2013, 2,function(x) var(x, na.rm=T))


## desviacion estandar 

desv_est2018<-apply(matriz_rem2018, 2,function(x) sd(x, na.rm=T))
desv_est2013<-apply(matriz_rem2013, 2,function(x) sd(x, na.rm=T))

## coeficiente de variacion

cv_est2018<-apply(matriz_rem2018, 2,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))
cv_est2013<-apply(matriz_rem2013, 2,function(x) sd(x, na.rm=T) / mean(x, na.rm=T))

## con librería reat
cv_est2_2018<-apply(matriz_rem2018, 2, cv, na.rm=TRUE)
cv_est2_2013<-apply(matriz_rem2013, 2, cv, na.rm=TRUE)

## une archivos
estatal_rem<-data.frame( varianza_est2013,desv_est2013,
                          cv_est2013,cv_est2_2013,varianza_est2018,desv_est2018,cv_est2018,
                          cv_est2_2018)
### guardar archivos sectorial_rem y estatal_rem


#### FIN #####

