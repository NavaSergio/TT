##########################################################################################
#########             CONSTRUCCION DE INDICADORES REGIONALES
#########             SESION 2.1
#########             COEFICIENTE DE GINI
#########             ENERO 2025
##########################################################################################
#limpiar consola  ^L

## limpia el ambiente de trabajo
rm(list = ls())


#Selecciona Directorio de trabajo. No es necesario si los datos están  en la misma ruta que el portafolio

setwd("~/Documents/Sesion 2")

packages<-c("REAT","readxl", "ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","spData" ,"spdep","mapview","colorRamps","fs","foreign","xlsx")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)


##Lee archivo excel 
mat_pobocup<-read_xlsx("Modif base total secre pobocup.xlsx")
str(mat_pobocup)


##### solo se queda con la matriz de datos

matriz_pob2018<-mat_pobocup[c(1:19),c(4:35)]
matriz_pob2013<-mat_pobocup[c(21:39),c(4:35)]

#Otra forma de Elimina los totales para quedarse solo con los datos estatales-sectoriales
## con la librería dplyr
#matriz_pob2018<-mat_pobocup%>%filter(!(No_sector==20)& Año==2018)
#matriz_pob2013<-mat_pobocup%>%filter(!(No_sector==20)& Año==2013)


## Lee archivo excel de remuneraciones 
mat_remun<-read_xlsx("Matriz SECRE Remuneraciones.xlsx",sheet="Sheet1")


##### solo se queda con la matriz de datos

matriz_rem2018<-mat_remun[c(1:19),c(4:35)]
matriz_rem2013<-mat_remun[c(21:39),c(4:35)]

### 1. Calcula índices de gini Sectorial por renglones sin ponderar 

gini_pobsect2018<- round(apply(matriz_pob2018, 1, gini,na.rm=TRUE,lc=TRUE),4)

gini_pobsect2013<- round(apply(matriz_pob2013, 1, gini,na.rm=TRUE,lc=TRUE),4)

## une por columnas en un data frame/archivo de datos

## une por columnas en un data frame/archivo de datos
labels_sect<-mat_pobocup[c(1:19),3]
#labels_sect<-mat_pobocup$Sector[c(1:19)]

gini_pobsect<-data.frame(cbind(Sector=labels_sect,gini_pobsect2018,gini_pobsect2013))

###guardar en RData, csv, xlsx


### Calcula índices de gini Estatal por columnas

gini_pobest2018<- round(apply(matriz_pob2018, 2, gini,na.rm=TRUE,lc=TRUE),4)
gini_pobest2013<- round(apply(matriz_pob2013, 2, gini,na.rm=TRUE,lc=TRUE),4)


gini_pobest<-data.frame(cbind(gini_pobest2018,gini_pobest2013))


###  2. Indice de Gini para el valor agregado censal bruto (vacb)


mat_vacb<-read_xlsx("Matriz SECRE vacb.xlsx",sheet="Sheet1")

##### solo se queda con la matriz de datos

matriz_vacb2018<-mat_vacb[c(1:19),c(4:35)]
matriz_vacb2013<-mat_vacb[c(21:39),c(4:35)]


#Otra forma de Elimina los totales para quedarse solo con los datos estatales-sectoriales
## con la librería dplyr
#matriz_vacb2018<-mat_vacb%>%filter(!(No_sector==20)& Año==2018)
#matriz_dat2013<-mat_vacb%>%filter(!(No_sector==20)& Año==2013)

### Calcula índices de gini Sectorial por renglones 

ginivacb_sect2018<- round(apply(matriz_vacb2018, 1, gini,na.rm=TRUE),4)
ginivacb_sect2013<- round(apply(matriz_vacb2013, 1, gini,na.rm=TRUE),4)

## une por columnas en un data frame/archivo de datos
gini_vacbsect<-data.frame(cbind(Sector=labels_sect,ginivacb_sect2018,ginivacb_sect2013))

### Calcula índices de gini Estatal por columnas

ginivacb_est2018<- round(apply(matriz_vacb2018, 2, gini,na.rm=TRUE),4)
ginivacb_est2013<- round(apply(matriz_vacb2013, 2, gini,na.rm=TRUE),4)

## une por columnas en un data frame/archivo de datos
gini_vacbest<-data.frame(cbind(ginivacb_est2018,ginivacb_est2013))


###  3. Indice de Gini para las remuneraciones

mat_remun<-read_xlsx("Matriz SECRE Remuneraciones.xlsx",sheet="Sheet1")


##### solo se queda con la matriz de datos

matriz_rem2018<-mat_remun[c(1:19),c(4:35)]
matriz_rem2013<-mat_remun[c(21:39),c(4:35)]


### Calcula índices de gini Sectorial por renglones 

ginirem_sect2018<- round(apply(matriz_rem2018, 1, gini,na.rm=TRUE),4)
ginirem_sect2013<- round(apply(matriz_rem2013, 1, gini,na.rm=TRUE),4)

#gini_ags_2018<-gini(matriz_rem2018$Aguascalientes ,weighting = matriz_pob2018$pobocup_Aguascalientes,na.rm = TRUE)

#combina<-cbind(matriz_rem2018,matriz_pob2018)

#gini_ags_2018<-gini(combina$Aguascalientes,weighting = combina$pobocup_Aguascalientes,na.rm = TRUE)
#gini_sect12018<-gini(combina[1,c(1:32)],weighting = combina[1,c(33:64)],na.rm=TRUE,lc=TRUE)


## une por columnas en un data frame/archivo de datos
gini_remsect<-data.frame(cbind(Sector=labels_sect,ginirem_sect2018,ginirem_sect2013))

### Calcula índices de gini Estatal por columnas

ginirem_est2018<- round(apply(matriz_rem2018, 2, gini,na.rm=TRUE),4)
ginirem_est2013<- round(apply(matriz_rem2013, 2, gini,na.rm=TRUE),4)

## une por columnas en un data frame/archivo de datos
gini_remest<-data.frame(cbind(ginirem_est2018,ginirem_est2013))


### une los archivos  

gini_sectorial<-data.frame(cbind(gini_pobsect,gini_vacbsect[,c(2,3)],gini_remsect[,c(2,3)]))

## tareita exportar/guardar en R y en csv o excel 

gini_estatal<-data.frame(cbind(gini_pobest,gini_vacbest,gini_remest))

##  guardar archivos en datos R 

save(gini_sectorial,file="gini_sectorial.RData")
save(gini_estatal,file="gini_estatal.RData")

########FIN ################


############  ETIQUETAS DE ESTADOS Y SECTORES 

#### Vectores de etiquetas de estados y sectores

labels_est<-c("Aguascalientes","Baja California", 
              "Baja California Sur" , 
              "Campeche", "Coahuila","Colima", "Chiapas",
              "Chihuahua","CDMX", "Durango", "Guanajuato",
              "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
              "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
              "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa",
              "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
              "Veracruz", "Yucatan" , "Zacatecas" )     


