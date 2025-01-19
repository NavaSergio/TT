##########################################################################################
#########             CONSTRUCCION DE INDICADORES REGIONALES
#########             SESION 4, CODIGO 2_5 
#########             MULTIPLICADORES, COEFICIENTES DE ESPECIALIZACION
#########             INDICES DE AGLOMERACION Y COAGLOMERACION, 
#########             ENERO 2025
##########################################################################################

### PRIMERO CORRER EL CÓDIGO 2_4

## limpia el ambiente de trabajo
rm(list = ls())

#Selecciona Directorio de trabajo. No es necesario si los datos están  en la misma ruta que el portafolio

setwd("~/Documents/Sesion 2")

## usamos la libreria wINEG 

packages<-c("REAT","wINEQ","readxl", "ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","spData" ,"spdep","mapview","colorRamps","fs","foreign","xlsx")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)


##Lee archivo de poblacion ocupada
mat_pobocup<-read_xlsx("Modif base total secre pobocup.xlsx")
str(mat_pobocup)

## Lee archivo excel de remuneraciones 
mat_remun<-read_xlsx("Matriz SECRE Remuneraciones.xlsx",sheet="Sheet1")


##### solo se queda con la matriz de datos

matriz_pob2018<-mat_pobocup[c(1:19),c(4:35)]
matriz_pob2013<-mat_pobocup[c(21:39),c(4:35)]

matriz_rem2018<-mat_remun[c(1:19),c(4:35)]
matriz_rem2013<-mat_remun[c(21:39),c(4:35)]

colnames(matriz_pob2018) <- paste("pobocup", colnames(matriz_pob2018), sep = "_")
colnames(matriz_rem2018) <- paste("rem", colnames(matriz_rem2018), sep = "_")

colnames(matriz_pob2013) <- paste("pobocup", colnames(matriz_pob2013), sep = "_")
colnames(matriz_rem2013) <- paste("rem", colnames(matriz_rem2013), sep = "_")

## Necesitamos unir las bases de datos de las remuneraciones y las poblaciones 
combina_2018<-cbind(matriz_rem2018, matriz_pob2018)

combina_2013<-cbind(matriz_rem2013, matriz_pob2013)


labels_est<-c("Aguascalientes","Baja.California", 
              "Baja.California.Sur" , 
              "Campeche", "Coahuila","Colima", "Chiapas",
              "Chihuahua","CDMX", "Durango", "Guanajuato",
              "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
              "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
              "Queretaro", "Quintana.Roo", "San.Luis.Potosi", "Sinaloa",
              "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
              "Veracruz", "Yucatan" , "Zacatecas" )     

### Hoover de concentración ponderado estatal por la poblacion ocupada

hoovpon_2018<-NULL
for (i in 1:32){
  hov_pon<-hoover(combina_2018[,i],ref = combina_2018[,32+i],na.rm=TRUE)
  hoovpon_2018<-cbind(hoovpon_2018,hov_pon)
}

hoovpon_2013<-NULL
for (i in 1:32){
  hov_pon<-hoover(combina_2013[,i],ref = combina_2013[,32+i],na.rm=TRUE)
  hoovpon_2013<-cbind(hoovpon_2013,hov_pon)
}


colnames(hoovpon_2018)<-labels_est
colnames(hoovpon_2013)<-labels_est

hoov_est<-data.frame(rbind(hoovpon_2013,hoovpon_2018))
hoov_est$Año<-c(2013,2018)
hoov_est<-hoov_est[,c(33,1:32)]


###Multiplicadores de remuneraciones
#trasponer el archivo de hoover ponderado

thoov_est<-data.frame(t(hoov_est))

thoov_est<-thoov_est[-1,]
colnames(thoov_est)<-c("Hoov_2013","Hoov_2018")
thoov_est$Mult_2013<-1/thoov_est$Hoov_2013
thoov_est$Mult_2018<-1/thoov_est$Hoov_2018


### Coeficientes de especialización  

matriz_pob2018<-mat_pobocup[c(1:19),c(4:36)]
matriz_pob2013<-mat_pobocup[c(21:39),c(4:36)]

matriz_rem2018<-mat_remun[c(1:19),c(4:36)]
matriz_rem2013<-mat_remun[c(21:39),c(4:36)]

matriz_pob2018[is.na(matriz_pob2018)]<-0
matriz_pob2013[is.na(matriz_pob2013)]<-0

matriz_rem2018[is.na(matriz_rem2018)]<-0
matriz_rem2013[is.na(matriz_rem2013)]<-0


ginispec_pob2018<-apply(matriz_pob2018[,c(1:32)],2,gini.spec,matriz_pob2018$TOTAL_SECTORIAL)
ginispec_pob2013<-apply(matriz_pob2013[,c(1:32)],2,gini.spec,matriz_pob2013$TOTAL_SECTORIAL)


krugspec_pob2018<-apply(matriz_pob2018[,c(1:32)],2,krugman.spec,matriz_pob2018$TOTAL_SECTORIAL)
krugspec_pob2013<-apply(matriz_pob2013[,c(1:32)],2,krugman.spec,matriz_pob2013$TOTAL_SECTORIAL)

### para las remuneraciones 
ginispec_rem2018<-apply(matriz_rem2018[,c(1:32)],2,gini.spec,matriz_rem2018$TOTAL_SECTORIAL)
ginispec_rem2013<-apply(matriz_rem2013[,c(1:32)],2,gini.spec,matriz_rem2013$TOTAL_SECTORIAL)

krugspec_rem2018<-apply(matriz_rem2018[,c(1:32)],2,krugman.spec,matriz_rem2018$TOTAL_SECTORIAL)
krugspec_rem2013<-apply(matriz_rem2013[,c(1:32)],2,krugman.spec,matriz_rem2013$TOTAL_SECTORIAL)


espec<-data.frame(rbind(ginispec_pob2013,krugspec_pob2013,ginispec_rem2013,krugspec_rem2018,
                             ginispec_pob2018,krugspec_pob2018,ginispec_rem2018,krugspec_rem2018))


### conc calcula el hoover, gini y krugman de los vectores y los concatena en una matriz



##### solo se queda con la matriz de datos

matriz_pob<-mat_pobocup[c(1:19,21:39),c(1:35)]


## con la librería dplyr
#convierte las variables de caracter a factores 
matriz_pob$Año<-as.factor(matriz_pob$Año)
matriz_pob$Sector<-as.factor(matriz_pob$Sector)
matriz_pob$No_sector <- str_pad(matriz_pob$No_sector, width = 2, pad = "0")
matriz_pob$No_sector<-as.factor(matriz_pob$No_sector)
# convierte los vacíos a ceros
#matriz_pob[is.na(matriz_pob)]<-0

# convierte la matriz en formato largo para realizar los cálculos de manera mas facil 

matriz_longpob<-matriz_pob%>%
  pivot_longer(
    cols="Aguascalientes":"Zacatecas",
    names_to = "ENTIDAD",
    values_to = "Pers_ocup" )

matpob_2018<-matriz_longpob[matriz_longpob$Año=="2018",]

matpob_2013<-matriz_longpob%>%filter(Año=="2013")

conc_2018 <- conc (e_ij = matpob_2018$Pers_ocup,
                   industry.id = matpob_2018$Sector,
                   region.id = matpob_2018$ENTIDAD,na.rm = TRUE)

conc_2013 <- conc (e_ij = matpob_2013$Pers_ocup,
                   industry.id = matpob_2013$Sector,
                   region.id = matpob_2013$ENTIDAD,na.rm=TRUE)




#### disp calcula todos los índices simultaneamente 

indpob_2013<-disp(matriz_pob2013[,-33])

indpob_2018<-disp(matriz_pob2018[,-33])
indpob_2018<-data.frame(disp(matriz_pob2018[,-33]))
indpob_2013<-data.frame(disp(matriz_pob2013[,-33]))

indrem_2018<-data.frame(disp(matriz_rem2018[,-33]))
indrem_2013<-data.frame(disp(matriz_rem2013[,-33]))

write.xlsx(indpob_2013,file="Indicadores regionales estales poblacion 2013.xlsx")


###### Calcula algunas operaciones por columnas y renglones para la matriz de remuneraciones

matriz_rem2013

total_estatal<-colSums (matriz_rem2013, na.rm = TRUE)
matriz_rem2013<-rbind(matriz_rem2013,total_estatal)

matriz_rem2013$total_sect<-rowSums (matriz_rem2013[,c(1:32)], na.rm = TRUE)

#media_estatal
# quitar renglon 21 matriz_rem2013<-matriz_rem2013[-21,]

matriz_rem2013<-rbind(matriz_rem2013,colMeans(matriz_rem2013[c(1:19),], na.rm = TRUE))
matriz_rem2013$media_sectorial<-rowMeans(matriz_rem2013[,c(1:32)], na.rm = TRUE)

summary(matriz_rem2013[c(1:19),(1:32)])

#con la librería psych

sd(matriz_rem2013$Aguascalientes[1:19],na.rm=TRUE)
var(matriz_rem2013$Aguascalientes[1:19],na.rm=TRUE)

describe(matriz_rem2013[c(1:19),c(1:32)])
