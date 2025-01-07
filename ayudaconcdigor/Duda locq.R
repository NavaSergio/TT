#setwd("~/Documents/Traducciones/Roldan/Replica Indices/curso indicadores 2025/1.Base de datos/1.base primera sesión")

# Instala y carga librerías
#install.packages(c("REAT","ggplot2","readxl"))
#library(REAT)

packages<-c("REAT","readxl", "ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","rgdal","spData" ,"spdep","mapview","colorRamps","fs","foreign","xlsx")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)


##Lee archivo excel
matriz<-read_xlsx("Modif base total secre.xlsx")
str(matriz)
#cambia valores vacío por 0
#matriz[is.na(matriz)]<-0


#convierte las variables de caracter a factores 
matriz$Año<-as.factor(matriz$Año)
matriz$Sector<-as.factor(matriz$Sector)

# convierte los vacíos a ceros
matriz[is.na(matriz)]<-0


colnames(matriz)<-c("Año","No_sector","Sector","Aguascalientes","Baja California", 
                    "Baja California Sur" , 
                    "Campeche", "Coahuila","Colima", "Chiapas",
                    "Chihuahua","CDMX", "Durango", "Guanajuato",
                    "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
                    "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
                    "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa",
                    "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                    "Veracruz", "Yucatan" , "Zacatecas","Total_sectorial") 


## Lo siguiente lo hago con la librería REAT
# Puede calcular por celda el coeficiente de localización locq para un estado-sector: matriz[2,4] 
## lo hace tomando el total de los sectores del estado, ,matriz[1,4], 
## el total del sector nacional matriz[2:,36] y el total nacional de todo  matriz[1,36].

## Para calcular el cociente de toda la matriz logré hacerlo con un doble ciclo, 
## también la librería lo puede calcular para todo el estado, digamos que por columna, 
## y solo quedaría en un ciclo para calcular para toda la matriz, pero por alguna razón, 
## no me sale. 

## Te envío el intento que hice que sale bien, cómo lo hace la librería por columnas, 
## y el intento que no me sale. Lo calculo para dos años: 2018 y 2013
## a ver si me puedes echar la mano por favor. Yo creo que sí lo vas a resolver rápido. :) 

## Al final te pongo el ejemplo que usa el artículo de los que hicierono la librería con unos 
## datos de Goettingen


###  METODO 2 USANDO CICLOS 
##Para el año 2018


coefloc_mat<-NULL
for (j in 4:35){
  coeflocal<-NULL
  coefloc<-NULL
  for(i in 2:20){
    coefloc[i]<-locq(matriz[i,j],matriz[1,j],matriz[i,36],matriz[1,36])
    coeflocal<-rbind(coeflocal,coefloc[i])
  }
  coeflocal<-unlist(coeflocal)
  coefloc_mat<-cbind(coefloc_mat,coeflocal)
}

colnames(coefloc_mat)<-c("Aguascalientes","Baja California", 
                         "Baja California Sur" , 
                         "Campeche", "Coahuila","Colima", "Chiapas",
                         "Chihuahua","CDMX", "Durango", "Guanajuato",
                         "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
                         "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
                         "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa",
                         "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                         "Veracruz", "Yucatan" , "Zacatecas") 


### Metodo 1 que no me sale


#convierte la matriz en un archivo de datos para que reconozca los nombres de los renglones

class(matriz)
matriz<-data.frame(matriz)
class(matriz)

locq(matriz$Aguascalientes[2:20],matriz[1,4], matriz$Total_sectorial[2:20], matriz[1,36],
     industry.names = as.character(matriz$No_sector[2:20]))

locq(matriz$Aguascalientes[2:20],matriz[1,4], matriz$Total_sectorial[2:20], matriz[1,36],
     industry.names = as.character(matriz$No_sector[2:20]), plot.results=TRUE, 
     plot.title = "Cociente de localización 2018")

locq(matriz[2:20,4],matriz[1,4], matriz[2:20,36], matriz[1,36],
     industry.names = as.character(matriz$No_sector[2:20]), plot.results=TRUE, 
     plot.title = "Cociente de localización 2018")

locq(matriz[2:20,4],matriz[1,4], matriz[2:20,36], matriz[1,36],
     industry.names = as.character(matriz$No_sector[2:20]), plot.results=TRUE, 
     plot.title = "Cociente de localización 2018")


###################################
# Esta sería la versión original, con la menor cantidad de cambios posible que reproduce coefloc_mat

coeflocal<-NULL
for(i in 4:35){
  coefloc<-locq(unlist(matriz[2:20,i]),unlist(matriz[1,i]),unlist(matriz[2:20,36]),unlist(matriz[1,36]))
  coeflocal<-cbind(coeflocal,coefloc)
}
rownames(coeflocal) <- matriz$No_sector[2:20]
colnames(coeflocal) <- colnames(matriz)[4:35]
coeflocal
######################################



##### Ejemplo del artículo que produce las gráficas el que yo hago no las produce

#Using the REAT function locq(), we calculate a location quotient for Gottingen with
#respect to the manufacturing industry ("Verarbeitendes Gewerbe"), which is represented
#by letter C:
  locq (Goettingen$Goettingen2017[4], Goettingen$Goettingen2017[1],
        Goettingen$BRD2017[4], Goettingen$BRD2017[1])
# Industry: manufacturing (letter C) in row 4
# row 1 = all-over employment#
#[1] 0.5369
#The output is simply the LQ value (LQij , where i is manufacturing and j is Gottingen).
#We see that the LQ is very low, indicating that manufacturing is underrepresented in
#Gottingen as compared to Germany. Now, we calculate LQ values for all industries
#(A-R), including a simple plot (function argument plot.results = TRUE):
 
   locq (Goettingen$Goettingen2017[2:16], Goettingen$Goettingen2017[1],
        Goettingen$BRD2017[2:16], Goettingen$BRD2017[1],
        industry.names = Goettingen$WZ2008_Code[2:16], plot.results = TRUE,
        plot.title = "Location quotients for Gottingen 2017")
# all industries (rows 2-16 in the dataset)