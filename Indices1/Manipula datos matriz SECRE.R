


#Selecciona Directorio de trabajo 
#setwd("~/Documents/Traducciones/Roldan/Replica Indices/Datos")

#librerías y datos 
#install.packages(c("REAT","ggplot2","readxl"))
#library(REAT)

packages<-c("REAT","readxl", "ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","rgdal","spData" ,"spdep","mapview","colorRamps","fs","foreign","xlsx")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)



##Lee archivo excel
matriz<-read_xlsx("Matriz SECRE.xlsx")
str(matriz)
#cambia valores vacío por 0
matriz[is.na(matriz)]<-0

matriz_est<-matriz[,c(1:35)]

matriz_est$gini_sect<- apply(matriz_est[,c(4:35)], 1, gini)
matriz_est$gini_ind<-NULL
apply(matriz_est[,c(4:36)], 2, gini,lc=TRUE)

#transforma la matriz en un arreglo largo
matriz_long <- matriz %>% 
  pivot_longer(
    cols = "1":"TOTAL SECTORIAL", 
    names_to = "CVE_ENT",
    values_to = "Personal ocupado"
  )

str(matriz_long)

matriz_long_wide<-matriz_long[,c(1,4,5)]%>%
  pivot_wider(
    names_from = "j",
    values_from="Personal ocupado"
  )

matriz_long_wide$gini_est<- apply(matriz_long_wide[,c(2:21)], 1, gini)

matriz_est<-rbind(matriz_est,c("gini_est","Estatal","ESTATAL",matriz_long_wide$gini_est))
matriz_est<-matriz_est[c(1:20,22),]



Entidad=c( "Aguascalientes","Baja California", "Baja California Sur" , 
           "Campeche", "Coahuila de Zaragoza","Colima", "Chiapas",
           "Chihuahua","Ciudad de México", "Durango", "Guanajuato",
           "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo", 
           "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
           "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
           "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
           "Veracruz de Ignacio de la Llave", "Yucatán" , "Zacatecas" )     


str(matriz_estatal)

matriz_estatal$CVE_ENT<-as.numeric(matriz_estatal$CVE_ENT)

for (i in 1:32){
  matriz_estatal$ENTIDAD[matriz_estatal$CVE_ENT==i]<-Entidad[i]
}

### ejemplos del artículo para la libreria REAT 

data("GoettingenHealth2")

head(GoettingenHealth2)

str(GoettingenHealth2)


# Coeficiente de Gini en salud

#1) Dispersion de médicos  grafica la curva de Lorenz

gini(GoettingenHealth2$phys_gen,lc=TRUE)
#[1] 0.8386269

                                     