
#########################################################################################################
#########
#########             CONSTRUCCION DE INDICADORES REGIONALES
#########             SESION 1.2
#########             COEFICIENTE DE LOCALIZACION
#########             ENERO 2025
#########
#########################################################################################################

#Selecciona Directorio de trabajo. No es necesario si los datos están  en la misma ruta que el portafolio
#setwd("~/Documents/Traducciones/Roldan/Replica Indices/curso indicadores 2025/1.Base de datos/1.base primera sesión")

#setwd("~/Documents/base pasp")

setwd("~/Documents/Sesion 1")

# Instala y carga librerías
#install.packages(c("REAT","ggplot2","readxl"))
#library(REAT)

packages<-c("REAT","readxl", "xlsx","ggplot2", "psych","dplyr","tidyverse","colorRamps","fs","foreign")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)


##Lee archivo excel
matriz<-read_xlsx("Modif base total secre.xlsx")
str(matriz)


#convierte las variables de caracter a factores 
#matriz$Año<-as.factor(matriz$Año)
matriz$Sector<-as.factor(matriz$Sector)
matriz$No_sector <- str_pad(matriz$No_sector, width = 2, pad = "0")

## con la librería dplyr
#convierte las variables de caracter a factores 
matriz$No_sector<-as.factor(matriz$No_sector)
# convierte los vacíos a ceros
#matriz[is.na(matriz)]<-0

colnames(matriz)<-c("Año","No_sector","Sector", "Aguascalientes","Baja California", 
                             "Baja California Sur" , 
                             "Campeche", "Coahuila","Colima", "Chiapas",
                             "Chihuahua","CDMX", "Durango", "Guanajuato",
                             "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
                             "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
                             "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa",
                             "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                             "Veracruz", "Yucatan" , "Zacatecas" , "Total_Sectorial")     



# convierte la matriz en formato largo para realizar los cálculos de manera mas facil 
matriz[is.na(matriz)]<-0


matriz_long<-matriz[c(-1,-21),-36]%>%
  pivot_longer(
    cols="Aguascalientes":"Zacatecas",
    names_to = "ENTIDAD",
    values_to = "Pers_ocup" )

matriz_long$ENTIDAD<-as.factor(matriz_long$ENTIDAD)

matpob_2018<-matriz_long[matriz_long$Año==2018,]
matpob_2013<-matriz_long[matriz_long$Año==2013,]



### COEFICIENTE DE LOCALIZACION HOOVER-BALASSA version para matriz

local2_2018<-locq2(e_ij = matpob_2018$Pers_ocup,
                   industry.id = matpob_2018$Sector, 
                   region.id = matpob_2018$ENTIDAD)


local2_2013<-locq2(e_ij = matpob_2013$Pers_ocup,
                   industry.id = matpob_2013$Sector, 
                   region.id = matpob_2013$ENTIDAD)


localhoover_bal2<-data.frame(rbind(cbind(Año=rep(2018,19),local2_2018),cbind(Año=rep(2013,19),local2_2013)))

save(localhoover_bal2,file="Localizacion facil.RData")

write.xlsx(local2,file="Cociente de localizacion.xlsx")

write.csv(localhoover_bal2,file="Cociente de Localizacion facil.csv")


### grafica o mapa de calor 

heatmap(local2_2013,Rowv=NA,Colv=NA)

heatmap(local2_2018,Rowv=NA,Colv=NA)


### Indice de especialización Q^R (Hoover ) para poblacion

matriz<-read_xlsx("Modif base total secre.xlsx")
str(matriz)


colnames(matriz)<-c("Año","No_sector","Sector", "Aguascalientes","Baja California", 
                    "Baja California Sur" , 
                    "Campeche", "Coahuila","Colima", "Chiapas",
                    "Chihuahua","CDMX", "Durango", "Guanajuato",
                    "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
                    "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
                    "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa",
                    "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                    "Veracruz", "Yucatan" , "Zacatecas" , "Total_Sectorial")     



matriz_pob2018<-matriz[c(2:20),c(4:36)]
matriz_pob2013<-matriz[c(22:40),c(4:36)]


hoover_2018<-round(apply(matriz_pob2018[,c(1:32)],2,hoover,ref=matriz_pob2018$Total_Sectorial,na.rm=TRUE),4)
hoover_2013<-round(apply(matriz_pob2013[,c(1:32)],2,hoover,ref=matriz_pob2013$Total_Sectorial,na.rm=TRUE),4)
hoover_2013
hoover_2018
#hoover_13ags<-hoover(matriz_pob2013$Aguascalientes,ref=matriz_pob2013$Total_Sectorial,na.rm=TRUE)

hooverdf<-data.frame(cbind(hoover_2018,hoover_2013))

save(hooverdf,file="Hooverdf_pob.RData")

write.xlsx(hooverdf,file="Coeficiente Hoover Pob Ocupada.xlsx")
write.csv(hooverdf,file="Coeficiente Hoover Pob Ocupada.csv")

### Para remuneraciones 
mat_remun<-read_xlsx("Matriz SECRE Remuneraciones.xlsx")
str(mat_remun)

matriz_rem2018<-matriz[c(1:19),c(4:36)]
matriz_rem2013<-matriz[c(21:39),c(4:36)]

hoover_rem2018<-round(apply(matriz_rem2018[,c(1:32)],2,hoover,ref=matriz_rem2018$Total_Sectorial,na.rm=TRUE),4)
hoover_rem2013<-round(apply(matriz_rem2013[,c(1:32)],2,hoover,ref=matriz_rem2013$Total_Sectorial,na.rm=TRUE),4)
hoover_rem2013
hoover_rem2018

hoover_remdf<-data.frame(cbind(hoover_rem2018,hoover_rem2013))
hoover_remdf$Entidad<-row.names(hoover_remdf)

#Grafica de dispersión 
plot(hoover_remdf$hoover_rem2013,hoover_remdf$hoover_rem2018, 
     main="Indice de Hoover para remuneraciones 2013 vs 2018",
     xlab="Hoover remuneraciones 2013",ylab="Hoover remuneraciones 2018")
text(hoover_remdf$hoover_rem2013,hoover_remdf$hoover_rem2018,hoover_remdf$Entidad)

save(hoover_remdf,file="Hooverdf_rem.RData")

write.xlsx(hoover_remdf,file="Coeficiente Hoover Remuneraciones.xlsx")
write.csv(hoover_remdf,file="Coeficiente Hoover Remuneraciones.csv")

#### FIN ####
