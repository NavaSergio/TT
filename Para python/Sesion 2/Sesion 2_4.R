##########################################################################################
#########             CONSTRUCCION DE INDICADORES REGIONALES
#########             SESION 2.4  continuacion
#########             COEFICIENTE DE GINI Y DE HOOVER PONDERADOS
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

## usamos la libreria wINEG

install.packages("wINEQ")
library(wINEQ)
#Gini(X, W = rep(1, length(X)), fast = TRUE, rounded.weights = FALSE)

### Gini ponderado estatal

ginpon_2018<-NULL
for (i in 1:32){
  ginp<-Gini(combina_2018[,i],W= combina_2018[,32+i])
  ginpon_2018<-cbind(ginpon_2018,ginp)
}

ginpon_2013<-NULL
for (i in 1:32){
  ginp<-Gini(combina_2013[,i],W= combina_2018[,32+i])
  ginpon_2013<-cbind(ginpon_2013,ginp)
}

### une archivos

labels_est<-c("Aguascalientes","Baja.California", 
              "Baja.California.Sur" , 
              "Campeche", "Coahuila","Colima", "Chiapas",
              "Chihuahua","CDMX", "Durango", "Guanajuato",
              "Guerrero", "Hidalgo", "Jalisco", "Mexico", "Michoacan", 
              "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
              "Queretaro", "Quintana.Roo", "San.Luis.Potosi", "Sinaloa",
              "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
              "Veracruz", "Yucatan" , "Zacatecas" )     


colnames(ginpon_2018)<-labels_est
colnames(ginpon_2013)<-labels_est

ginipon_est<-data.frame(rbind(ginpon_2013,ginpon_2018))
ginipon_est$Año<-c(2013,2018)
ginipon_est<-ginipon_est[,c(33,1:32)]

### gráfica con ggplot2 necesita que los datos estén en formato largo

gini_long<-ginipon_est%>%
  pivot_longer(cols="Aguascalientes":"Zacatecas",
               names_to = "ENTIDAD",
               values_to = "gini_pond")


gini_long$ENTIDAD<-factor(gini_long$ENTIDAD,levels=labels_est)
gini_long$Año<-as.factor(gini_long$Año)

ggplot(data=gini_long,aes(x=ENTIDAD,y=gini_pond,fill=Año))+
  geom_bar(position="dodge", stat="identity")+
  ylab("Gini ponderado")+
  ggtitle("Coeficiente de Gini Estatal ponderado para remuneraciones")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))


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

### gráfica con ggplot2 necesita que los datos estén en formato largo

hoov_long<-hoov_est%>%
  pivot_longer(cols="Aguascalientes":"Zacatecas",
               names_to = "ENTIDAD",
               values_to = "hoover_pond")

hoov_long$ENTIDAD<-factor(hoov_long$ENTIDAD,levels=labels_est)
hoov_long$Año<-as.factor(hoov_long$Año)

ggplot(data=hoov_long,aes(x=ENTIDAD,y=hoover_pond,fill=Año))+
  geom_bar(position="dodge", stat="identity")+
  ylab("Hoover ponderado")+
  ggtitle("Coeficiente de Hoover Estatal ponderado para remuneraciones")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))

##########################################################################################
#####
#####     SEGUNDA PARTE INDICE DE GINI SECTORIAL ####
#####     REQUIERE TRASPONER LAS MATRICES DE REMUNERACIONES Y POBLACION POR SEPARADO
#####     Y LUEGO UNIRLAS POR COLUMNAS
##########################################################################################

labels_sect<-c("11_Agricultura","21_Mineria","22_Elect,agua,gas","23_Construccion","31-33_Manufactura",
               "43_Comercio_mayoreo","46_Comercio_menudeo","48-49_Transp,correo,almac","51_Inf_MediosMas",
               "52_ServFinSeg","53_ServInmob","54_ServProf,Cien,Tec","55_DirecCorp","56_ServApoyo_Remed",
               "61 ServEduc","62_ServSalud","71_ServEsparcCultDep","72_ServAlojAlimBeb","Otros")

labels_sect

pob18_t<-t(matriz_pob2018)
rem18_t<-t(matriz_rem2018)

colnames(rem18_t)<-labels_sect
colnames(pob18_t)<-paste("pob",labels_sect,sep="_")

combina18<-cbind(rem18_t,pob18_t)

gini_pon_2018<-NULL
for (i in 1:19){
  ginp<-Gini(combina18[,i],W= combina18[,i+19])
  gini_pon_2018<-cbind(gini_pon_2018,ginp)
}


pob13_t<-t(matriz_pob2013)
rem13_t<-t(matriz_rem2013)

colnames(rem13_t)<-labels_sect
colnames(pob13_t)<-paste("pob",labels_sect,sep="_")

combina13<-cbind(rem13_t,pob13_t)

gini_pon_2013<-NULL
for (i in 1:19){
  ginp<-Gini(combina13[,i],W= combina13[,i+19])
  gini_pon_2013<-cbind(gini_pon_2013,ginp)
}


ginipon_sect<-data.frame(rbind(gini_pon_2013,gini_pon_2018))
ginipon_sect$Año<-c(2013,2018)
ginipon_sect<-ginipon_sect[,c(20,1:19)]

colnames(ginipon_sect)<-c("Año",labels_sect)


### gráfica con ggplot2 necesita que los datos estén en formato largo

ginisect_long<-ginipon_sect%>%
  pivot_longer(cols="11_Agricultura":"Otros",
               names_to = "Sector",
               values_to = "gini_pond")

ginisect_long$Sector<-factor(ginisect_long$Sector,levels=labels_sect)
ginisect_long$Año<-as.factor(ginisect_long$Año)

ggplot(data=ginisect_long,aes(x=Sector,y=gini_pond,fill=Año))+
  geom_bar(position="dodge", stat="identity")+
  ylab("Gini ponderado")+
  ggtitle("Coeficiente de Gini Sectorial para remuneraciones ponderado")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))


### PARA EL HOOVER SECTORIAL TAMBIÉN TENDRÍA QUE REALIZARSE EL MISMO PROCESO 

############## FIN ##############