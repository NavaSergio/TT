##########################################################################################
#########             CONSTRUCCION DE INDICADORES REGIONALES
#########             SESION 4
#########             PROYECCIONES POBLACIONALES Y GRAFICAS
#########             
#########             ENERO 2025
##########################################################################################


## limpia el ambiente de trabajo
rm(list = ls())

setwd("~/Documents/Sesion 4 Graficas")

packages<-c("readxl", "ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","rgdal","spData" ,"spdep","mapview","colorRamps","fs","foreign","xlsx")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)

proyeccion<-read_xlsx("Proyecciones poblacionales 2024_2035.xlsx",sheet="Sheet1")

proyeccion[sapply(proyeccion, is.character)] <- lapply(proyeccion[sapply(proyeccion, is.character)], 
                                               as.factor)

## calcular estadísticas descriptivas para todas las columnass 

summary(proyeccion)

## con libreria psych

describe(proyeccion)

###cambiar a formato largo

proy_long <- proyeccion %>% 
  pivot_longer(
    cols = "Hombres_2024":"Total_2035", 
    names_to = "Año_sexo",
    values_to = "Poblacion"
  )

proy_long <- separate(proy_long, col = Año_sexo, into = c("Sexo", "Año"), sep = "_")

proy_long[sapply(proy_long, is.character)] <- lapply(proy_long[sapply(proy_long, is.character)], 
                                                       as.factor)

levels(proy_long$Año)

levels(proy_long$Sexo)
levels(proy_long$CV_ENT)
levels(proy_long$Entidad)

table(proy_long$Entidad,proy_long$CV_ENT,exclude=NULL)

proy_long$Entidad<-factor(proy_long$Entidad,levels=c( "Aguascalientes","Baja California", "Baja California Sur" , "Campeche", 
                                                      "Coahuila de Zaragoza","Colima", "Chiapas","Chihuahua","Ciudad de México", 
                                                      "Durango", "Guanajuato",
                                                      "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo", 
                                                      "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                                                      "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
                                                      "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                                                      "Veracruz de Ignacio de la Llave", "Yucatán" , "Zacatecas" )     
)
levels(proy_long$Entidad)


proy_estatal<-proy_long%>%
  group_by(CV_ENT,Entidad,Año,Sexo)%>%
  summarise(Pob_Est=sum(Poblacion),Pob_prom=mean(Poblacion),
            desvstd=sd(Poblacion))

proy_estatal$i<-as.numeric(proy_estatal$CV_ENT)

table(proy_estatal$CV_ENT,proy_estatal$i,exclude=NULL)

#install.packages("scales")
library("scales")

labels.entidad<- c( "Aguascalientes","Baja California", "Baja California Sur" , "Campeche", 
                    "Coahuila de Zaragoza","Colima", "Chiapas","Chihuahua","Ciudad de México", 
                    "Durango", "Guanajuato","Guerrero", "Hidalgo", "Jalisco", "México", 
                    "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                    "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
                    "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                    "Veracruz de Ignacio de la Llave", "Yucatán" , "Zacatecas" )     


#labels.entidad<-levels(proy_estatal$Entidad)

#version 1 para un estado

for (j in 1:1){
  p=proy_estatal%>%filter(i==j)%>%
  ggplot(aes(x=Año,y=Pob_Est,group=Sexo,color=Sexo))+
  geom_line(aes(linetype = Sexo))+
  geom_point(aes(shape=Sexo))+
  scale_shape_manual(values=c(19, 17, 15))+
  scale_color_manual(values=c('#ff33ff','#0033ff', 'black'))+
  xlab("Año")+
  ylab("Población")+
  scale_y_continuous(label=comma)+
  ggtitle(paste("Proyección de la población de 17 a 19 años \n",labels.entidad[j]))+
  theme_bw() + theme( plot.title = element_text(hjust = 0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.title = element_blank(),
                      panel.border = element_rect(size=1),
                      legend.position = c(0.9, 0.35))
  
  print(p)  
}

#version 2 modifica el área del gráfico 

for (j in 1:1){
  p=proy_estatal%>%filter(i==j)%>%
    ggplot(aes(x=Año,y=Pob_Est,group=Sexo,color=Sexo))+
    geom_line(aes(linetype = Sexo))+
    geom_point(aes(shape=Sexo))+
    scale_shape_manual(values=c(19, 17, 15))+
    scale_color_manual(values=c('magenta','blue', 'black'))+
    xlab("Año")+
    ylab("Población")+
    scale_y_continuous(label=comma)+
    ggtitle(paste("Proyección de la población de 17 a 19 años \n",labels.entidad[j]))+
    theme_bw() + theme( plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.title = element_blank(),
                        panel.border = element_rect(size=1),
                        legend.position = c(0.9, 0.35))
  print(p)  
  
}

##version 3 para guardar en pdf

for (j in 1:1){
  pdf(paste("ProyeccionPob_Estado_X",j,".pdf"))
  p=proy_estatal%>%filter(i==j)%>%
  ggplot(aes(x=Año,y=Pob_Est,group=Sexo,color=Sexo))+
  geom_line(aes(linetype = Sexo))+
  geom_point(aes(shape=Sexo))+
  scale_shape_manual(values=c(19, 17, 15))+
  scale_color_manual(values=c('magenta','blue', 'black'))+
  xlab("Año")+
  ylab("Población")+
  scale_y_continuous(label=comma)+
  ggtitle(paste("Proyección de la población de 17 a 19 años \n",labels.entidad[j]))+
  theme_bw() + theme( plot.title = element_text(hjust = 0.5),
  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.title = element_blank(),
                  panel.border = element_rect(size=1),
                  legend.position = c(0.9, 0.35))
  print(p)  
dev.off()
}


#version 4 para todos los estados 
for (j in 1:32){
  p=proy_estatal%>%filter(i==j)%>%
    ggplot(aes(x=Año,y=Pob_Est,group=Sexo,color=Sexo))+
    geom_line(aes(linetype = Sexo))+
    geom_point(aes(shape=Sexo))+
    scale_shape_manual(values=c(19, 17, 15))+
    scale_color_manual(values=c('magenta','blue', 'black'))+
    xlab("Año")+
    ylab("Población")+
    scale_y_continuous(label=comma)+
    ggtitle(paste("Proyección de la población de 17 a 19 años \n",labels.entidad[j]))+
    theme_bw() + theme( plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.title = element_blank(),
                        panel.border = element_rect(size=1),
                        legend.position = c(0.9, 0.35))
  print(p)  
 
}

#version 5 para todos los estados guardando en un pdf por estado
for (j in 1:32){
  pdf(paste("ProyeccionPob_Estado_",j,".pdf"),height=8, width=11)
  p=proy_estatal%>%filter(i==j)%>%
    ggplot(aes(x=Año,y=Pob_Est,group=Sexo,color=Sexo))+
    geom_line(aes(linetype = Sexo))+
    geom_point(aes(shape=Sexo))+
    scale_shape_manual(values=c(19, 17, 15))+
    scale_color_manual(values=c('magenta','blue', 'black'))+
    xlab("Año")+
    ylab("Población")+
    scale_y_continuous(label=comma)+
    ggtitle(paste("Proyección de la población de 17 a 19 años \n",labels.entidad[j]))+
    theme_bw() + theme( plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.title = element_blank(),
                        panel.border = element_rect(size=1),
                        legend.position = c(0.9, 0.35))
  print(p)  
  dev.off()
}

#version 6 para todos los estados guardando en un solo pdf

pdf("ProyeccionPob_Todos.pdf",height=8, width=11)
for (j in 1:32){
  
  p=proy_estatal%>%filter(i==j)%>%
    ggplot(aes(x=Año,y=Pob_Est,group=Sexo,color=Sexo))+
    geom_line(aes(linetype = Sexo))+
    geom_point(aes(shape=Sexo))+
    scale_shape_manual(values=c(19, 17, 15))+
    scale_color_manual(values=c('magenta','blue', 'black'))+
    xlab("Año")+
    ylab("Población")+
    scale_y_continuous(label=comma)+
    ggtitle(paste("Proyección de la población de 17 a 19 años \n",labels.entidad[j]))+
    theme_bw() + theme( plot.title = element_text(hjust = 0.5),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.title = element_blank(),
                        panel.border = element_rect(size=1),
                        legend.position = c(0.9, 0.35))
  print(p)  
 
}
dev.off()


####### FIN ######