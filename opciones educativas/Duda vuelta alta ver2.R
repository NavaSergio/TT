
packages<-c("REAT","readxl", "ggplot2", "psych","GGally","dplyr","tidyverse","groupdata2","sf","spData" ,"spdep","mapview","colorRamps","fs","foreign","xlsx")

if(length(setdiff(packages,rownames(installed.packages())))>0){
  install.packages(setdiff(packages,rownames(installed.packages())),repos="http://cran.rstudio.com")
}

sapply(packages,require,character.only=TRUE)

# Obtener la lista de archivos .RData en el directorio actual
archivos_rdata <- list.files(pattern = "\\.RData$")

# Cargar cada archivo .RData
for (archivo in archivos_rdata) {
  load(archivo)
}

write.csv(altadem_folios,file="altadem_folios.csv")
write.csv(alta,file="alta.csv")
write.csv(demalta_gen1,file="demalta_gen1.csv")
write.csv(oferalt_gen1long,file="oferalt_gen1long.csv")
write.csv(demof_altagen1,file="demof_altagen1.csv")
#demalta_gen1$CVE_OPC<-as.factor(demalta_gen1$CVE_OPC)
#oferta$H<-ceiling(oferta$OFERTA/2)
#oferta$M<-ceiling(oferta$OFERTA/2)

# oferalt_gen1<-oferta%>%filter(CVE_OPC %in% alta)%>%
#   select(CVE_OPC,OFERTA)%>%
#   mutate(H=floor(OFERTA/2),M=ceiling(OFERTA/2))
# 
# oferalt_gen1long<-oferalt_gen1%>%select(CVE_OPC,H,M)%>%
#   pivot_longer( 
#     cols="H":"M",
#     names_to = "SEXO",
#     values_to = "OFERTA" )
# 
# save(oferalt_gen1long,file="oferalt_gen1long.RData")


#oferalt_gen1long$CVE_OPC<-as.character(oferalt_gen1long$CVE_OPC)


# demof_altagen1<-merge(demalta_gen1,oferalt_gen1long,by=c("CVE_OPC","SEXO"),all.x=TRUE)
# demof_altagen1$caben<-ifelse(demof_altagen1$gen_demanda<=demof_altagen1$OFERTA,"asigna","sortea")
# demof_altagen1$dif<-demof_altagen1$OFERTA-demof_altagen1$gen_demanda

# save(demof_altagen1,file="demof_altagen1.RData")
# write.xlsx(demof_altagen1,file="demof_altagen1.xlsx")

### antes hacía algo parecido a esto, que era un sorteo aleatorio, pero ahora como es 
## con cuota de género, se me complica más
## ecoems es la base completa, pero ahora ya tenemos separados los que se quedan en esta ronda


muestra<-alta_dem$OFERTA

asig_alta1 <- vector(mode='list', length=length(alta))

sin_asignar<- vector(mode='list', length=length(alta))
demanda_alta2<- vector(mode='list', length=length(alta))


for (i in 1:length(alta)){
  asig<-ecoems%>%filter(OPC_ED01 == alta[i])
  grupo<-sample(asig$FOLIO,size = muestra[i],replace = FALSE)
  asig_alta1[[i]]<-grupo
  sin_asig<-asig%>%filter(!(FOLIO %in%grupo))%>% select(FOLIO)
  sin_asignar[[i]]<-sin_asig
  demanda_alta2[[i]]<-length(asig$FOLIO)-muestra[[i]]
}

sin_asig_folios<-unlist(sin_asignar, use.names = FALSE)
asig_alta1<-unlist(asig_alta1,use.names = FALSE)
demanda_alta2<-unlist(demanda_alta2,use.names = FALSE)



