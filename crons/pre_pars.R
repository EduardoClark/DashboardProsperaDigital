###Preload data###
source("loadLibraries.R")
source("./aux.R")

UM <- read.csv("./data/ec_muestra_20160203.csv",stringsAsFactors = FALSE, fileEncoding = "Windows-1252") %>% subset(cl_treatmentArm!=0) 
MSJ <- read.csv("./data/mensajes_pd_rp.csv",stringsAsFactors=F,header=T)
CTC <- read.csv("./data/contacts.csv",stringsAsFactors=F,header=T)
RNS <- read.csv("./data/runs.csv",,stringsAsFactors=F, header=T)
CLUES <- read.csv("./data/CAT_CLUES_Febrero2016.csv",stringsAsFactors=F,header=T)
CLUES <- CLUES[,c('CLUES','CLAVE.ENTIDAD','CLAVE.MUNICIPIO')]
CLUES$mun <- paste(CLUES$CLAVE.ENTIDAD, CLUES$CLAVE.MUNICIPIO, sep="_")
names(UM)[21:22] <- c("lon","lat")

pre_pars <- c(
  'Beneficiarias' = sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera)),
  'UnidadesMedicas' = length(get_unique_clues(CTC)[,1]),
  'Personal' = length(get_unique_clues(CTC)[,1]),
  'Estados' = length(get_unique_states(CTC))-1,
  'Municipios' = length(unique(CLUES[CLUES$CLUES%in%get_unique_clues(CTC)[,1],'mun'])),
  'CasosMIALERTA' = length(unique(RNS[
    RNS[,"flow_name"]=="miAlerta" & RNS[,"completed"]=="True","contact"] )),
  'CambiosMICITA' = length(unique(RNS[
    grepl("miCita_bf",RNS[,"flow_name"]) & RNS[,"completed"]=="True","contact"] )),
  'Usuarios' = nrow(CTC),
  'Mensajes' = nrow(MSJ),
  'Enviados' = nrow(MSJ[MSJ$direction=='O',]),
  'UnidadesMedicas2' =nrow(UM),
  'tasaerror' = sprintf("%i%%",floor(100*sum(RNS[,"completed"]=="False")/nrow(RNS))),
  'Recibidos' =nrow(MSJ[MSJ$direction=='I',]),
  'PoblacionAbierta' = nrow(CTC)-sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera)),
  'ConbebeTotal' = sprintf("%i%%",floor(100*get_conbebe(CTC,prospera=FALSE)/nrow(CTC))),
  'ConbebeProspera' = sprintf("%i%%",floor(100*get_conbebe(CTC,prospera=TRUE)/sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera)))),
  'ConbebeAbierto' = sprintf("%i%%",floor(100*(get_conbebe(CTC,prospera=FALSE)-get_conbebe(CTC,prospera=TRUE))/(nrow(CTC)-sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera))))),
  'Auxiliares' = get_aux(CTC), 
  'Vocales' = get_vocal(CTC),
  'PorcentajeClinicas' = round(length(get_unique_clues(CTC)[,1]) / nrow(UM) * 100,1)
)

save(pre_pars,file = "preLoadedObjects/pre_pars.RData")
remove(list=ls())
