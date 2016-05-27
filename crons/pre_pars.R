#!/usr/bin/env Rscript
setwd('/srv/shiny-server/ProsperaDigital/')
###Preload data###
source("AuxScripts/loadLibraries.R")
source("AuxScripts/aux.R")
require(lubridate)

unzip("data/data.zip",exdir = ".")
UM <- read.csv("data/ec_muestra_20160203.csv",stringsAsFactors = FALSE, fileEncoding = "Windows-1252") %>% subset(cl_treatmentArm!=0) 
MSJ <- read.csv("data/messages.csv",stringsAsFactors=F,header=T)
CTC <- read.csv("data/contacts.csv",stringsAsFactors=F,header=T)
RNS <- read.csv("data/runs.csv",,stringsAsFactors=F, header=T)
CLUES <- read.csv("data/CAT_CLUES_Febrero2016.csv",stringsAsFactors=F,header=T)
CLUES <- CLUES[,c('CLUES','CLAVE.ENTIDAD','CLAVE.MUNICIPIO')]
CLUES$mun <- paste(CLUES$CLAVE.ENTIDAD, CLUES$CLAVE.MUNICIPIO, sep="_")
MSJ$DiffDate <- as.Date(MSJ$created_on) - as.Date(MSJ$created_on[1])
Canal <- unique(MSJ[,c(13,8)]) 
Canal <- Canal %>% group_by(relayer) %>% dplyr::summarise(Usuarios=n())
Canal <- filter(Canal,is.na(relayer)==FALSE)
RNS$Completo <- ifelse(RNS$completed==1,1,0)
Reminders <- RNS[grepl("reminders",x = RNS$flow_name)==TRUE,]
Concerns <- RNS[grepl("concerns",x = RNS$flow_name)==TRUE,]
Prevent <- RNS[grepl("prevent",x = RNS$flow_name)==TRUE | grepl("zika",x = RNS$flow_name)==TRUE,]
Plan <- RNS[grepl("labor",x = RNS$flow_name)==TRUE | grepl("planning",x = RNS$flow_name)==TRUE,]
Fechas <- read.csv("data/Edomex capacitación.csv",stringsAsFactors = FALSE) %>% 
  mutate(Fecha=dmy(FECHA.DE.CAPACITACIÓN)) %>% mutate(Fecha = Fecha - Sys.Date()) %>% 
  filter(Fecha>0)
MIALERTA <- RNS[RNS$flow_name=="miAlerta",]
MICITA <- RNS[RNS$flow_name=="miCita_bf1" | RNS$flow_name=="miCita_cl1" ,]

pre_pars <- c(
  'Beneficiarias' = sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera)),
  'UnidadesMedicas' = nrow(UM) - nrow(Fechas),
  'Personal' = length(get_unique_clues(CTC)[,1]),
  'Estados' = length(unique(UM$cl_ent_clave_clCat)),
  'Municipios' = length(unique(CLUES[CLUES$CLUES%in%get_unique_clues(CTC)[,1],'mun'])),
  'CasosMIALERTA' = length(unique(RNS[
    RNS[,"flow_name"]=="miAlerta" & RNS[,"completed"]==1,"contact"] )),
  'CambiosMICITA' = length(unique(RNS[
    grepl("miCita_bf",RNS[,"flow_name"]) & RNS[,"completed"]==1,"contact"] )),
  'Usuarios' = nrow(CTC),
  'Mensajes' = nrow(MSJ),
  'Enviados' = nrow(MSJ[MSJ$direction=='O',]),
  'UnidadesMedicas2' =nrow(UM),
  'tasaerror' = sprintf("%i%%",floor(100 - (100*sum(RNS[,"completed"]==0)/nrow(RNS)))),
  'Recibidos' =nrow(MSJ[MSJ$direction=='I',]),
  'PoblacionAbierta' = nrow(CTC)-sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera)),
  'ConbebeTotal' = sprintf("%i%%",floor(100*get_conbebe(CTC,prospera=FALSE)/nrow(CTC))),
  'ConbebeProspera' = sprintf("%i%%",floor(100*get_conbebe(CTC,prospera=TRUE)/sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera)))),
  'ConbebeAbierto' = sprintf("%i%%",floor(100*(get_conbebe(CTC,prospera=FALSE)-get_conbebe(CTC,prospera=TRUE))/(nrow(CTC)-sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera))))),
  'Auxiliares' = get_aux(CTC), 
  'Vocales' = get_vocal(CTC),
  'PorcentajeClinicas' = round((nrow(UM) - nrow(Fechas)) / nrow(UM) * 100,1),
  'Telcel' = nrow(MSJ[MSJ$relayer=="1733",]),
  'Movistar' = nrow(MSJ[MSJ$relayer=="1493",]),
  'Otros canales' = nrow(MSJ) - nrow(MSJ[MSJ$relayer=="1493",]) - nrow(MSJ[MSJ$relayer=="1733",]),
  'PromedioMensajes' = round(nrow(MSJ) / nrow(CTC),0),
  'PromedioMensajesDia' = round(nrow(MSJ) / as.numeric(Sys.Date() - as.Date("2015-12-07")),0),
  'MensajesUltimaSemana' = nrow(MSJ[MSJ$DiffDate>-8,]),
  'UsuariosTelcel' = as.numeric(Canal[Canal$relayer=="1733",2]),
  'UsuariosMovistar' = as.numeric(Canal[Canal$relayer=="1493",2]),
  'UsuariosOtroscanales' = sum(Canal$Usuarios) - as.numeric(Canal[Canal$relayer=="1733",2]) - as.numeric(Canal[Canal$relayer=="1493",2]),
  'PromedioMensajesTelcel' = round(nrow(MSJ[MSJ$relayer=="1733",]) / as.numeric(Canal[Canal$relayer=="1733",2]),0),
  'PromedioMensajesMovistar' = round(nrow(MSJ[MSJ$relayer=="1493",]) / as.numeric(Canal[Canal$relayer=="1493",2]),0),
  'PromedioMensajesOtrosCanales' = round((nrow(MSJ) - nrow(MSJ[MSJ$relayer=="1493",]) - nrow(MSJ[MSJ$relayer=="1733",]))/ (sum(Canal$Usuarios) - as.numeric(Canal[Canal$relayer=="1733",2]) - as.numeric(Canal[Canal$relayer=="1493",2])),0),
  'RecordatorioFlujos' = nrow(Reminders[Reminders$order==1,]),
  'RecordatorioMensajes' = nrow(Reminders),
  'RecordatoriosTasaRespuesta' = paste(round(mean(Reminders$Completo,na.rm = TRUE) *100,1),"%",sep=""),
  'SenalesAlertaFlujos' = nrow(Concerns[Concerns$order==1,]),
  'SenalesAlertaMensajes' = nrow(Concerns),
  'SenalesAlertaTasaRespuesta' = paste(round(mean(Concerns$Completo,na.rm = TRUE) *100,1),"%",sep=""),
  'PreventAlertaFlujos' = nrow(Prevent[Prevent$order==1,]),
  'PreventAlertaMensajes' = nrow(Prevent),
  'PreventAlertaTasaRespuesta' = paste(round(mean(Prevent$Completo,na.rm = TRUE) *100,1),"%",sep=""),
  'PlanAlertaFlujos' = nrow(Plan[Plan$order==1,]),
  'PlanAlertaMensajes' = nrow(Plan),
  'PlanAlertaTasaRespuesta' = paste(round(mean(Plan$Completo,na.rm = TRUE) *100,1),"%",sep=""),
  'MiAlertaDetonaciones' = nrow(MIALERTA[MIALERTA$order==1,]),
  'MiAlertaCompleto' = nrow(MIALERTA[MIALERTA$order==1 & MIALERTA$completed==1,]),
  'MiAlertaUsuarias' = length(unique(MIALERTA$urns_0)),
  'MiCitaDetonaciones' = nrow(MICITA[MICITA$order==1,]),
  'MiCitataCompleto' = nrow(MICITA[MICITA$order==1 & MICITA$completed==1,]),
  'MiCitaUsuarias' = length(unique(MICITA$urns_0))
 )

save(pre_pars,file = "preLoadedObjects/pre_pars.RData")
source("crons/zipruns.R")
remove(list=ls())
