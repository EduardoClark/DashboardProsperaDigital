###Preload data###
source("AuxScripts/loadLibraries.R")
source("AuxScripts/aux.R")

UM <- read.csv("./CopyOfdata/ec_muestra_20160203.csv",stringsAsFactors = FALSE, fileEncoding = "Windows-1252") %>% subset(cl_treatmentArm!=0) 
MSJ <- read.csv("./CopyOfdata/mensajes_pd_rp.csv",stringsAsFactors=F,header=T)
CTC <- read.csv("./CopyOfdata/contacts.csv",stringsAsFactors=F,header=T)
RNS <- read.csv("./CopyOfdata/runs.csv",,stringsAsFactors=F, header=T)
CLUES <- read.csv("./CopyOfdata/CAT_CLUES_Febrero2016.csv",stringsAsFactors=F,header=T)
CLUES <- CLUES[,c('CLUES','CLAVE.ENTIDAD','CLAVE.MUNICIPIO')]
CLUES$mun <- paste(CLUES$CLAVE.ENTIDAD, CLUES$CLAVE.MUNICIPIO, sep="_")
MSJ$DiffDate <- as.Date(MSJ$created_on) - as.Date(MSJ$created_on[1])
Canal <- unique(MSJ[,c(3,6)]) 
Canal <- Canal %>% group_by(relayer) %>% dplyr::summarise(Usuarios=n())
Canal <- filter(Canal,is.na(relayer)==FALSE)
RNS$Completo <- ifelse(RNS$completed=="True",1,0)
Reminders <- RNS[grepl("reminders",x = RNS$flow_name)==TRUE,]
Concerns <- RNS[grepl("concerns",x = RNS$flow_name)==TRUE,]
Prevent <- RNS[grepl("prevent",x = RNS$flow_name)==TRUE | grepl("zika",x = RNS$flow_name)==TRUE,]
Plan <- RNS[grepl("labor",x = RNS$flow_name)==TRUE | grepl("planning",x = RNS$flow_name)==TRUE,]

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
  'tasaerror' = sprintf("%i%%",floor(100 - (100*sum(RNS[,"completed"]=="False")/nrow(RNS)))),
  'Recibidos' =nrow(MSJ[MSJ$direction=='I',]),
  'PoblacionAbierta' = nrow(CTC)-sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera)),
  'ConbebeTotal' = sprintf("%i%%",floor(100*get_conbebe(CTC,prospera=FALSE)/nrow(CTC))),
  'ConbebeProspera' = sprintf("%i%%",floor(100*get_conbebe(CTC,prospera=TRUE)/sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera)))),
  'ConbebeAbierto' = sprintf("%i%%",floor(100*(get_conbebe(CTC,prospera=FALSE)-get_conbebe(CTC,prospera=TRUE))/(nrow(CTC)-sum(apply(CTC[,grepl('groups', colnames(CTC))] ,1 ,test_prospera))))),
  'Auxiliares' = get_aux(CTC), 
  'Vocales' = get_vocal(CTC),
  'PorcentajeClinicas' = round(length(get_unique_clues(CTC)[,1]) / nrow(UM) * 100,1),
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
  'PlanAlertaTasaRespuesta' = paste(round(mean(Plan$Completo,na.rm = TRUE) *100,1),"%",sep="")
 )

save(pre_pars,file = "preLoadedObjects/pre_pars.RData")
remove(list=ls())
