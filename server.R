library(shiny)
library(leaflet)
require(dplyr)
require(maps)
library(rgdal)
require(htmltools)
require(Hmisc)
library(htmltools)
library(htmlwidgets)
library(metricsgraphics)
library(RColorBrewer)
library(xts)
library(dygraphs)

source("./template_html.R")

###Clinic Level Map
states <- readOGR(dsn="./data",layer = "prueba", verbose = T)
states@data$Color <- ifelse(states@data$PD==1,"#00cc99","#0C5F4A")
UM <- read.csv("./data/ec_muestra_20160203.csv",stringsAsFactors = FALSE, fileEncoding = "Windows-1252") %>% subset(cl_treatmentArm!=0) 
MSJ <- read.csv("./data/messages.csv")
CTC <- read.csv("./data/contacts.csv")
RNS <- read.csv("./data/runs.csv", header=T)
names(UM)[21:22] <- c("lon","lat")
HUIcon <- makeIcon(
  iconUrl = "http://104.236.151.123:3838/DashboardProspera/assets/icono-unidad.svg",
  iconWidth = 38, iconHeight = 95
)
GTO <- states
TMP <- leaflet(states,height = "500px", width = "100%") %>% addTiles() %>%
  addPolygons( stroke = TRUE, color="white", fillOpacity = 1, weight=1,fillColor = states$Color) %>%
  setView(lng = -99,lat = 18.3,zoom=7) %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon=HUIcon,
             popup = ~(paste("<span>Nombre</span><br>",
                             capitalize(tolower(cl_nombre_clCat)),
                             "<br><span>Municipio</span><br>",
                             capitalize(tolower(cl_mun_nombre_clCat)),
                             "<br><span>Estado</span><br>",
                             capitalize(tolower(cl_ent_nombre_clCat)),
                             "<br><span>Beneficiarias</span><br>",
                             NA,
                             "<br><span>Mensajes</span><br>",
                             NA,
                             sep=" " )),
             data=UM)

##Clinicas - TimeSeries
datetimes <- seq.POSIXt(as.POSIXct("2015-12-01", tz="CST"),
                        as.POSIXct("2016-02-05", tz="CST"), by="day")

TimeData <- data.frame(row=1, val=runif(67,min = 0,max = 30)) %>% mutate(Por=val/sum(val))
TimeData$Acc <- TimeData$Por
for(i in 2:nrow(TimeData)){TimeData[i,4]<- TimeData[i,4] + TimeData[i-1,4]}
TimeData$row <- NULL
TimeData$val <- NULL
TimeData$Por <- NULL
TimeData$Acc <- (TimeData$Acc * 100)*.28
TimeData$Progreso <- TimeData$Acc
TimeData$Acc <- NULL



########################################################################################################
shinyServer(function(input, output) {
  
  output$mapaprueba <- renderLeaflet({
    if(input$nivel=="Unidad Médica") {TMP}
    else {TMP %>% setView(lng = -98,lat = 24.3,zoom=5)}
  })
  
  output$resumen <- renderUI({
    if(input$nivel=="Nacional"){
      args <- list(  Estados = nrow(table(UM$cl_ent_clave_clCat)),
                     Usuarios = prettyNum(nrow(CTC),big.mark=",",scientific=FALSE),
                     Mensajes = prettyNum(nrow(MSJ),big.mark=",",scientific=FALSE),
                     CasosMIALERTA = prettyNum(length(unique(RNS[
                       RNS[,"flow_name"]=="miAlerta" 
                       & RNS[,"completed"]=="True",
                      "contact"] ) )
                      ,big.mark=",",scientific=FALSE),
                     Municipios = nrow(table(UM$cl_mun_clave_clCat)),
                     Beneficiarias = prettyNum(nrow(CTC),big.mark=",",scientific=FALSE),
                     Enviados = prettyNum(nrow(MSJ[MSJ$direction=='O',]),big.mark=",",scientific=FALSE),
                     CambiosMICITA = prettyNum(0,big.mark=",",scientific=FALSE),
                     UnidadesMedicas = nrow(UM),
                     Personal = prettyNum(336,big.mark=",",scientific=FALSE),
                     Recibidos = prettyNum(nrow(MSJ[MSJ$direction=='I',]),big.mark=",",scientific=FALSE),
                     Tasadeerror = sprintf("%i%%",floor(100*sum(RNS[,"completed"]=="False")/nrow(RNS)))
      )
      template_resumen(args)
      } 
    else if(input$nivel=="Estatal") {
      args <- list(  Estados = nrow(table(UM[UM$cl_ent_clave_clCat==7,]$cl_ent_clave_clCat)),
                     Usuarios = prettyNum(1297,big.mark=",",scientific=FALSE),
                     Mensajes = prettyNum(7422,big.mark=",",scientific=FALSE),
                     CasosMIALERTA = prettyNum(3,big.mark=",",scientific=FALSE),
                     Municipios = nrow(table(UM[UM$cl_ent_clave_clCat==7,]$cl_mun_clave_clCat)),
                     Beneficiarias = prettyNum(1237,big.mark=",",scientific=FALSE),
                     Enviados = prettyNum(4676,big.mark=",",scientific=FALSE),
                     CambiosMICITA = prettyNum(21,big.mark=",",scientific=FALSE),
                     UnidadesMedicas = nrow(UM[UM$cl_ent_clave_clCat==7,]),
                     Personal = prettyNum(60,big.mark=",",scientific=FALSE),
                     Recibidos = prettyNum(2746,big.mark=",",scientific=FALSE),
                     Tasadeerror = "11%"
      )
      template_resumen(args)
      } 
    else if(input$nivel=="Unidad Médica"){"Unidad Médica"}
    else {"Nacional"}
  })
  
  output$clinicas <- renderUI({
    if(input$nivel=="Nacional"){
      arg_list <- list( 
        Numerodeclinicastotal = nrow(UM),
        Numerodeclinicasincorporadas = prettyNum( round(nrow(UM)*.27,0),big.mark=",",scientific=FALSE),
        Porcentajedeclinicasyaincorporado = paste(round((nrow(UM)*.27)/nrow(UM) * 100,1),"%",sep="")
      )
      template_clinicas(arg_list)
      } 
    else if(input$nivel=="Estatal") {
      arg_list <- list( 
        Numerodeclinicastotal = nrow(UM[UM$cl_ent_clave_clCat==7,]),
        Numerodeclinicasincorporadas = prettyNum( round(nrow(UM[UM$cl_ent_clave_clCat==7,])*.27,0),big.mark=",",scientific=FALSE),
        Porcentajedeclinicasyaincorporado = paste(round((nrow(UM[UM$cl_ent_clave_clCat==7,])*.27)/nrow(UM[UM$cl_ent_clave_clCat==7,]) * 100,1),"%",sep="")
      )
      template_clinicas(arg_list)
      } 
    else if(input$nivel=="Unidad Médica"){"Unidad Médica"}
    else {"Nacional"}
  })
  
  output$usuarios <- renderUI({
    if(input$nivel=="Nacional"){
      args <- list(
        Usuariastotales = prettyNum(1235,big.mark=",",scientific=FALSE),
        BeneficiariasProspera = prettyNum(1190,big.mark=",",scientific=FALSE),
        Poblacionabierta = prettyNum(45,big.mark=",",scientific=FALSE),
        Conbebetotal = "23%",
        ConbebeProspera = "27%",
        ConbebeAbierto = "21%",
        Auxiliares = "45",
        Vocales = "22",
        Responsables = "125"
      )
      template_usuarios(args)
    } 
    else if(input$nivel=="Estatal") {
      args <- list(
        Usuariastotales = prettyNum(423,big.mark=",",scientific=FALSE),
        BeneficiariasProspera = prettyNum(402,big.mark=",",scientific=FALSE),
        Poblacionabierta = prettyNum(21,big.mark=",",scientific=FALSE),
        Conbebetotal = "31%",
        ConbebeProspera = "28%",
        ConbebeAbierto = "35%",
        Auxiliares = "12",
        Vocales = "7",
        Responsables = "37"
      )
      template_usuarios(args)
    } 
    else if(input$nivel=="Unidad Médica"){"Unidad Médica"}
    else {"Nacional"}
  })
  
  # output$timeline <- renderDygraph({
  #   TimeSeries
  # })
  
  
})




