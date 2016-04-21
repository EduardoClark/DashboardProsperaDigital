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

###Clinic Level Map
states <- readOGR("data",layer = "prueba", verbose = FALSE)
states@data$Color <- ifelse(states@data$PD==1,"#00cc99","#0C5F4A")
UM <- read.csv("data/ec_muestra_20160203.csv",stringsAsFactors = FALSE, fileEncoding = "Windows-1252") %>% subset(cl_treatmentArm!=0) 
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
  
  output$caption <- renderUI({
    if(input$nivel=="Nacional"){HTML(paste(
  "<div class='container' style='margin-top:-40px'>",
      "<div class='column-left' style='text-align:left;'>",
        "<span style='font-size:50px; line-height: 50%;'>",
          nrow(table(UM$cl_ent_clave_clCat)),
        "</span><br>",
          "Estados<br><br><br><br>",
        "<span style='font-size:50px; line-height: 50%;'>",
          prettyNum(4323,big.mark=",",scientific=FALSE),
        "</span><br>",
          "Usuarios<br><br><br><br>",
         "<span style='font-size:50px; line-height: 50%;'>",
          prettyNum(175727,big.mark=",",scientific=FALSE),
         "</span><br>",
            "Mensajes<br><br><br><br>",
          "<span style='font-size:50px; line-height: 50%;'>",
           prettyNum(20,big.mark=",",scientific=FALSE),
          "</span><br>",
            "Casos MIALERTA",
        "</div>",
  "<div class='column-center' style='text-align:left;'>",
  "<span style='font-size:50px; line-height: 50%;'>",
  nrow(table(UM$cl_mun_clave_clCat)),
  "</span><br>",
  "Municipios<br><br><br><br>",
  "<span style='font-size:50px; line-height: 50%;'>",
  prettyNum(3987,big.mark=",",scientific=FALSE),
  "</span><br>",
  "Beneficiarias<br><br><br><br>",
  "<span style='font-size:50px; line-height: 50%;'>",
  prettyNum(92315,big.mark=",",scientific=FALSE),
  "</span><br>",
  "Enviados<br><br><br><br>",
  "<span style='font-size:50px; line-height: 50%;'>",
  prettyNum(77,big.mark=",",scientific=FALSE),
  "</span><br>",
  "Cambios MICITA",
  "</div>",
  "<div class='column-right' style='text-align:left;'>",
  "<span style='font-size:50px; line-height: 50%;'>",
  nrow(UM),
  "</span><br>",
  "Unidades Médicas<br><br><br><br>",
  "<span style='font-size:50px; line-height: 50%;'>",
  prettyNum(336,big.mark=",",scientific=FALSE),
  "</span><br>",
  "Personal<br><br><br><br>",
  "<span style='font-size:50px; line-height: 50%;'>",
  prettyNum(83412,big.mark=",",scientific=FALSE),
  "</span><br>",
  "Recibidos<br><br><br><br>",
  "<span style='font-size:50px; line-height: 50%;'>",
  "8%",
  "</span><br>",
  "Tasa de error",
  "</div>",
     
  "</div>"),sep="")} 
    else if(input$nivel=="Estatal") {HTML(paste(
      "<div class='container' style='margin-top:-40px'>",
      "<div class='column-left' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      nrow(table(UM[UM$cl_ent_clave_clCat==7,]$cl_ent_clave_clCat)),
      "</span><br>",
      "Estados<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(1297,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Usuarios<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(7422,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Mensajes<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(3,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Casos MIALERTA",
      "</div>",
      "<div class='column-center' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      nrow(table(UM[UM$cl_ent_clave_clCat==7,]$cl_mun_clave_clCat)),
      "</span><br>",
      "Municipios<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(1237,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Beneficiarias<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(4676,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Enviados<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(21,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Cambios MICITA",
      "</div>",
      "<div class='column-right' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      nrow(UM[UM$cl_ent_clave_clCat==7,]),
      "</span><br>",
      "Unidades Médicas<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(60,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Personal<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(2746,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Recibidos<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "11%",
      "</span><br>",
      "Tasa de error",
      "</div>",
      
      "</div>"),sep="")} 
    else if(input$nivel=="Unidad Médica"){"Unidad Médica"}
    else {"Nacional"}
   })
  
  output$clinicas <- renderUI({
    if(input$nivel=="Nacional"){HTML(paste(
      "<div class='container' style='margin-top:10px'>",
      "<div class='column-left' style='text-align:left;width: 50%;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      nrow(UM),
      "</span><br>",
      "Número de clínicas total<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum( round(nrow(UM)*.27,0),big.mark=",",scientific=FALSE),
      "</span><br>",
      "Número de clínicas incorporadas<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      paste(round((nrow(UM)*.27)/nrow(UM) * 100,1),"%",sep=""),
      "</span><br>",
      "Porcentaje de clínicas ya incorporado",
      "</div></div>",sep=""))} 
    else if(input$nivel=="Estatal") {HTML(paste(
      "<div class='container' style='margin-top:10px'>",
      "<div class='column-left' style='text-align:left;width: 50%;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      nrow(UM[UM$cl_ent_clave_clCat==7,]),
      "</span><br>",
      "Número de clínicas total<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum( round(nrow(UM[UM$cl_ent_clave_clCat==7,])*.27,0),big.mark=",",scientific=FALSE),
      "</span><br>",
      "Número de clínicas incorporadas<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      paste(round((nrow(UM[UM$cl_ent_clave_clCat==7,])*.27)/nrow(UM[UM$cl_ent_clave_clCat==7,]) * 100,1),"%",sep=""),
      "</span><br>",
      "Porcentaje de clínicas ya incorporado",
      "</div></div>",sep=""))} 
    else if(input$nivel=="Unidad Médica"){"Unidad Médica"}
    else {"Nacional"}
  })
  
  output$usuarios <- renderUI({
    if(input$nivel=="Nacional"){HTML(paste(
      "<div class='container' style='margin-top:10px'>",
      "<div class='column-left' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(1235,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Usuarias totales<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(1190,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Beneficiarias Prospera<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(45,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Población abierta",
      "</div>",
      "<div class='column-center' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "23%",
      "</span><br>",
      "Con bebé (total)<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "27%",
      "</span><br>",
      "Con bebé (Prospera)<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "21%",
      "</span><br>",
      "Con bebé (Abierto)<br><br><br><br>",
      "</div>",
      "<div class='column-right' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "45",
      "</span><br>",
      "Auxiliares<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "22",
      "</span><br>",
      "Vocales<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "125",
      "</span><br>",
      "Responsables<br><br><br><br>",
      "</div>",
      "</div>"),sep="")} 
    else if(input$nivel=="Estatal") {HTML(paste(
      "<div class='container' style='margin-top:10px'>",
      "<div class='column-left' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(423,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Usuarias totales<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(402,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Beneficiarias Prospera<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      prettyNum(21,big.mark=",",scientific=FALSE),
      "</span><br>",
      "Población abierta",
      "</div>",
      "<div class='column-center' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "31%",
      "</span><br>",
      "Con bebé (total)<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "28%",
      "</span><br>",
      "Con bebé (Prospera)<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "35%",
      "</span><br>",
      "Con bebé (Abierto)<br><br><br><br>",
      "</div>",
      "<div class='column-right' style='text-align:left;'>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "12",
      "</span><br>",
      "Auxiliares<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "7",
      "</span><br>",
      "Vocales<br><br><br><br>",
      "<span style='font-size:50px; line-height: 50%;'>",
      "37",
      "</span><br>",
      "Responsables<br><br><br><br>",
      "</div>",
      "</div>"),sep="")} 
    else if(input$nivel=="Unidad Médica"){"Unidad Médica"}
    else {"Nacional"}
  })
  
  # output$timeline <- renderDygraph({
  #   TimeSeries
  # })
  

})


  

