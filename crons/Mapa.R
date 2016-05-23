###Clinic Level Map###
source("AuxScripts/loadLibraries.R")

states <- readOGR(dsn="./data",layer = "prueba", verbose = T)
states@data$Color <- ifelse(states@data$PD==1,"#00cc99","#0C5F4A")
UM <- read.csv("./data/ec_muestra_20160203.csv",stringsAsFactors = FALSE, fileEncoding = "Windows-1252") %>% subset(cl_treatmentArm!=0) 
CLUES <- read.csv("./data/CAT_CLUES_Febrero2016.csv",stringsAsFactors=F,header=T)
CLUES <- CLUES[,c('CLUES','CLAVE.ENTIDAD','CLAVE.MUNICIPIO')]
CLUES$mun <- paste(CLUES$CLAVE.ENTIDAD, CLUES$CLAVE.MUNICIPIO, sep="_")
names(UM)[21:22] <- c("lon","lat")
HUIcon <- makeIcon(
  iconUrl = "http://104.236.151.123:3838/ProsperaDigital/assets/icono-unidad.svg",
  iconWidth = 38, iconHeight = 95
)
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
save(TMP,file = "preLoadedObjects/Mapa.RData")
remove(list=ls())
