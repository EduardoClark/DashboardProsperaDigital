
Telcel <- readOGR("data/CellCoverage/Shapes/TelcelGSM","TelcelGSM")
Telcel3G <- readOGR("data/CellCoverage/Shapes/Telcel3G/","Telcel3G")
Movistar <- readOGR("data/CellCoverage/Shapes/Movistar/","Movistar")
Iusa <- readOGR("data/CellCoverage/Shapes/IusacelGSM/","IusacelGSM")
Iusa3G <- readOGR("data/CellCoverage/Shapes/Iusacel3G","Iusacel3G")

require(foreign)
Censo <- read.dbf("ITER_NALDBF10.dbf",as.is = TRUE)
Censo <- Censo[,1:10]
Censo <- Censo[is.na(Censo$LATITUD)==FALSE,]
require(dplyr)
Censo$POBTOT <- as.numeric(Censo$POBTOT)
Censo <- Censo %>% mutate(PorPob =POBTOT / sum(POBTOT))
Censo <- Censo %>% mutate(Mas10 = ifelse(POBTOT>=10000,1,0))
Menos <- Censo %>% subset(Mas10==0)
Mas <- Censo %>% subset(Mas10==1)
Mas2018 <- Mas %>% arrange(desc(POBTOT))
Mas2018$pobAg <- Mas2018$PorPob
for(i in 2:(nrow(Mas2018))){
  Mas2018[i,13] <-   Mas2018[i-1,13] + Mas2018[i,13]
  print(i)
}
Mas2018A <- Mas2018[Mas2018$pobAg <= .25,]
Menos2018 <- Menos %>% arrange(desc(POBTOT))
Menos2018$pobAg <- Menos2018$PorPob
for(i in 2:(nrow(Menos2018))){
  Menos2018[i,13] <-   Menos2018[i-1,13] + Menos2018[i,13]
  print(i)
}
Menos2018A <- Menos2018[Menos2018$pobAg <= .05,]
RC2018 <- rbind(Menos2018A,Mas2018A)


names(RC2018)[8:9] <- c("lat","lon")

PM <- leaflet(Telcel) %>% addTiles() %>% 
  addPolygons(
    stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5, fillColor="blue"
  ) %>% addPolygons(
    stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5, fillColor="green", data=Movistar
  ) %>% addPolygons(
    stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5, fillColor="red", data=Iusa
  )

C4G <- leaflet(Telcel3G) %>% addTiles() %>% 
  addPolygons(
    stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5, fillColor="blue"
  ) %>% addPolygons(
    stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5, fillColor="green", data=Iusa3G
  ) %>% addCircleMarkers(data=RC2018)


addMarkers(
  clusterOptions = markerClusterOptions()
)

write.csv(RC2018,"RC2018.csv",row.names=FALSE)

