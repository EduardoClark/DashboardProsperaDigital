#!/usr/bin/env Rscript
setwd('/srv/shiny-server/ProsperaDigital')
#Zip datafiles
##Compress files for github  
Data <- list.files("data/",full.names = TRUE,pattern = ".csv")
zip(zipfile = "data/data.zip",Data) 
unlink(Data)