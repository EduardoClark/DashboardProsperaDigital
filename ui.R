library(shiny)
library(leaflet)
library(rgdal)
library(dygraphs)
library(htmltools)
library(R.utils)


## ui.R ##
htmlTemplate("template.html",
  button = actionButton("action", "Action"),
  slider = sliderInput("x", "X", 1, 100, 21),
  navbar = navbarPage(id="nivel",tabPanel("Nacional"),
                              tabPanel("Nacional"),
                              tabPanel("Estatal"),
                              tabPanel("Unidad Médica")),
  navlist = navlistPanel(id="categorias",
   tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> Resumen<hr>"),htmlOutput("caption")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> Clinicas<hr>"), htmlOutput("clinicas")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> Usuarios<hr>"), htmlOutput("usuarios")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> Desertores<hr>")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> Total de Mensajes<hr>")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> Total de Mensajes por canal<hr>")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> Campañas<hr>")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> Tasa de error por pregunta<hr>")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> MIALARMA<hr>")),
    tabPanel(HTML("<span style='color:#00cc99;'>&#x25CF;</span> MICITA<hr>"))
  ),
  
  navbar2 = navbarPage(tabPanel(HTML("&nbsp;&nbsp;Nacion")),
                     tabPanel("Nacional"),
                     navbarMenu("Estado",
                                tabPanel("Sub-Component A"),
                                tabPanel("Sub-Component B")),
                     tabPanel("Unidad medica")
  ),
  
  mymap = leafletOutput("mapaprueba")
  
  # ,timeseries = dygraphOutput("timeline")
  

  
 
  
  
  
)

