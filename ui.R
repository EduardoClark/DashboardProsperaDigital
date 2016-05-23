source("AuxScripts/loadLibraries.R")

## ui.R ##
span_hr_template <- function(legend){
  span_ <- "<span style='color:#00cc99;'>&#x25CF;</span> %s<hr>"
  html_code <- sprintf(span_,legend)
  return(html_code)
}

htmlTemplate("template.html",
  button = actionButton("action", "Action"),
  slider = sliderInput("x", "X", 1, 100, 20),
  navbar = navbarPage(id="nivel",tabPanel("Nacional"),
                              tabPanel("Nacional"),
                              tabPanel("Estatal"),
                              tabPanel("Unidad Médica")),
  navlist = navlistPanel(id="categorias",
   tabPanel(HTML(span_hr_template('Resumen')),htmlOutput("resumen")),
    tabPanel(HTML(span_hr_template('Clinicas')), htmlOutput("clinicas")),
    tabPanel(HTML(span_hr_template('Usuarios')), htmlOutput("usuarios")),
    tabPanel(HTML(span_hr_template('Total de Mensajes')), htmlOutput("mensajes")),
    tabPanel(HTML(span_hr_template('Operadores')), htmlOutput("canal")),
    tabPanel(HTML(span_hr_template('Campañas')), htmlOutput("campaign")),
    tabPanel(HTML(span_hr_template('MIALARMA')), htmlOutput("alarma")),
    tabPanel(HTML(span_hr_template('MICITA')), htmlOutput("cita"))
    # ,
    # tabPanel(HTML(span_hr_template('Tasa de error por pregunta'))),
    # tabPanel(HTML(span_hr_template('Desertores')))
  ),
  
  navbar2 = navbarPage(tabPanel(HTML("&nbsp;&nbsp;Nacion")),
                     tabPanel("Nacional"),
                     navbarMenu("Estado",
                                tabPanel("Sub-Component A"),
                                tabPanel("Sub-Component B")),
                     tabPanel("Unidad medica")
  ),
  
  mymap = leafletOutput("mapaprueba")
  
)

