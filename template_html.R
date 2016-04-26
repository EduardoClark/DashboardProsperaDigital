span_style <- "font-size:50px; line-height: 50%;"

template_resumen <- function(arg_list){
  
  Estados <- arg_list$Estados
  Usuarios <- arg_list$Usuarios
  Mensajes <- arg_list$Mensajes
  CasosMIALERTA <- arg_list$CasosMIALERTA
  Municipios <- arg_list$Municipios
  Beneficiarias <- arg_list$Beneficiarias
  Enviados <- arg_list$Enviados
  CambiosMICITA <- arg_list$CambiosMICITA
  UnidadesMedicas <- arg_list$UnidadesMedicas
  Personal <- arg_list$Personal
  Recibidos <- arg_list$Recibidos
  Tasadeerror <- arg_list$Tasadeerror
  
  html_ <- tags$div(
    class = "container",
    style = "margin-top:-40px",
    tags$div(
      class = "column-left", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Estados
      ),
      tags$br("Estados",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Usuarios
      ),
      tags$br("Usuarios",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Mensajes
      ),
      tags$br("Mensajes",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        CasosMIALERTA
      ),
      tags$br("Casos MIALERTA")
    ),
    tags$div(
      class = "column-center", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Municipios
      ),
      tags$br("Municipios",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Beneficiarias
      ),
      tags$br("Beneficiarias",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Enviados
      ),
      tags$br("Enviados",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        CambiosMICITA
      ),
      tags$br("Cambios MICITA")
    ),
    tags$div(
      class = "column-right", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        UnidadesMedicas
      ),
      tags$br("Unidades Médicas",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Personal
      ),
      tags$br("Personal",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Recibidos
      ),
      tags$br("Recibidos",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Tasadeerror
      ),
      tags$br("Tasa de error")
    )
  )
  
  return(html_)
  
}


template_clinicas <- function(arg_list){
  
  Numerodeclinicastotal <- arg_list$Numerodeclinicastotal
  Numerodeclinicasincorporadas <- arg_list$Numerodeclinicasincorporadas
  Porcentajedeclinicasyaincorporado <- arg_list$Porcentajedeclinicasyaincorporado
  
  html_ <- tags$div(
    class = "container",
    style = "margin-top:-10px",
    tags$div(
      class = "column-left", 
      style = "text-align:left;width: 50%;",
      tags$span(
        style=span_style ,
        Numerodeclinicastotal
      ),
      tags$br("Número de clínicas total",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Numerodeclinicasincorporadas
      ),
      tags$br("Número de clínicas incorporadas",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Porcentajedeclinicasyaincorporado
      ),
      tags$br("Porcentaje de clínicas ya incorporado")
    )
  )
  
  return(html_)
  
}

template_usuarios <- function(arg_list){
  
  Usuariastotales <- arg_list$Usuariastotales
  BeneficiariasProspera <- arg_list$BeneficiariasProspera
  Poblacionabierta <- arg_list$Poblacionabierta
  Conbebetotal <- arg_list$Conbebetotal
  ConbebeProspera <- arg_list$ConbebeProspera
  ConbebeAbierto <- arg_list$ConbebeAbierto
  Auxiliares <- arg_list$Auxiliares
  Vocales <- arg_list$Vocales
  Responsables <- arg_list$Responsables
  
  html_ <- tags$div(
    class = "container",
    style = "margin-top:-10px",
    tags$div(
      class = "column-left", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Usuariastotales),
      tags$br("Usuarias totales",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        BeneficiariasProspera),
      tags$br("Beneficiarias Prospera",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Poblacionabierta),
      tags$br("Población abierta",tags$br(),tags$br(),tags$br())
    ),
    tags$div(
      class = "column-center", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Conbebetotal),
      tags$br("Con bebé (total)",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        ConbebeProspera),
      tags$br("Con bebé (Prospera)",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        ConbebeAbierto),
      tags$br("Con bebé (Abierto)",tags$br(),tags$br(),tags$br())
    ),
    tags$div(
      class = "column-right", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Auxiliares),
      tags$br("Auxiliares",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Vocales),
      tags$br("Vocales",tags$br(),tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Responsables),
      tags$br("Responsables",tags$br(),tags$br(),tags$br())
    )
  )
  
  return(html_)
  
}




