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
      tags$br("Estados",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Usuarios
      ),
      tags$br("Usuarios",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Mensajes
      ),
      tags$br("Mensajes",tags$br(),tags$br()),
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
      tags$br("Municipios",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Beneficiarias
      ),
      tags$br("Beneficiarias",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Enviados
      ),
      tags$br("Enviados",tags$br(),tags$br()),
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
      tags$br("Unidades Médicas",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Personal
      ),
      tags$br("Personal",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Recibidos
      ),
      tags$br("Recibidos",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Tasadeerror
      ),
      tags$br("Tasa de respuesta")
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
      tags$br("Número de clínicas total",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Numerodeclinicasincorporadas
      ),
      tags$br("Número de clínicas incorporadas",tags$br(),tags$br()),
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
      tags$br("Usuarias totales",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        BeneficiariasProspera),
      tags$br("Beneficiarias Prospera",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Poblacionabierta),
      tags$br("Población abierta")
    ),
    tags$div(
      class = "column-center", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Conbebetotal),
      tags$br("Con bebé (total)",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        ConbebeProspera),
      tags$br("Con bebé (Prospera)",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        ConbebeAbierto),
      tags$br("Con bebé (Abierto)")
    ),
    tags$div(
      class = "column-right", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Auxiliares),
      tags$br("Auxiliares",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Vocales),
      tags$br("Vocales",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        Responsables),
      tags$br("Responsables")
    )
  )
  
  return(html_)
  
}

template_mensajes <- function(arg_list){
  
  Mensajes <- arg_list$Mensajes
  MensajesTelcel <- arg_list$MensajesTelcel
  PromedioMensajes <- arg_list$PromedioMensajes
  Enviados <- arg_list$Enviados
  MensajesMovistar <- arg_list$MensajesMovistar
  PromedioMensajesDia <- arg_list$PromedioMensajesDia
  Recibidos <- arg_list$Recibidos
  MensajesOtros <- arg_list$MensajesOtros
  MensajesUltimaSemana <- arg_list$MensajesUltimaSemana
  

  html_ <- tags$div(
    class = "container",
    style = "margin-top:-10px",
    tags$div(
      class = "column-left", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Mensajes),
      tags$br("Mensajes Totales",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        MensajesTelcel),
      tags$br("Mensajes Telcel",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PromedioMensajes),
      tags$br("Promedio Mensajes por Usuario")
    ),
    tags$div(
      class = "column-center", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Enviados),
      tags$br("Mensajes Enviados",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        MensajesMovistar),
      tags$br("Mensajes Movistar",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PromedioMensajesDia),
      tags$br("Promedio Mensajes por Dia")
    ),
    tags$div(
      class = "column-right", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Recibidos),
      tags$br("Mensajes recibidos",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        MensajesOtros),
      tags$br("Mensajes otros canales",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        MensajesUltimaSemana),
      tags$br("Mensajes en la ultima semana")
    )
  )
  
  return(html_)
  
}

template_canal <- function(arg_list){
  
  Telcel <- arg_list$Telcel
  UsuariosTelcel <- arg_list$UsuariosTelcel
  PromedioMensajesTelcel <- arg_list$PromedioMensajesTelcel
  Movistar <- arg_list$Movistar
  UsuariosMovistar <- arg_list$UsuariosMovistar
  PromedioMensajesMovistar <- arg_list$PromedioMensajesMovistar
  Otroscanales <- arg_list$Otroscanales
  UsuariosOtroscanales <- arg_list$UsuariosOtroscanales
  PromedioMensajesOtrosCanales <- arg_list$PromedioMensajesOtrosCanales

  html_ <- tags$div(
    class = "container",
    style = "margin-top:-10px",
    tags$div(
      class = "column-left", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Telcel),
      tags$br("Mensajes Telcel",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        UsuariosTelcel),
      tags$br("Usuarios Telcel",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PromedioMensajesTelcel),
      tags$br("Mensajes por usuario Telcel")
    ),
    tags$div(
      class = "column-center", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Movistar),
      tags$br("Mensajes Movistar",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        UsuariosMovistar),
      tags$br("Usuarios Movistar",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PromedioMensajesMovistar),
      tags$br("Mensajes por usuario Movistar")
    ),
    tags$div(
      class = "column-right", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        Otroscanales),
      tags$br("Mensajes otros canales",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        UsuariosOtroscanales),
      tags$br("Usuarios otros canales",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PromedioMensajesOtrosCanales),
      tags$br("Mensajes por usuario otros canales")
    )
  )
  
  return(html_)
  
}

template_campaign <- function(arg_list){
  
  Estados <- arg_list$Estados

  RecordatoriosFlujos = arg_list$ RecordatoriosFlujo
  RecordatoriosMensajes = arg_list$RecordatoriosMensajes
  RecordatoriosTasaRespuesta = arg_list$RecordatoriosTasaRespuesta
  SenalesAlertaFlujos = arg_list$SenalesAlertaFlujos
  SenalesAlertaMensajes = arg_list$SenalesAlertaMensajes
  SenalesAlertaTasaRespuesta = arg_list$SenalesAlertaTasaRespuesta
  PreventAlertaFlujos = arg_list$PreventAlertaFlujos
  PreventAlertaMensajes = arg_list$PreventAlertaMensajes
  PreventAlertaTasaRespuesta = arg_list$PreventAlertaTasaRespuesta
  PlanAlertaFlujos = arg_list$PlanAlertaFlujos
  PlanAlertaMensajes = arg_list$PlanAlertaMensajes
  PlanAlertaTasaRespuesta = arg_list$PlanAlertaTasaRespuesta
  
  
  html_ <- tags$div(
    class = "container",
    style = "margin-top:-40px",
    tags$div(
      class = "column-left", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        RecordatoriosFlujos
      ),
      tags$br("Flujos de Recordatorios",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        SenalesAlertaFlujos
      ),
      tags$br("Flujos de Señales de Alerta",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PreventAlertaFlujos
      ),
      tags$br("Flujos de Prevención",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PlanAlertaFlujos
      ),
      tags$br("Flujos de planeación")
    ),
    tags$div(
      class = "column-center", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        RecordatoriosMensajes
      ),
      tags$br("Mensajes de Recordatorios",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        SenalesAlertaMensajes
      ),
      tags$br("Mensajes de Señales de Alerta",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PreventAlertaMensajes
      ),
      tags$br("Mensajes de Prevención",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PlanAlertaMensajes
      ),
      tags$br("Mensajes de Planeación")
    ),
    tags$div(
      class = "column-right", 
      style = "text-align:left;",
      tags$span(
        style=span_style ,
        RecordatoriosTasaRespuesta
      ),
      tags$br("Tasa de respuesta de Recordatorios",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        SenalesAlertaTasaRespuesta
      ),
      tags$br("Tasa de respuesta a Alertas",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PreventAlertaTasaRespuesta
      ),
      tags$br("Tasa de respuesta de Prevención",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        PlanAlertaTasaRespuesta
      ),
      tags$br("Tasa de respuesta a Planeación")
    )
  )
  
  return(html_)
  
}

template_alerta <- function(arg_list){
  
  AlertaDetonaciones <- arg_list$AlertaDetonaciones
  AlertaCompleto <- arg_list$AlertaCompleto
  AlertaUsuarias <- arg_list$AlertaUsuarias
  
  html_ <- tags$div(
    class = "container",
    style = "margin-top:-10px",
    tags$div(
      class = "column-left", 
      style = "text-align:left;width: 50%;",
      tags$span(
        style=span_style ,
        AlertaDetonaciones
      ),
      tags$br("Detonaciones de MIALERTA",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        AlertaCompleto
      ),
      tags$br("Flujos MIALERTA Completados",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        AlertaUsuarias
      ),
      tags$br("Usuarias de MIALERTA")
    )
  )
  
  return(html_)
  
}

template_cita <- function(arg_list){
  
  CitaDetonaciones <- arg_list$CitaDetonaciones
  CitaCompleto <- arg_list$CitaCompleto
  CitaUsuarias <- arg_list$CitaUsuarias
  
  html_ <- tags$div(
    class = "container",
    style = "margin-top:-10px",
    tags$div(
      class = "column-left", 
      style = "text-align:left;width: 50%;",
      tags$span(
        style=span_style ,
        CitaDetonaciones
      ),
      tags$br("Detonaciones de MICITA",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        CitaCompleto
      ),
      tags$br("Flujos MICITA Completados",tags$br(),tags$br()),
      tags$span(
        style=span_style ,
        CitaUsuarias
      ),
      tags$br("Usuarias de MICITA")
    )
  )
  
  return(html_)
  
}