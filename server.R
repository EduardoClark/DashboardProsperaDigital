#Load Libraries
source("AuxScripts/loadLibraries.R")
#Description
source("AuxScripts/template_html.R")
#load objects
load("preLoadedObjects/Mapa.RData")
load("preLoadedObjects/pre_pars.RData")


shinyServer(function(input, output) {

##Mapa
  output$mapaprueba <- renderLeaflet({
    if(input$nivel=="Unidad Médica") {TMP}
    else {TMP %>% setView(lng = -98,lat = 24.3,zoom=5)}
  })
  
##Resumen
  output$resumen <- renderUI({
      args <- list(  Estados = prettyNum(pre_pars['Estados']),
                     Usuarios = prettyNum(pre_pars['Usuarios'],big.mark=",",scientific=FALSE),
                     Mensajes = prettyNum(pre_pars['Mensajes'],big.mark=",",scientific=FALSE),
                     CasosMIALERTA = prettyNum(pre_pars['CasosMIALERTA'],big.mark=",",scientific=FALSE),
                     Municipios = pre_pars['Municipios'],#nrow(table(UM$cl_mun_clave_clCat)),
                     Beneficiarias = prettyNum(pre_pars['Beneficiarias'],big.mark=",",scientific=FALSE),
                     Enviados = prettyNum(pre_pars['Enviados'],big.mark=",",scientific=FALSE),
                     CambiosMICITA = prettyNum(pre_pars['CambiosMICITA'],big.mark=",",scientific=FALSE),
                     UnidadesMedicas = prettyNum(pre_pars['UnidadesMedicas']),
                     Personal = prettyNum(pre_pars['Personal'],big.mark=",",scientific=FALSE),
                     Recibidos = prettyNum(pre_pars['Recibidos'],big.mark=",",scientific=FALSE),
                     Tasadeerror = pre_pars['tasaerror']
      )
      template_resumen(args)
  })
  
  ##Clinicas
  output$clinicas <- renderUI({
      arg_list <- list( 
        Numerodeclinicastotal = prettyNum(pre_pars['UnidadesMedicas2']),
        Numerodeclinicasincorporadas = prettyNum(pre_pars['UnidadesMedicas'],big.mark=",",scientific=FALSE),
        Porcentajedeclinicasyaincorporado = paste(pre_pars['PorcentajeClinicas'],"%",sep="")
      )
      template_clinicas(arg_list)
  })
  
  ##Usuarios
  output$usuarios <- renderUI({
      args <- list(
        Usuariastotales = prettyNum(pre_pars['Usuarios'],big.mark=",",scientific=FALSE),
        BeneficiariasProspera = prettyNum(pre_pars['Beneficiarias'],big.mark=",",scientific=FALSE),
        Poblacionabierta = prettyNum(pre_pars['PoblacionAbierta'],big.mark=",",scientific=FALSE),
        Conbebetotal =pre_pars['ConbebeTotal'] ,
        ConbebeProspera = pre_pars['ConbebeProspera'],
        ConbebeAbierto = pre_pars['ConbebeAbierto'],
        Auxiliares = pre_pars['Auxiliares'],
        Vocales = pre_pars['Vocales'],
        Responsables = prettyNum(pre_pars['UnidadesMedicas'])
      )
      template_usuarios(args)
  })
  
##Mensajes
  output$mensajes <- renderUI({
    args <- list(
      Mensajes = prettyNum(pre_pars['Mensajes'],big.mark=",",scientific=FALSE),
      MensajesTelcel = prettyNum(pre_pars['Telcel'],big.mark=",",scientific=FALSE),
      PromedioMensajes = prettyNum(pre_pars['PromedioMensajes'],big.mark=",",scientific=FALSE),
      Enviados = prettyNum(pre_pars['Enviados'],big.mark=",",scientific=FALSE),
      MensajesMovistar = prettyNum(pre_pars['Movistar'],big.mark=",",scientific=FALSE),
      PromedioMensajesDia = prettyNum(pre_pars['PromedioMensajesDia'],big.mark=",",scientific=FALSE),
      Recibidos = prettyNum(pre_pars['Recibidos'],big.mark=",",scientific=FALSE),
      MensajesOtros = prettyNum(pre_pars['Otros canales'],big.mark=",",scientific=FALSE),
      MensajesUltimaSemana = prettyNum(pre_pars['MensajesUltimaSemana'],big.mark=",",scientific=FALSE)
    )
    template_mensajes(args)
  })
  
##Info por canal
  output$canal <- renderUI({
    args <- list(
      Telcel = prettyNum(pre_pars['Telcel'],big.mark=",",scientific=FALSE),
      UsuariosTelcel = prettyNum(pre_pars['UsuariosTelcel'],big.mark=",",scientific=FALSE),
      PromedioMensajesTelcel = prettyNum(pre_pars['PromedioMensajesTelcel'],big.mark=",",scientific=FALSE),
      Movistar = prettyNum(pre_pars['Movistar'],big.mark=",",scientific=FALSE),
      UsuariosMovistar = prettyNum(pre_pars['UsuariosMovistar'],big.mark=",",scientific=FALSE),
      PromedioMensajesMovistar = prettyNum(pre_pars['PromedioMensajesMovistar'],big.mark=",",scientific=FALSE),
      Otroscanales = prettyNum(pre_pars['Otros canales'],big.mark=",",scientific=FALSE),
      UsuariosOtroscanales = prettyNum(pre_pars['UsuariosOtroscanales'],big.mark=",",scientific=FALSE),
      PromedioMensajesOtrosCanales = prettyNum(pre_pars['PromedioMensajesOtrosCanales'],big.mark=",",scientific=FALSE)
    )
    template_canal(args)
  })

##Campañas
  output$campaign <- renderUI({
    args <- list(  RecordatoriosFlujos = prettyNum(pre_pars['RecordatorioFlujos'],big.mark=",",scientific=FALSE),
                   RecordatoriosMensajes = prettyNum(pre_pars['RecordatorioMensajes'],big.mark=",",scientific=FALSE),
                   RecordatoriosTasaRespuesta = pre_pars['RecordatoriosTasaRespuesta'],
                   SenalesAlertaFlujos = prettyNum(pre_pars['SenalesAlertaFlujos'],big.mark=",",scientific=FALSE),
                   SenalesAlertaMensajes = prettyNum(pre_pars['SenalesAlertaMensajes'],big.mark=",",scientific=FALSE),
                   SenalesAlertaTasaRespuesta = pre_pars['SenalesAlertaTasaRespuesta'],
                   PreventAlertaFlujos = prettyNum(pre_pars['PreventAlertaFlujos'],big.mark=",",scientific=FALSE),
                   PreventAlertaMensajes = prettyNum(pre_pars['PreventAlertaMensajes'],big.mark=",",scientific=FALSE),
                   PreventAlertaTasaRespuesta = pre_pars['PreventAlertaTasaRespuesta'],
                   PlanAlertaFlujos = prettyNum(pre_pars['PlanAlertaFlujos'],big.mark=",",scientific=FALSE),
                   PlanAlertaMensajes = prettyNum(pre_pars['PlanAlertaMensajes'],big.mark=",",scientific=FALSE),
                   PlanAlertaTasaRespuesta = pre_pars['PlanAlertaTasaRespuesta']
    )
    template_campaign(args)
  })  
  
  
##MiAlarma
  output$alarma <- renderUI({
    arg_list <- list( 
      AlertaDetonaciones = prettyNum(pre_pars['MiAlertaDetonaciones']),
      AlertaCompleto = prettyNum(pre_pars['MiAlertaCompleto'],big.mark=",",scientific=FALSE),
      AlertaUsuarias = prettyNum(pre_pars['MiAlertaUsuarias'],big.mark=",",scientific=FALSE)
    )
    template_alerta(arg_list)
  })
  
##MiCita
  output$cita <- renderUI({
    arg_list <- list( 
      CitaDetonaciones = prettyNum(pre_pars['MiCitaDetonaciones']),
      CitaCompleto = prettyNum(pre_pars['MiCitataCompleto'],big.mark=",",scientific=FALSE),
      CitaUsuarias = prettyNum(pre_pars['MiCitaUsuarias' ],big.mark=",",scientific=FALSE)
    )
    template_cita(arg_list)
  })

  
})

