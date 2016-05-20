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
                     UnidadesMedicas = prettyNum(pre_pars['UnidadesMedicas2']),
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

  
})




