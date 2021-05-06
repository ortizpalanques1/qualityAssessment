library(leaflet)
library(mongolite)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(stringr)
#####APLICACIÓN PARA OBSERVAR VARIABLES DE MULTILIMPIEZAS#####
options(scipen = 999)
## CSS
appCSS <- ".mandatory_star { color:red; }"
##Variables
title <- tags$a(href='http://www.multilimpiezas.com/',
                tags$img(src = "multilogo.jpg", height = '50', width = '77'),'Multilimpiezas')
fecha <- Sys.time()
year <- format(as.Date(fecha, format="%Y-%m-%d"),"%Y")
month <- format(as.Date(fecha, format="%Y-%m-%d"),"%m")
day <- format(as.Date(fecha, format="%Y-%m-%d"),"%d")
trimestre <- ifelse(as.numeric(month) <= 3,1,ifelse(as.numeric(month) <=6,2,ifelse(as.numeric(month) <=9,3,4)))
# Campos obligatorios en la forma de horas trabajadas
fieldsMandatory <- c("provincia", "annually")
# Campos a archivar en la forma de horas trabajadas (todos)
fieldsAll <- c("provincia", "annually")
##Funciones
#Cargar mongodb
source(file="opcionesMongoDB.R", local=T)[1]
# What is in "opcionesMongoDB.R":
# list(   
#   options(mongodb = list(
#     "host" = "xxx.xxxxx.mongodb.net",
#     "username" = "xxxxxxxxxx",
#     "password" = "xxxxxxxxxxxx"
#   ))
# )
cargador <- function(x,y){
    databaseName <- x
    collectionName <- y
    db <<-  mongo(collection = collectionName,
                  url = sprintf(
                      "mongodb+srv://%s:%s@%s/%s?retryWrites=true&w=majority",
                      options()$mongodb$username,
                      options()$mongodb$password,
                      options()$mongodb$host,
                      databaseName))
}
#Etiqueta ls campos obligatorios en la forma de horas trabajadas
labelMandatory <- function(label){
  tagList(
    label,
    span("*", class="mandatory_star")
  )
}
#Value Box Special
valueBox2 <- function(value, title, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      h3(title),
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}
#Evaluación de objetivos en tab Comercial: en "tipo", usar "texto", "color" o "icon" según lo que pida el código
evalua <- function(tipo,evaluado, evaluador,nivelCritico){
  if (tipo == "texto"){
    ifelse(evaluado < 0 & abs(evaluado)<= (nivelCritico*evaluador),"Por conseguir",ifelse(evaluado < 0 & abs(evaluado)<= evaluador,"Falta poco", "Meta superada por"))
  } else if (tipo == "color"){
    ifelse(evaluado < 0 & abs(evaluado)<= (nivelCritico*evaluador),"red",ifelse(evaluado < 0 & abs(evaluado)<= evaluador,"yellow", "green"))
  } else {
    ifelse(evaluado < 0 & abs(evaluado) <= (nivelCritico*evaluador),"exclamation-triangle",ifelse(evaluado < 0 & abs(evaluado) <= evaluador,"thumbs-down", "certificate"))
  }
}
##Procesamiento de datos
#Creación del dataframe para el gráfico Satisfaccion Trabajadores
cargador("ISO","encuestaTrabajadoresReal")
satisfaccionTrabajadores <- db$find()
cargador("ISO","itemsEncuestaTrabajadores")
itemsEncuestaTrabajadores <- db$find()
satisfaccionTrabajadores[satisfaccionTrabajadores==99] <- NA
satisfaccionTrabajadores <- satisfaccionTrabajadores %>%
  filter(Year==max(Year) || Year == (max(Year)-1)) %>%
  pivot_longer(cols = starts_with("P_"),names_to="pregunta", values_to="valor") %>%
  mutate(valor = as.numeric(valor)) %>% 
  left_join(., itemsEncuestaTrabajadores[c("Dimension", "Key")], by=c("pregunta"="Key")) %>% 
  group_by(Year,Dimension)%>% 
  summarise(promedio=round(mean(valor, na.rm=TRUE),2)) 
#Obtener diferencia total de la encuesta
diferenciaSatisfaccionTrabajadores <- satisfaccionTrabajadores %>%
    group_by(Year) %>% 
    summarise(Promedio=round(mean(promedio,na.rm = TRUE),2)) 
diferenciaFinalT <- round(diferenciaSatisfaccionTrabajadores$Promedio[2]-diferenciaSatisfaccionTrabajadores$Promedio[1],2)
promedioFinalTrabajador <- round(diferenciaSatisfaccionTrabajadores$Promedio[2],2)
rm(diferenciaSatisfaccionTrabajadores)
#Creación del dataframe para el gráfico Satisfaccion Trabajadores
cargador("ISO","encuestaClientes")
satisfaccionClientes <- db$find()
cargador("ISO","itemsEncuestaClientes")
itemsEncuestaClientes <- db$find()
satisfaccionClientes <- satisfaccionClientes %>% 
    filter(Ciclo==max(Ciclo) || Ciclo == (max(Ciclo)-1)) %>% 
    pivot_longer(cols = starts_with("P_"),names_to="pregunta", values_to="valor") %>% 
    left_join(., itemsEncuestaClientes[c("Dimension", "Key")], by=c("pregunta"="Key")) %>% 
    group_by(Ciclo,Dimension) %>% 
    summarise(promedio=round(mean(valor, na.rm=TRUE),2)) 
#Obtener diferencia total de la encuesta
diferenciaSatisfaccionClientes <- satisfaccionClientes %>%
    group_by(Ciclo) %>% 
    summarise(Promedio=round(mean(promedio,na.rm = TRUE),2)) 
diferenciaFinalC <- round(diferenciaSatisfaccionClientes$Promedio[2]-diferenciaSatisfaccionClientes$Promedio[1],2)
promedioFinalCliente <- round(diferenciaSatisfaccionClientes$Promedio[2],2)
rm(diferenciaSatisfaccionClientes)
#Cargar Facturaciones anuales
cargador("ISO","contratosEspecificos")
evaluaContratos <- db$find()
contratosEspecificos <- evaluaContratos %>% 
    filter(Year>=(max(Year)-5)) %>% 
    group_by(Year) %>% 
    summarise(Total=sum(MontoConIVA,na.rm = TRUE))
yearAlpha=min(contratosEspecificos$Year)
yearOmega=max(contratosEspecificos$Year)
espacios=nrow(contratosEspecificos)
ncontratos0 <- evaluaContratos %>% 
    filter(Year==year)
ncontratos=nrow(ncontratos0)
# Evolución de Clientes y Contratos
nClientes <- evaluaContratos %>% 
    group_by(Cliente,Year) %>% 
    summarise(Total=n())
nContrato <- evaluaContratos %>% 
    group_by(Contrato,Year) %>% 
    summarise(Total=n())
#Cargar Objetivos
cargador("ISO","objetivos")
objetivos <- db$find()
factura <- objetivos[objetivos$PK=="factura",colnames(objetivos)=="Valor"]#Este es el objetivo
facturacion <- contratosEspecificos[contratosEspecificos$Year==year,colnames(contratosEspecificos)=="Total"]#Totalde facturacion año
faltaFactura <- facturacion-factura
facturacionPY <- contratosEspecificos[contratosEspecificos$Year==(as.numeric(year)-1),colnames(contratosEspecificos)=="Total"]#Totalde facturacion año
objetivoClientes <- objetivos[objetivos$PK=="cliente",colnames(objetivos)=="Valor"]#Este es el objetivo
nClientesYear <- nrow(dplyr::filter(nClientes,Year==year))
faltaClientes <- nClientesYear-objetivoClientes
nContratosYear <- nrow(dplyr::filter(nContrato,Year==year))
objetivoContratos <- objetivos[objetivos$PK=="contrato",colnames(objetivos)=="Valor"]#Este es el objetivo
faltaContratos <- nContratosYear-objetivoContratos
incidenteObjetivo <- objetivos$Valor[objetivos$PK=="incidencia"]
reclamoObjetivo <- objetivos$Valor[objetivos$PK=="reclamo"]
averiaObjetivo <- objetivos$Valor[objetivos$PK=="averia"]
#Horas trabajadas (Período de prueba)
cargador("ISO","tiempoTrabajadores")
pruebaTiempo <- db$find()
pruebaTiempo$Entra <- as.Date(pruebaTiempo$Entra, format = "%d-%m-%Y")
pruebaTiempo$Sale <- as.Date(pruebaTiempo$Sale, format = "%d-%m-%Y")
pruebaTiempo$Sale <- ifelse(is.na(pruebaTiempo$Sale),Sys.Date(),pruebaTiempo$Sale)
pruebaTiempo$Sale <- as.Date(pruebaTiempo$Sale, origin = "1970-01-01")
#Obtener la diferencia de días
pruebaTiempo$date_diff <- difftime(pruebaTiempo$Sale,pruebaTiempo$Entra, units = "weeks" )
inicio <- format(min(pruebaTiempo$Entra, na.rm=TRUE),"%Y")
final <- format(max(pruebaTiempo$Sale, na.rm=TRUE),"%Y")
epoca <- as.numeric(final)-as.numeric(inicio)+1
anualidad <- seq(from=as.numeric(inicio), to=as.numeric(final))
losUltimosDias <- NULL
for (i in 1:length(anualidad)){
  ultimoDia <- if(i < length(anualidad)){
    as.Date(paste0(anualidad[i],"-12-31"),"%Y-%m-%d")
  }
  else{
    as.Date( format(Sys.Date(),"%Y-%m-%d"))
  }
  losUltimosDias <- c(losUltimosDias,ultimoDia)
}
losUltimosDias <- as.Date(losUltimosDias, origin = "1970-01-01")
horasSemanaMatrix <- matrix(ncol=epoca,nrow = length(pruebaTiempo$Entra))
for(columna in 1:length(losUltimosDias)){
  for(fila in 1:nrow(pruebaTiempo)){
    horasSemanaMatrix[fila,columna] <- difftime(as.Date(ifelse(pruebaTiempo$Sale[fila] < losUltimosDias[columna],pruebaTiempo$Sale[fila],losUltimosDias[columna]),origin="1970-01-01"),pruebaTiempo$Entra[fila], units = "weeks")
    #horasSemanaMatrix[fila,columna] <- (as.Date(as.character(losUltimosDias[columna]), format="%Y-%m-%d")-as.Date(as.character(pruebaTiempo$Entra[fila]), format="%Y-%m-%d")+1)/7
    horasSemanaMatrix[fila,columna] <- ifelse(horasSemanaMatrix[fila,columna] < 0, 0,horasSemanaMatrix[fila,columna])
  }
}
horaDefinitiva <- NULL
for( columna in ncol(horasSemanaMatrix):2){
  samua <- horasSemanaMatrix[,columna]-horasSemanaMatrix[,(columna-1)]
  horaDefinitiva <- cbind(samua,horaDefinitiva)
}
horaDefinitiva <- data.frame(horasSemanaMatrix[,1],horaDefinitiva)
nombrarColumnas <- paste0("Ciclo_",anualidad)
colnames(horaDefinitiva) <- nombrarColumnas
pruebaTiempo <- data.frame(pruebaTiempo,horaDefinitiva)
#Obtener el número de horas trabajadas el año pasado. Se usa en la página de Ambiente
anoPasado <- as.numeric(year)-1
annusCicloMA <- paste0("Ciclo_",anoPasado)
procesopruebaTiempoMA <- pruebaTiempo %>%
  filter(Problematico==FALSE)%>%
  mutate(HORAS=HorasSemana*get(annusCicloMA))
horasTrabajadasAnoPasado <- sum(procesopruebaTiempoMA$HORAS, na.rm = TRUE)
rm(procesopruebaTiempoMA)
#Obtener el número de horas trabajadas el año en curso. Se usa en la página de Ambiente
annusCicloMA <- paste0("Ciclo_",as.numeric(year))
procesopruebaTiempoMA <- pruebaTiempo %>%
  filter(Problematico==FALSE)%>%
  mutate(HORAS=HorasSemana*get(annusCicloMA))
horasTrabajadasAnoActual <- sum(procesopruebaTiempoMA$HORAS, na.rm = TRUE)
rm(procesopruebaTiempoMA)
#Ambiente. Consumo del año en curso
cargador("ISO","pedidosConCodigo")
pedidosConCodigo <- db$find()
cargador("ISO","codigoDesechos")
codigoDesechos <- db$find()
consumosAnuales <- pedidosConCodigo %>% 
  select(Trazabilidad, FacturaFecha,  starts_with("SCA"), starts_with("SBA"), starts_with("SEP")) %>% 
  pivot_longer(cols = starts_with("S"), names_to="Producto", values_to="Cantidad") %>% 
  mutate(ProductoGeneral=ifelse(Producto=="SCAGAS"|Producto=="SCADIE","Combustible",
                                ifelse(Producto=="SBABG0"|Producto=="SBABP0","Bolsas","Envases Plásticos"))) %>% 
  mutate(FacturaFecha = as.Date(FacturaFecha, format = "%d-%m-%Y")) %>% 
  mutate(Annus = as.numeric(format(FacturaFecha,"%Y"))) %>% 
  mutate(FacturaMes = as.numeric(format(FacturaFecha,"%m"))) %>% 
  mutate(Trimestre = ifelse(FacturaMes < 4, 1,
                            ifelse(FacturaMes < 7, 2,
                                   ifelse(FacturaMes < 10, 3,4)))) %>%
  group_by(ProductoGeneral,Producto,Annus,Trimestre) %>% 
  summarise(Trimestral = sum(Cantidad, na.rm = TRUE)) %>% 
  filter(!is.na(Annus))
ValoresConsumo <- left_join(consumosAnuales,codigoDesechos[c("Codex","DescriptioSpeciei","MensuraePonderum","PondusKg")], by=c("Producto"="Codex")) %>% 
  mutate(PondusKg=ifelse(is.na(PondusKg),1,PondusKg)) %>% 
  mutate(Consumo_Parcial=Trimestral*PondusKg) %>% 
  ungroup() %>% 
  filter(Annus==(as.numeric(year))|Annus==(as.numeric(year)-1)) %>% 
  group_by(ProductoGeneral,DescriptioSpeciei, MensuraePonderum, Annus) %>% 
  summarise(TOTAL = sum(Consumo_Parcial, na.rm=TRUE)) %>% 
  mutate(Etiqueta = paste(round(TOTAL,2),MensuraePonderum,sep = " ")) %>% 
  mutate(HorasTotales=ifelse(Annus==(as.numeric(year)),horasTrabajadasAnoActual,horasTrabajadasAnoPasado)) %>% 
  mutate(Proporcion=TOTAL/HorasTotales) %>% 
  mutate(Etiqueta02=paste0(round(Proporcion,3)," ",MensuraePonderum, ". por hora")) 
ValoresComparador <- ValoresConsumo %>% 
  dplyr::select(ProductoGeneral,DescriptioSpeciei,Annus,Proporcion) %>% 
  pivot_wider(names_from = Annus, values_from=Proporcion) %>% 
  mutate(Evaluatio=`2021`/`2020`) %>% 
  mutate(Aviso=ifelse(Evaluatio < 0.8, "BIEN",ifelse(Evaluatio <=1,"CUIDADO","ALERTA")))
ValoresAnoActual <- ValoresConsumo[ValoresConsumo$Annus==as.numeric(year),]
ValoresAnoPasado <- ValoresConsumo[ValoresConsumo$Annus==(as.numeric(year)-1),]
rm(ValoresConsumo)
# El "Objetivo" es lograr un consumo menor que el del año pasado
gasolina <- ValoresAnoActual$Etiqueta02[ValoresAnoActual$DescriptioSpeciei=="Gasolina" ]
diesel<- ValoresAnoActual$Etiqueta02[ValoresAnoActual$DescriptioSpeciei=="Diesel"]
bolsaGrande<- ValoresAnoActual$Etiqueta02[ValoresAnoActual$DescriptioSpeciei=="Bolsa grande"]
bolsaPequena<- ValoresAnoActual$Etiqueta02[ValoresAnoActual$DescriptioSpeciei=="Bolsa pequeña"]
envasePlastico1<- ValoresAnoActual$Etiqueta02[ValoresAnoActual$DescriptioSpeciei=="Envase plástico 1"]
envasePlastico5<- ValoresAnoActual$Etiqueta02[ValoresAnoActual$DescriptioSpeciei=="Envase plástico 5"]
anteGasolina <- ValoresAnoPasado$Etiqueta02[ValoresAnoPasado$DescriptioSpeciei=="Gasolina" ]
anteDiesel<- ValoresAnoPasado$Etiqueta02[ValoresAnoPasado$DescriptioSpeciei=="Diesel"]
anteBolsaGrande<- ValoresAnoPasado$Etiqueta02[ValoresAnoPasado$DescriptioSpeciei=="Bolsa grande"]
anteBolsaPequena<- ValoresAnoPasado$Etiqueta02[ValoresAnoPasado$DescriptioSpeciei=="Bolsa pequeña"]
anteEnvasePlastico1<- ValoresAnoPasado$Etiqueta02[ValoresAnoPasado$DescriptioSpeciei=="Envase plástico 1"]
anteEnvasePlastico5<- ValoresAnoPasado$Etiqueta02[ValoresAnoPasado$DescriptioSpeciei=="Envase plástico 5"]
# Tab Calidad
cargador("ISO","incRecAve")
incRecAve <- db$find()
incRecAve$Fecha <- as.Date(incRecAve$Fecha,format = "%d-%m-%Y")
incRecAve$Annus <- as.numeric(format(incRecAve$Fecha,"%Y")) 
incRecAveActual <- incRecAve %>% 
  dplyr::filter(Annus==as.numeric(year)) %>% 
  group_by(PK_Objetivo) %>% 
  summarise(Total=n()) %>% 
  left_join(.,objetivos[c("PK","Valor")], by=c("PK_Objetivo"="PK")) %>% 
  mutate(Ratio=Total/Valor) %>% 
  mutate(Aviso=ifelse(Ratio < 0.8, "BIEN",ifelse(Ratio <=1,"CUIDADO","ALERTA")))
###UI
ui <- dashboardPage(title = "Multilimpiezas",
    dashboardHeader(title=title,titleWidth = 300),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dirección", tabName = "direccion", icon = icon("clipboard-list")),
            menuItem("Satisfacción", tabName = "satisfaccion", icon = icon("id-badge")),
            menuItem("Comercial", tabName = "comercial", icon = icon("chart-line")),
            menuItem("Limpieza", tabName = "limpieza", icon = icon("spray-can")),
            menuItem("Medio Ambiente", tabName = "medioAmbiente", icon = icon("otter")),
            menuItem("Calidad", tabName = "calidad", icon = icon("certificate"))
        )
    ),
    dashboardBody(
      tags$style(".small-box.bg-navy { background-color: #e76555 !important; color: #3a4664 !important; }"),
      tags$style(".box.bg-navy { background-color: #e76555 !important; color: #3a4664 !important; }"),
        tabItems(
            tabItem(tabName = "direccion",
                    fluidRow(
                        #Implantación
                        box(
                            title = "Implantación de Multilimpiezas (2020)", 
                            status = "primary", 
                            solidHeader = TRUE,
                            width = 6, 
                            height = 350,
                            leafletOutput("implantacion",height = 280)
                        ),
                        #Proveedores
                        box(
                            title = "Proveedores de Multilimpiezas",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            height = 350,
                            dataTableOutput("proveedores")
                        )
                    ),
                    fluidRow(
                        #Nº trabajadores
                        valueBoxOutput("facturacion"),
                        valueBoxOutput("ntrabajadores"),
                        valueBoxOutput("ncontratos")
                    )
            ),
            tabItem(tabName = "satisfaccion",
                    fluidRow(
                        column(width = 6,
                               #Satisfacción trabajadores
                               box(
                                   title = "Satisfacción de los trabajadores",
                                    status = "primary",
                                    solidHeader = FALSE,
                                    width = NULL,
                                    plotOutput("satisfaccionTrabajadores")
                                )
                        ),
                        column(width = 6,
                               #Satisfacción clientes
                               box(
                                   title = "Satisfacción de los clientes (SIMULACIÓN)",
                                   status = "primary",
                                   solidHeader = FALSE,
                                   width = NULL,
                                   plotOutput("satisfaccionClientes")
                               )
                        )
                    ),
                    fluidRow(
                        valueBoxOutput("diferenciaFinalTrabajador", width = 3),
                        valueBoxOutput("objetivoEncuestaTrabajador", width = 3),
                        valueBoxOutput("diferenciaFinalCliente", width = 3),
                        valueBoxOutput("objetivoEncuestaCliente", width = 3)
                    )
            ),
            tabItem(tabName = "comercial",
                    column(width = 6,
                           fluidRow(
                               box(
                                   title = "Evolución de la facturación",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   height = 320,
                                   plotOutput("importeFacturacion")
                               )
                           ),
                           fluidRow(
                               valueBoxOutput("objetivoFacturacion", width = 6),
                               valueBoxOutput("faltaFacturacion", width = 6)
                           ),
                           fluidRow(
                             infoBoxOutput("facturacionPreviousYear", width = 6),
                             infoBoxOutput("facturacionThisYear", width = 6)
                           )
                    ),
                    column(width = 6,
                           fluidRow(
                               #Importe facturación
                               box(
                                   title = "Evolución de los clientes y contratos",
                                   status = "primary",
                                   solidHeader = TRUE,
                                   width = 12,
                                   height = 320,
                                   plotOutput("evolucionClienteContrato")
                               ),
                               fluidRow(
                                   infoBoxOutput("objetivoContratos", width = 6),
                                   infoBoxOutput("faltaContratos", width = 6)
                               ),
                               fluidRow(
                                   infoBoxOutput("objetivoClientes", width = 6),
                                   infoBoxOutput("faltaClientes", width = 6)
                               )
                           )
                    )
            ),
            tabItem(tabName = "limpieza",
                    column(width = 6,
                           fluidRow(
                             div(id="form",
                                 column(width=6,
                                        shinyjs::inlineCSS(appCSS),
                                        selectInput(
                                          "provincia",
                                          labelMandatory("Categoría"),
                                          c("Provincia","Lugar"),
                                          selected = NULL,
                                          multiple = FALSE,
                                          selectize = TRUE,
                                          width = NULL,
                                          size = NULL
                                        )
                                 ),
                                 column(width = 6,
                                        numericInput(
                                          "annually",
                                          labelMandatory("Año"),
                                          year,
                                          min = as.numeric(inicio),
                                          max = as.numeric(final),
                                          step = 1,
                                          width = NULL
                                        ),
                                        shinyjs::useShinyjs(),
                                        actionButton("enviar", "Enviar", class = "btn-primary")
                                 )
                             ),style = "height:130px"
                           ),
                           fluidRow(
                             # Cuadro de las horas trabajadas
                             box(
                               title = "Total horas trabajadas (SIMULACIÓN)",
                               status = "primary",
                               solidHeader = TRUE,
                               width = 12,
                               height = 400,
                               plotOutput("tiempoTrabajadores")
                             )
                           )
                    ),
                    column(width = 6,
                           fluidRow(
                             #Nº Total de Horas trabajadas
                             valueBoxOutput("totalHorasTrabajadasYear", width = 6),
                             #Nº Total de trabajadores dados de alta
                             valueBoxOutput("altaTrabajadores", width = 6),
                             style = "height:130px"
                           ),
                           fluidRow(
                             # Cuadro de las horas trabajadas
                             box(
                               title = "Número de trabajadores (SIMULACIÓN)",
                               status = "primary",
                               solidHeader = TRUE,
                               width = 12,
                               height = 400,
                               plotOutput("numeroDeTrabajadores")
                             )
                           )
                    )
            ),
            tabItem(tabName = "medioAmbiente",
                    fluidRow(#Consumo gasolina
                      valueBoxOutput("cosumoGasolinaAnoActual", width = 4),
                      valueBoxOutput("gasolinaEvaluo",width = 2),
                      #Consumo diesel
                      valueBoxOutput("cosumoDieselAnoActual", width = 4),
                      valueBoxOutput("dieselEvaluo",width = 2)
                    ),
                    fluidRow(#Consumo bolsas de basura entre horas tabajadas
                      valueBoxOutput("cosumoBolsaGrandeAnoActual", width = 4),
                      valueBoxOutput("bolsaGrandeEvaluo",width = 2),
                      #Consumo bolsas pequeñas entre horas trabajadas
                      valueBoxOutput("cosumoBolsaPequenaAnoActual", width = 4),
                      valueBoxOutput("bolsaPequenaEvaluo",width = 2)
                    ),
                    fluidRow(#Consumo envases de aerosol
                      valueBoxOutput("cosumoEnvasePlastico1AnoActual", width = 4),
                      valueBoxOutput("envasePlastico1Evaluo",width = 2),
                      #Consumo envases de aerosol
                      valueBoxOutput("cosumoEnvasePlastico5AnoActual", width = 4),
                      valueBoxOutput("envasePlastico5Evaluo",width = 2)
                    )
            ),
            tabItem(tabName = "calidad",
                    fluidRow(
                      #Nº Incidencias (evolución o número. Por determinar)
                      valueBoxOutput("incidenciasAnoActual", width = 4),
                      valueBoxOutput("incidenciasEvaluo",width = 2)
                    ),
                    fluidRow(
                      #Nº de reclamaciones de clientes
                      valueBoxOutput("reclamacionesAnoActual", width = 4),
                      valueBoxOutput("reclamacionesEvaluo",width = 2)
                    ),
                    fluidRow(
                      #Nº de averías causadas por personal en instalaciones del cliente
                      valueBoxOutput("averiasAnoActual", width = 4),
                      valueBoxOutput("averiasEvaluo",width = 2)
                    )
            )
        )
      )
    )
# Define server logic required to draw a histogram
server <- function(input, output) {
    #Implantación
    output$implantacion <- renderLeaflet({
        cargador("ISO","implantacion")
        implantacionMapa <- db$find() %>% 
          dplyr::filter(Year==as.numeric((year)))
        leaflet(implantacionMapa) %>%
            setView(lng = -5, lat = 40, zoom = 05) %>% 
            addTiles() %>%
            addMarkers(lng = ~LON, lat = ~LAT,
                       popup = ~as.character(Contrato),
                       label = ~as.character(Lugar),
                       options = popupOptions(closeButton = FALSE))
    })
    #Proveedores
    output$proveedores <- renderDataTable({
        cargador("ISO","proveedores")
        proveedoresTabla <- db$find()
        proveedoresTabla <- proveedoresTabla[c("NOMBRE_PROVEEDOR","SERVICIO","NIVEL")]
        colnames(proveedoresTabla) <- c("Nombre", "Servicio", "Nivel")
        proveedoresTabla},
         options = list(orderClasses = TRUE,
                        lengthMenu = c(1,2,3),
                        pageLength = 3))
    #Facturacion
    output$facturacion <- renderValueBox({
        valueBox(
            subtitle = paste0("Facturación en ",year), 
            value = facturacion, 
            icon = icon("euro-sign"), 
            color = "red"
        )
    })
    #Nº Trabajadores
    output$ntrabajadores <- renderValueBox({
      annusCicloA <- paste0("Ciclo_",year)
      numeroAnualTrabajadoresA <- pruebaTiempo %>% 
        filter(Problematico==FALSE) %>% 
        select(DNI,annusCicloA) %>%
        filter(get(annusCicloA)>0) %>% 
        group_by(DNI) %>% 
        summarise(TOTAL = sum(get(annusCicloA),na.rm = TRUE)) 
      numeroAnualTrabajadoresVectorA <- nrow(numeroAnualTrabajadoresA)
      valueBox(
          subtitle = paste0("Trabajadores en ",year),
          value = numeroAnualTrabajadoresVectorA, 
          icon = icon("id-badge"), 
          color = "red"
      )
    })
    #Nº Contratos
    output$ncontratos <- renderValueBox({
        valueBox(
            subtitle = paste0("Contratos cerrados en ",year), 
            value = ncontratos, 
            icon = icon("handshake"), 
            color = "red"
        )
    })
    #Satisfacción Trabajadores
    output$satisfaccionTrabajadores <- renderPlot({
        graficoSatisfaccionTrabajadores <- ggplot(satisfaccionTrabajadores,aes(x=Dimension, y=promedio, label = promedio, fill=factor(Year)))+
            geom_bar(stat="identity", position=position_dodge())+
            geom_text(aes(y=0.5),vjust=0.5, color="white",
                      position = position_dodge(0.9), size=3.5)+
            scale_fill_manual(values=c('#3a4664','#e76555'))+
            labs(y="Promedio", fill="Año")+
            coord_flip()+
            theme(
                axis.title = element_text(colour = "#3a4664"),
                axis.text = element_text(colour = "#3a4664", size=10),
                axis.title.y = element_blank(),
                legend.position = "bottom",
                legend.background = element_rect(fill = "#ffdbdc"),
                plot.background = element_rect(colour = "#ffdbdc",fill = "#ffdbdc"),
                panel.background = element_rect(colour = "#ffdbdc",fill = "#ffdbdc"),
                panel.grid = element_line(colour = "#ffdbdc")
            )
        graficoSatisfaccionTrabajadores
    })
    # Diferencia de la satisfacción de los trabajadores
    output$diferenciaFinalTrabajador <- renderValueBox({
        valueBox(
            value = diferenciaFinalT, 
            subtitle = paste0(ifelse(diferenciaFinalT <= 0,"Disminución", "Aumento"), " de la satisfacción del trabajador"), 
            icon = icon(ifelse(diferenciaFinalT <= 0,"arrow-down","arrow-up")),
            color = ifelse(diferenciaFinalT <= 0,"red","blue")
        )
    })
    # Obetivo encuesta trabajador
    output$objetivoEncuestaTrabajador <- renderValueBox({
        valueBox(
            value = promedioFinalTrabajador, 
            subtitle = paste0("Satisfacción trabajadores: ",ifelse(promedioFinalTrabajador <= 5,"Objetivo no cumplido y en riesgo", ifelse(promedioFinalTrabajador <= 6.8,"Objetivo no cumplido","Objetivo cumplido"))), 
            icon = icon(ifelse(promedioFinalTrabajador <= 5,"exclamation-triangle", ifelse(promedioFinalTrabajador <= 6.8,"thumbs-down","certificate"))),
            color = ifelse(promedioFinalTrabajador <= 5,"red", ifelse(promedioFinalTrabajador <= 6.8,"yellow","green"))
        )
    })
    #Satisfacción Clientes
    output$satisfaccionClientes <- renderPlot({
        graficoSatisfaccionClientes <- ggplot(satisfaccionClientes,aes(x=Dimension, y=promedio, label = promedio, fill=factor(Ciclo)))+
            geom_bar(stat="identity", position=position_dodge())+
            geom_text(aes(y=0.5),vjust=0.5, color="white",
                      position = position_dodge(0.9), size=3.5)+
            scale_fill_manual(values=c('#3a4664','#e76555'))+
            labs(y="Promedio", fill="Año")+
            coord_flip()+
            theme(
                axis.title = element_text(colour = "#3a4664"),
                axis.text = element_text(colour = "#3a4664", size=10),
                axis.title.y = element_blank(),
                legend.position = "bottom",
                legend.background = element_rect(fill = "#ffdbdc"),
                plot.background = element_rect(colour = "#ffdbdc",fill = "#ffdbdc"),
                panel.background = element_rect(colour = "#ffdbdc",fill = "#ffdbdc"),
                panel.grid = element_line(colour = "#ffdbdc")
            )
        graficoSatisfaccionClientes
    })
    # Diferencia de la satisfacción de los clientes
    output$diferenciaFinalCliente <- renderValueBox({
        valueBox(
            value = diferenciaFinalC, 
            subtitle = paste0(ifelse(diferenciaFinalC <= 0,"Disminución", "Aumento"), " de la satisfacción del cliente"), 
            icon = icon(ifelse(diferenciaFinalC <= 0,"arrow-down","arrow-up")),
            color = ifelse(diferenciaFinalC <= 0,"red","blue")
        )
    })
    # Obetivo encuesta cliente
    output$objetivoEncuestaCliente <- renderValueBox({
        valueBox(
            value = promedioFinalCliente, 
            subtitle = paste0("Satisfacción clientes: ",ifelse(promedioFinalCliente <= 3,"Objetivo no cumplido y en riesgo", ifelse(promedioFinalCliente <= 3.8,"Objetivo no cumplido","Objetivo cumplido"))), 
            icon = icon(ifelse(promedioFinalCliente <= 3,"exclamation-triangle", ifelse(promedioFinalCliente <= 3.8,"thumbs-down","certificate"))),
            color = ifelse(promedioFinalCliente <= 3,"red", ifelse(promedioFinalCliente <= 3.8,"yellow","green"))
        )
    })
    # Importe facturación
    output$importeFacturacion <- renderPlot({
        graphicoContratosEspecificos <- ggplot(contratosEspecificos, aes(x=Year, y=Total/1000))+
            geom_line(aes(x=Year, y=Total/1000), color = "#e76555", size = 1) +
            geom_hline(aes(yintercept=(factura/1000)),color="#3a4664", size=1, linetype="dashed") +
            geom_area(aes(x=Year, y=Total/1000), fill="#ffdbdc", alpha=0.5)+
            #annotate(geom="text", label="Objetivo", x=2020, y=500, vjust=-1, hjust=-1)+
            geom_label(aes(x=2020,y=(factura/1000)), 
                       label=paste0("Objetivo = ", factura, "€"), 
                       size=6, 
                       color="#3a4664",
                       nudge_x = 0.5,
                       nudge_y = -20)+
            labs(
                y="Miles de Euros"
            )+
            scale_x_continuous("Año",limits=c(yearAlpha,yearOmega),breaks=waiver(),n.breaks = espacios)+
            theme(
                panel.background = element_blank(),
                panel.grid.major.y = element_line(color="#7f7f7f"),
                axis.title = element_text(color="#3a4664", size = 24),
                axis.text = element_text(color="#3a4664", size = 16),
                axis.ticks = element_blank()
            )
        graphicoContratosEspecificos
    },height = 250)
    #Evolución de Clientes y contratos
    output$evolucionClienteContrato <- renderPlot({
        graficoEvolucionClientesContratosXX <- ggplot()+
            geom_line(data=nClientes,aes(x=Year),stat="count", color="#e76555", size=1)+
            geom_area(data=nClientes,aes(x=Year,  color="Clientes"),stat="count", fill="#e76555",alpha=0.2)+
            geom_line(data=nContrato,aes(x=Year),stat="count", color="#3a4664", size=1)+
            geom_area(data=nContrato,aes(x=Year, color="Contratos"),stat="count", fill="#3a4664", alpha=0.2)+
            scale_x_continuous("Año",limits=c(yearAlpha,yearOmega),breaks=waiver(),n.breaks = espacios)+
            scale_color_manual("", limits=c("Clientes", "Contratos"), values = c("#e76555","#3a4664"))+
            guides(colour = guide_legend(override.aes = list(pch = c(16, 21), fill = c("#e76555","#3a4664"))))+
            theme(
                axis.title.y = element_blank(),
                panel.background = element_blank(),
                panel.grid.major.y = element_line(color="#7f7f7f"),
                panel.grid.minor.y = element_blank(),
                axis.title = element_text(color="#3a4664", size = 24),
                axis.text = element_text(color="#3a4664", size = 16),
                axis.ticks = element_blank(),
                legend.position = "bottom"
            )
        graficoEvolucionClientesContratosXX
    },height = 250)
    #Objetivo Facturación
    output$objetivoFacturacion <- renderValueBox({
        valueBox(
            subtitle = paste0("Meta para ",year), 
            value = tags$p(paste0(factura," €"),style="font-size: 100%;"), 
            icon = icon("euro-sign"), 
            #fill = TRUE,
            color = "blue"
        )
    })
    # Cuanto falta o sobra de facturación
    output$faltaFacturacion <- renderValueBox({
        valueBox(
            subtitle = evalua("texto",faltaFactura,factura,0.9),
            value = tags$p(paste0(abs(faltaFactura)," €"),style="font-size: 100%;"), 
            icon = icon("euro-sign"), 
            #fill = TRUE,
            color = evalua("color",faltaFactura,factura,0.9)
        )
    })
    # Facturación Previous Year
    output$facturacionPreviousYear <- renderInfoBox({
      infoBox(
        paste0("Facturado en ",(as.numeric(year)-1),":"), 
        value = tags$p(facturacionPY,style="font-size: 125%;"), 
        icon = icon("euro-sign"), 
        fill = TRUE,
        color = "black"
      )
    })
    # Facturacion This Year
    output$facturacionThisYear <- renderInfoBox({
      infoBox(
        paste0("Facturado en ",year,":"), 
        value = tags$p(facturacion,style="font-size: 125%;"), 
        icon = icon("euro-sign"), 
        fill = TRUE,
        color = "black"
      )
    })
    output$objetivoClientes <- renderInfoBox({
        infoBox(
            paste0("Meta para ",year,":"), 
            value = tags$p(paste0(objetivoClientes, " Clientes"),style="font-size: 125%;"),
            icon = icon("briefcase"), 
            fill = TRUE,
            color = "blue"
        )
    })
    # Cuanto falta o sobra de clientes
    output$faltaClientes <- renderInfoBox({
         infoBox(
             evalua("texto",faltaClientes,objetivoClientes,0.75),
             value = tags$p(abs(faltaClientes), style = "font-size: 200%;"),  
             icon = icon(ifelse(faltaClientes < 0 & abs(faltaClientes) <= (0.75*objetivoClientes),"exclamation-triangle",ifelse(faltaClientes < 0 & abs(faltaClientes) <= objetivoClientes,"thumbs-down", "certificate"))),
             fill = TRUE,
             color = evalua("color",faltaClientes,objetivoClientes,0.75)
         )
    })
    # Objetivo número de contratos
    output$objetivoContratos <- renderInfoBox({
        infoBox(
            paste0("Meta para ",year,":"), 
            value = tags$p(paste0(objetivoContratos, " Contratos"),style="font-size: 125%;"), 
            icon = icon("handshake"), 
            fill = TRUE,
            color = "blue"
        )
    })
    # Cuanto falta o sobra de contratos
    output$faltaContratos <- renderInfoBox({
        infoBox(
            evalua("texto",faltaContratos,objetivoContratos,0.75),
            value = tags$p(abs(faltaContratos), style = "font-size: 200%;"), 
            icon = icon(evalua("icon",faltaContratos,objetivoContratos,0.75)),
            fill = TRUE,
            color = evalua("color",faltaContratos,objetivoContratos,0.75)
        )
    })
    # Forma de las horas trabajadas. id="form".
    observe({
      # check if all mandatory fields have a value
      mandatoryFilled <- 
        vapply(fieldsMandatory,
               function(x){
                 !is.null(input[[x]]) && input[[x]] !=""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      # enable/disable the submit button
      shinyjs::toggleState(id="enviar", condition=mandatoryFilled)
    })
    # Grafico Tiempo
    # Función para unir todos los datos de la forma
    formData <- reactive({
      dataForma <- sapply(fieldsAll, function(x) input[[x]])
      dataForma <- c(dataForma)
      dataForma
    })
    observeEvent(input$enviar,{
      agrupador=(formData()[1])
      elAnnus=(formData()[2])
      annusCiclo <- paste0("Ciclo_",elAnnus)
      procesopruebaTiempo <- pruebaTiempo %>% 
        filter(Problematico==FALSE) %>% 
        mutate(HORAS=HorasSemana*get(annusCiclo)) %>% 
        group_by_at(agrupador) %>%
        summarise(TOTAL=sum(HORAS,na.rm = TRUE)) %>% 
        rename(estaEs=1) %>% 
        filter(TOTAL > 0)
      totalHorasTrabajadasYearVector <- sum(procesopruebaTiempo$TOTAL, na.rm = TRUE)
      numeroAnualTrabajadores <- pruebaTiempo %>% 
        filter(Problematico==FALSE) %>% 
        select(DNI,annusCiclo,as.name(agrupador)) %>%
        rename(grupo=3) %>% 
        filter(get(annusCiclo)>0) %>% 
        group_by(grupo,DNI) %>% 
        summarise(TOTAL = sum(get(annusCiclo),na.rm = TRUE)) 
      numeroAnualTrabajadoresVector <- nrow(numeroAnualTrabajadores)
      graficopruebaTiempo <- ggplot(procesopruebaTiempo,aes(x=str_wrap(estaEs,12),y=TOTAL, label=round(TOTAL,0)))+
        geom_col(fill="#e76555",width = 0.675)+
        geom_text(aes(y = max(TOTAL)/8), color="#3a4664", size=6, fontface="bold")+
        labs(y=paste0("Horas trabajadas en ",elAnnus),
             caption = "Solo casos con información completa")+
        coord_flip()+
        theme(
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(color = "#3a4664", face="bold", size=10),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(color = "#3a4664", face="bold", size=16),
          panel.background = element_blank()
        )
      output$tiempoTrabajadores <- renderPlot({  
        graficopruebaTiempo
      }, height = 320)
      output$totalHorasTrabajadasYear <- renderValueBox({
        valueBox(
          subtitle = paste0("Total Horas ",elAnnus), 
          value = round(totalHorasTrabajadasYearVector,2), 
          icon = icon("clock"), 
          color = "navy"
        )
      })
      output$altaTrabajadores <- renderValueBox({
        valueBox(
          subtitle = paste0("Trabajadores en ",elAnnus), 
          value = numeroAnualTrabajadoresVector, 
          icon = icon("id-card-alt"), 
          color = "navy"
        )
      })
      output$numeroDeTrabajadores <- renderPlot({
        dataNumeroDeTrabajadores <-numeroAnualTrabajadores %>%
          group_by(grupo) %>% 
          tally() %>%
          rename(cantidad=n) 
        graficoNumeroDeTrabajadores <- ggplot(dataNumeroDeTrabajadores,aes(x=str_wrap(grupo,12), y=cantidad))+
          geom_col(fill="#e76555",width = 0.675)+
          geom_text(aes(y=0.5,label=cantidad),color="#3a4664", size=6, fontface="bold")+
          labs(y=paste0("Número de trabajadores ",elAnnus),
               caption = "Solo casos con información completa")+
          coord_flip()+
          theme(
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(color = "#3a4664", face="bold", size=10),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(color = "#3a4664", face="bold", size=16),
            panel.background = element_blank()
          )
        graficoNumeroDeTrabajadores
      }, height = 320)
    })
    #Tab de ambiente
    output$cosumoGasolinaAnoActual <- renderValueBox(valueBox2(
      value = gasolina,
      subtitle = paste0("Objetivo: Menos de ", anteGasolina),
      title = "Gasolina",
      icon = icon("gas-pump"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$gasolinaEvaluo <- renderValueBox(
      valueBox(
        subtitle =  "Gasolina",
        value = ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Gasolina"], 
        icon = icon(ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Gasolina"]=="BIEN","certificate",
                           ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Gasolina"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Gasolina"]=="BIEN","green",
                       ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Gasolina"]=="CUIDADO","yellow","red"))
      )
    )
    output$cosumoDieselAnoActual <- renderValueBox(valueBox2(
      value = diesel,
      subtitle = paste0("Objetivo: Menos de ", anteDiesel),
      title = "Diesel",
      icon = icon("gas-pump"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$dieselEvaluo <- renderValueBox(
      valueBox(
        subtitle =  "Diesel",
        value = ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Diesel"], 
        icon = icon(ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Diesel"]=="BIEN","certificate",
                           ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Diesel"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Diesel"]=="BIEN","green",
                       ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Diesel"]=="CUIDADO","yellow","red"))
      )
    )
    output$cosumoBolsaGrandeAnoActual <- renderValueBox(valueBox2(
      value = bolsaGrande,
      subtitle = paste0("Objetivo: Menos de ", anteBolsaGrande),
      title = "Bolsas Grandes",
      icon = icon("trash"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$bolsaGrandeEvaluo <- renderValueBox(
      valueBox(
        subtitle =  "Bolsa Grande",
        value = ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa grande"], 
        icon = icon(ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa grande"]=="BIEN","certificate",
                           ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa grande"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa grande"]=="BIEN","green",
                       ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa grande"]=="CUIDADO","yellow","red"))
      )
    )
    output$cosumoBolsaPequenaAnoActual <- renderValueBox(valueBox2(
      value = bolsaPequena,
      subtitle = paste0("Objetivo: Menos de ", anteBolsaPequena),
      title = "Bolsas Pequeñas",
      icon = icon("trash"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$bolsaPequenaEvaluo <- renderValueBox(
      valueBox(
        subtitle =  "Bolsa Pequeña",
        value = ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa pequeña"], 
        icon = icon(ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa pequeña"]=="BIEN","certificate",
                           ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa pequeña"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa pequeña"]=="BIEN","green",
                       ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Bolsa pequeña"]=="CUIDADO","yellow","red"))
      )
    )
    output$cosumoEnvasePlastico1AnoActual <- renderValueBox(valueBox2(
      value = envasePlastico1,
      subtitle = paste0("Objetivo: Menos de ", anteEnvasePlastico1),
      title = "Envases 1lt",
      icon = icon("prescription-bottle"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$envasePlastico1Evaluo <- renderValueBox(
      valueBox(
        subtitle =  "Envase 1lt.",
        value = ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 1"], 
        icon = icon(ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 1"]=="BIEN","certificate",
                           ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 1"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 1"]=="BIEN","green",
                       ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 1"]=="CUIDADO","yellow","red"))
      )
    )
    output$cosumoEnvasePlastico5AnoActual <- renderValueBox(valueBox2(
      value = envasePlastico5,
      subtitle = paste0("Objetivo: Menos de ", anteEnvasePlastico5),
      title = "Envases 5lt",
      icon = icon("prescription-bottle"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$envasePlastico5Evaluo <- renderValueBox(
      valueBox(
        subtitle =  "Envase 5lt.",
        value = ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 5"], 
        icon = icon(ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 5"]=="BIEN","certificate",
                           ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 5"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 5"]=="BIEN","green",
                       ifelse(ValoresComparador$Aviso[ValoresComparador$DescriptioSpeciei=="Envase plástico 5"]=="CUIDADO","yellow","red"))
      )
    )
    # Tab Calidad
    output$incidenciasAnoActual <- renderValueBox(valueBox2(
      value = incRecAveActual$Total[incRecAveActual$PK_Objetivo=="incidencia"],
      subtitle = paste0("Objetivo: Menos de ", incidenteObjetivo),
      title = "Incidencias(test)",
      icon = icon("user-injured"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$incidenciasEvaluo <- renderValueBox(
      valueBox(
        subtitle =  "Incidencias",
        value = incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="incidencia"], 
        icon = icon(ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="incidencia"]=="BIEN","certificate",
                           ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="incidencia"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="incidencia"]=="BIEN","green",
                       ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="incidencia"]=="CUIDADO","yellow","red"))
      )
    )
    output$reclamacionesAnoActual <- renderValueBox(valueBox2(
      value = incRecAveActual$Total[incRecAveActual$PK_Objetivo=="reclamo"],
      subtitle = paste0("Objetivo: Menos de ", reclamoObjetivo),
      title = "Reclamaciones(test)",
      icon = icon("angry"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$reclamacionesEvaluo <- renderValueBox(
      valueBox(
        subtitle =  "Reclamaciones",
        value = incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="reclamo"], 
        icon = icon(ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="reclamo"]=="BIEN","certificate",
                           ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="reclamo"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="reclamo"]=="BIEN","green",
                       ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="reclamo"]=="CUIDADO","yellow","red"))
      )
    )
    output$averiasAnoActual <- renderValueBox(valueBox2(
      value = incRecAveActual$Total[incRecAveActual$PK_Objetivo=="averia"],
      subtitle = paste0("Objetivo: Menos de ", averiaObjetivo),
      title = "Averías(test)",
      icon = icon("tools"),
      width = 4,
      color = "navy",
      href = NULL
    ))
    output$averiasEvaluo <- renderValueBox(
      valueBox(
        subtitle =  "Averías",
        value = incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="averia"], 
        icon = icon(ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="averia"]=="BIEN","certificate",
                           ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="averia"]=="CUIDADO","thumbs-down","exclamation-triangle"))), 
        color = ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="averia"]=="BIEN","green",
                       ifelse(incRecAveActual$Aviso[incRecAveActual$PK_Objetivo=="averia"]=="CUIDADO","yellow","red"))
      )
    )
}
# Run the application 
shinyApp(ui = ui, server = server)