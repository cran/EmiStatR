# creation of input server.R file for UI of the EmiStatR model, simulation
# author: Arturo Torres
# organization: LIST
# date: 13.09.2017 - 13.09.2017

# ns  : name of the structure

# 1.    catchment data
# id  : identification number [-]
# nm  : name of the municipality [-]
# nc  : name of the catchment [-]
# numc: number of the catchment [-]
# use : use of the soil [-]
# Atotal: total area [ha]
# Aimp: reduced area [ha]
# tfS : flow time structure [min]
# af  : reduction factor [-]
# pe  : population equivalent [PE]

# 2.    Structure data
# Qd  : throttled outflow [l/s]:
# V   : volume [m3]
#-----------------------------------------------------------------------------------------
library(shiny)
library(shinyFiles) # shinyDirChoose()
library(EmiStatR) # version 1.2.0.3
library(ggplot2)
library(xts)

# Define server logic 
shinyServer(function(input, output, session) {
  #-----------------------------------------------------------------------------------------
  volumes <<- getVolumes()
  dir.current <- getwd()
  setwd("..")
  dir.shiny <- getwd()
  dir.inputCSO <- paste0(dir.shiny, "/EmiStatR_inputCSO/inputCSO")
  dir.input <- paste0(dir.shiny, "/EmiStatR_input/input")
  
  #-----------------------------------------------------------------------------------------
  observe({
    # change directory for loading
    output$nd <- renderPrint(as.character(dir.inputCSO))
    
    ## load existing CSO or a user defined
    if(length(input$checkboxGroup.data) == 0){
      updateCheckboxInput(session=session, inputId="checkbox.CSO", label = "CSO input", value = FALSE)
      
      updateCheckboxInput(session=session, inputId="checkbox.ww", label = "Wastewater", value = FALSE)
      updateCheckboxInput(session=session, inputId="checkbox.inf", label = "Infiltration water", value = FALSE)
      updateCheckboxInput(session=session, inputId="checkbox.rw", label = "Rainwater", value = FALSE)
      
      updateSelectInput(session=session, inputId="selectbox.catchment", label = NULL, choices = "None",
                        selected = "None")
      
      rm(list=ls(pattern = "E"))
      rm(list=ls(pattern = "P"))
      rm(list=ls(pattern = "inf"))
      rm(list=ls(pattern = "rw"))
      rm(list=ls(pattern = "ww"))
      
      return(NULL)
    }else if(any(input$checkboxGroup.data == 'Goesdorf') & input$selectbox.catchment == 'Goesdorf'){
      setwd(dir.input)
      # wastewater data
      load("wastewater.RData")
      ww <<- ww
      updateCheckboxInput(session=session, inputId="checkbox.ww", label = "Wastewater", value = TRUE)
      
      # infiltration data
      load("infiltration.RData")
      inf <<- inf
      updateCheckboxInput(session=session, inputId="checkbox.inf", label = "Infiltration water", value = TRUE)
      
      # rainwater data
      load("rainwater.RData")
      rw <<- rw
      updateCheckboxInput(session=session, inputId="checkbox.rw", label = "Rainwater", value = TRUE)
      
      setwd(dir.inputCSO)
      load("E1_inputCSO.RData")
      # catchment data
      dataE.id      <<-  as.numeric(E1["id"])
      dataE.ns      <<-  as.character(E1["ns"])
      dataE.nm      <<-  as.character(E1["nm"])
      dataE.nc      <<- as.character(E1["nc"])
      dataE.numc    <<-  as.numeric(E1["numc"])
      dataE.use     <<- as.character(E1["use"])
      dataE.Atotal  <<-  as.numeric(E1["Atotal"])
      dataE.Aimp    <<- as.numeric(E1["Aimp"])
      dataE.tfS     <<- as.numeric(E1["tfS"])
      dataE.pe      <<- as.numeric(E1["pe"])
      
      # structure data
      dataE.V       <<-  as.numeric(E1["V"])
      dataE.lev2vol <<- E1[["lev2vol"]]
      dataE.lev.ini <<- as.numeric(E1["lev.ini"])
      dataE.Qd      <<-  as.numeric(E1["Qd"])
      dataE.Ad      <<-  as.numeric(E1["Ad"])
      dataE.Cd      <<-  as.numeric(E1["Cd"])
      updateCheckboxInput(session=session, inputId="checkbox.CSO", label = "CSO input", value = TRUE)
      
    }else if(any(input$checkboxGroup.data == 'Kaundorf') & input$selectbox.catchment == 'Kaundorf'){
      setwd(dir.input)
      # wastewater data
      load("wastewater.RData")
      ww <<- ww
      updateCheckboxInput(session=session, inputId="checkbox.ww", label = "Wastewater", value = TRUE)
      
      # infiltration data
      load("infiltration.RData")
      inf <<- inf
      updateCheckboxInput(session=session, inputId="checkbox.inf", label = "Infiltration water", value = TRUE)
      
      # rainwater data
      load("rainwater.RData")
      rw            <<- rw
      updateCheckboxInput(session=session, inputId="checkbox.rw", label = "Rainwater", value = TRUE)
      
      setwd(dir.inputCSO)
      load("E2_inputCSO.RData")
      # catchment data
      dataE.id      <<-  as.numeric(E2["id"])
      dataE.ns      <<-  as.character(E2["ns"])
      dataE.nm      <<-  as.character(E2["nm"])
      dataE.nc      <<- as.character(E2["nc"])
      dataE.numc    <<-  as.numeric(E2["numc"])
      dataE.use     <<- as.character(E2["use"])
      dataE.Atotal  <<-  as.numeric(E2["Atotal"])
      dataE.Aimp    <<- as.numeric(E2["Aimp"])
      dataE.tfS     <<- as.numeric(E2["tfS"])
      dataE.pe      <<- as.numeric(E2["pe"])
      
      # structure data
      dataE.V       <<-  as.numeric(E2["V"])
      dataE.lev2vol <<- E2[["lev2vol"]]
      dataE.lev.ini <<- as.numeric(E2["lev.ini"])
      dataE.Qd      <<-  as.numeric(E2["Qd"])
      dataE.Ad      <<-  as.numeric(E2["Ad"])
      dataE.Cd      <<-  as.numeric(E2["Cd"])
      
      updateCheckboxInput(session=session, inputId="checkbox.CSO", label = "CSO input", value = TRUE)
      
    }else if(any(input$checkboxGroup.data == "Nocher-Route") & input$selectbox.catchment == 'Nocher-Route'){
      setwd(dir.input)
      # wastewater data
      load("wastewater.RData")
      ww <<- ww
      updateCheckboxInput(session=session, inputId="checkbox.ww", label = "Wastewater", value = TRUE)
      
      # infiltration data
      load("infiltration.RData")
      inf <<- inf
      updateCheckboxInput(session=session, inputId="checkbox.inf", label = "Infiltration water", value = TRUE)
      
      # rainwater data
      load("rainwater.RData")
      rw            <<- rw
      updateCheckboxInput(session=session, inputId="checkbox.rw", label = "Rainwater", value = TRUE)
      
      # catchment data
      setwd(dir.inputCSO)
      load("E3_inputCSO.RData")
      
      dataE.id      <<-  as.numeric(E3["id"])
      dataE.ns      <<-  as.character(E3["ns"])
      dataE.nm      <<-  as.character(E3["nm"])
      dataE.nc      <<- as.character(E3["nc"])
      dataE.numc    <<-  as.numeric(E3["numc"])
      dataE.use     <<- as.character(E3["use"])
      dataE.Atotal  <<-  as.numeric(E3["Atotal"])
      dataE.Aimp    <<- as.numeric(E3["Aimp"])
      dataE.tfS     <<- as.numeric(E3["tfS"])
      dataE.pe      <<- as.numeric(E3["pe"])
      
      # structure data
      dataE.V       <<-  as.numeric(E3["V"])
      dataE.lev2vol <<- E3[["lev2vol"]]
      dataE.lev.ini <<- as.numeric(E3["lev.ini"])
      dataE.Qd      <<-  as.numeric(E3["Qd"])
      dataE.Ad      <<-  as.numeric(E3["Ad"])
      dataE.Cd      <<-  as.numeric(E3["Cd"])
      updateCheckboxInput(session=session, inputId="checkbox.CSO", label = "CSO input", value = TRUE)
      
    }else if(input$checkboxGroup.data == "user"){
      updateCheckboxInput(session=session, inputId="checkbox.CSO", label = "CSO input", value = TRUE)
      
      # render file input
      output$render.file.lev2vol <- renderUI({
        fileInput('file.lev2vol', '',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'))
      })
    }
    
    setwd(dir.current)
  })
  #-----------------------------------------------------------------------------------------
  table.ww <- reactive({
    if(length(input$checkboxGroup.data) == 0 | input$checkbox.ww == FALSE |
       input$selectbox.catchment == "None"){return(NULL)}
    {
      # compose data frame
      data.frame(
        Variable = c("Water consumption, qs [l/(PE d)]", 
                     "Pollution COD, CODs [g/(PE d)]",
                     "Pollution NH4, NH4s [g/(PE d)]"),
        Value = as.character(c(ww[["qs"]], 
                               ww[["CODs"]],
                               ww[["NH4s"]])), 
        stringsAsFactors=FALSE)  
    }
  })
  #-----------------------------------------------------------------------------------------
  table.inf <- reactive({
    if(length(input$checkboxGroup.data) == 0 | input$checkbox.inf == FALSE |
       input$selectbox.catchment == "None"){return(NULL)}
    {
      # compose data frame
      data.frame(
        Variable = c("Inflow, qf [l/(s ha)]:",
                     "Pollution COD [g/(PE d)]:",
                     "Pollution NH4 [g/(PE d)]:"),
        Value = as.character(c( 
          inf[["qf"]],
          inf[["CODf"]],
          inf[["NH4f"]])), 
        stringsAsFactors=FALSE) 
    }
  })
  #-----------------------------------------------------------------------------------------
  table.rw <- reactive({
    if(length(input$checkboxGroup.data) == 0 | input$checkbox.rw == FALSE |
       input$selectbox.catchment == "None"){return(NULL)}
    {
      # compose data frame
      data.frame(
        Variable = c("Pollution COD [mg/l]:",
                     "Pollution NH4 [mg/l]:",
                     "Rain measurement station:" 
        ),
        Value = as.character(c( 
          rw[["CODr"]],
          rw[["NH4r"]],
          rw[["stat"]]
        )), 
        stringsAsFactors=FALSE)
    }
  })
  #-----------------------------------------------------------------------------------------
  table.ns <- reactive({
    if(length(input$checkboxGroup.data) == 0 | input$checkbox.CSO == FALSE |
       input$selectbox.catchment == "None"){return(NULL)}
    data.frame(
      Variable = c("Id", "Structure"),
      Value = as.character(c(dataE.id, dataE.ns)), 
      stringsAsFactors=FALSE)   
  })
  #-----------------------------------------------------------------------------------------
  table.catchment <- reactive({
    if(length(input$checkboxGroup.data) == 0 | input$checkbox.CSO == FALSE |
       input$selectbox.catchment == "None"){return(NULL)}
    {
      # compose data frame
      data.frame(
        Variable = c("Municipality:",
                     "Catchment name:",
                     "Catchment number",
                     "Use",
                     "Total area, Atotal [ha]",
                     "Impervious area, Aimp [ha]",
                     "Flow time structure, tfS [min]",
                     "Population equivalent [PE]:"),
        Value = as.character(c(dataE.nm,
                               dataE.nc,
                               dataE.numc,
                               dataE.use,
                               dataE.Atotal,
                               dataE.Aimp,
                               dataE.tfS,
                               dataE.pe)), 
        stringsAsFactors=FALSE)
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({
    if(input$selectbox.rain == "None"){
      if(exists("P1") == TRUE){rm(P1)}
      return(NULL)
    }else if(input$selectbox.rain == "P1"){
      data("P1")
      P1 <<- P1
      
      # render plot rain
      output$render.checkbox.rain.plot <- renderUI({
        checkboxInput("checkbox.rain.plot", label = "Plot", value = FALSE)
      })
      
    }else if(input$selectbox.rain == "Esch_Sure2010"){
      data("Esch_Sure2010")
      P1 <<- Esch_Sure2010
      
      # render plot rain
      output$render.checkbox.rain.plot <- renderUI({
        checkboxInput("checkbox.rain.plot", label = "Plot", value = FALSE)
      })
    }
    setwd(dir.current)
  })
  #-----------------------------------------------------------------------------------------
  observe({ # plot rainfall time series
    if(length(length(input$selectbox.rain) == 0 | input$checkbox.rain.plot) == 0 
       ){
      output$plot.rain <- renderPlot({NULL})
      return(NULL)
    }else if(input$selectbox.rain == "None"){
      updateCheckboxInput(session = session, inputId = "checkbox.rain.plot", value = FALSE)
    }else if(input$checkbox.rain.plot == TRUE){
      output$plot.rain <- renderPlot(
        {
          x11()
          ggplot(data = P1) + aes(P1[1], P1[2]) + 
            geom_line(colour = "#000099") +  # blue line
            xlab("Time") + ylab("Rainfall [mm]") +
            ggtitle("Rainfall time series, P1") 
        }, 
        width = 300, height=50, res = 25)
    }
  })
  #-----------------------------------------------------------------------------------------
  table.structure <- reactive({
    if(length(input$checkboxGroup.data) == 0 | input$checkbox.CSO == FALSE |
       input$selectbox.catchment == "None"){return(NULL)}
    
    if(input$selectbox.catchment == "Goesdorf"){ 
      lev2vol.file <- "level-volume_E1.csv"
    }else if(input$selectbox.catchment == "Kaundorf"){
      lev2vol.file <- "level-volume_E2.csv"
    }else if(input$selectbox.catchment == "Nocher-Route"){
      lev2vol.file <- "level-volume_E3.csv"
    }else if(length(input$selectbox.catchment) == 0 & length(lev2vol.file)==0){
      lev2vol.file <- "(choose a file)"
    }else lev2vol.file <- as.character(input$file.lev2vol$name)
    
    # Compose data frame
    data.frame(
      Variable = c("Volume, V [m3]:",
                   "Relationship level [m] - volume [m3]",
                   "Initial water level, Lev_ini [m]:",
                   "Maximum throttled outflow, Qd_max [l/s]:",
                   "Orifice area, Ad [m2]:",
                   "Orifice discharge coefficient, Cd [-]:"),
      Value = as.character(c(dataE.V,
                             lev2vol.file,
                             dataE.lev.ini,
                             dataE.Qd,
                             dataE.Ad,
                             dataE.Cd)), 
      stringsAsFactors=FALSE)
  })
  #-----------------------------------------------------------------------------------------
  observe({
    ## update selectbox.catchment
    if(length(input$checkboxGroup.data) != 0){
      updateSelectInput(session=session, inputId="selectbox.catchment", label = NULL, 
                        choices = input$checkboxGroup.data, 
                        selected = input$checkboxGroup.data[length(input$checkboxGroup.data)])
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({
    ## update output tables
    if(input$selectbox.catchment != "None"){
      
      # Show the values using an HTML table
      output$render.checkboxGroup.data <-
        renderPrint(input$checkboxGroup.data)
      
      # Show the values using an HTML table
      output$render.table.ww <- renderTable({
        table.ww()
      })
      
      # Show the values using an HTML table
      output$render.table.inf <- renderTable({
        table.inf()
      })
      
      # Show the values using an HTML table
      output$render.table.rw <- renderTable({
        table.rw()
      })
      
      # Show the values using an HTML table
      output$render.table.ns <- renderTable({
        table.ns()
      })
      
      # Show the values using an HTML table
      output$render.table.catchment <- renderTable({
        table.catchment()
      })
      
      # Show the values using an HTML table
      output$render.table.structure <- renderTable({
        table.structure()
      })
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({ # render infile for qs.daily
    if(input$selectbox.qs.daily == "none"){
      output$render.infile.qs.daily <- renderUI({NULL})
      return(NULL)
    }else if(input$selectbox.qs.daily == "file"){
      output$render.infile.qs.daily <- renderUI({
        fileInput('infile.qs.daily', '',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'))
      })
      if(length(input$infile.qs.daily) == 0){return(NULL)}
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({ # factors for qs.daily
    if(input$selectbox.qs.daily == "none" & input$checkbox.qs.daily.plot == TRUE){
      deltat <- 120 # mins
      time   <- seq(as.POSIXct(as.Date(Sys.time()), format = "%H:%M:%S"), by = deltat*60, length.out = 12)
      daily.factors    <- as.xts(rep(1, 12), order.by = time)
      
      qs.daily <<- data.frame(cbind(index(daily.factors), coredata(daily.factors))) # actual factors for EmiStatR
      colnames(qs.daily) <- c("time", "factor")
      
      xlabels <- index(daily.factors)
      xlabels <- format(xlabels, "%H:%M")
      
      output$plot.qs <- renderPlot(
        {
          x11()
          qplot(qs.daily[,"time"], qs.daily[,"factor"]) + geom_point() + geom_line() +
            xlab("Time") + ylab("Factor [--]") +
            ggtitle("Daily factors for water consumption, qs") +
            scale_x_continuous(breaks = qs.daily[,"time"], labels=xlabels)
          
        }, 
        width = 300, height=250, res = 75)
    }else if(length(input$infile.qs.daily) == 0){
      updateCheckboxInput(session=session, inputId="checkbox.qs.daily.plot", value=FALSE )
      output$plot.qs.daily <- renderPlot({NULL})
      return(NULL)
    }else if(length(input$infile.qs.daily) > 0 & input$checkbox.qs.daily.plot == TRUE){
      qs.daily.file <- input$infile.qs.daily$datapath
      
      qs.daily <<- read.csv(qs.daily.file, header = TRUE) # actual factors for EmiStatR
      a        <- IsReg(data = qs.daily, format="%H:%M:%S", tz="UTC")
      daily.factors <- a[[2]]
      
      output$plot.qs <- renderPlot(
        {
          x11()
          autoplot(daily.factors) + geom_point() + geom_line() +
            xlab("Time") + ylab("Factor [--]") +
            ggtitle("Daily factors for water consumption, qs")
        }, 
        width = 300, height=250, res = 75)
    }else if(input$checkbox.qs.daily.plot == FALSE){
      output$plot.qs.daily <- renderPlot({NULL})
    }
      setwd(dir.current)
  })
  #-----------------------------------------------------------------------------------------
  observe({ # render infile for qs.weekly
    if(input$selectbox.qs.weekly == "none"){
      output$render.infile.qs.weekly <- renderUI({NULL})
      return(NULL)
    }else if(input$selectbox.qs.weekly == "file"){
      output$render.infile.qs.weekly <- renderUI({
        fileInput('infile.qs.weekly', '',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'))
      })
      if(length(input$infile.qs.weekly) == 0){return(NULL)}
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({ # factors for qs.weekly
    if(input$selectbox.qs.weekly == "none" & input$checkbox.qs.weekly.plot == TRUE){
      qs.weekly <<- list(mon=1, tue=1, wed=1, thu=1, fri=1, sat=1, sun=1 ) # actual factors for EmiStatR
      qs.weekly.plot <- cbind.data.frame(names(qs.weekly), as.numeric(c(1:7)), as.numeric(1))
      colnames(qs.weekly.plot) <- c("Day", "Day number", "Factor [--]")
    
      output$plot.qs <- renderPlot(
        {
          x11()
          qplot(qs.weekly.plot[,"Day number"], qs.weekly.plot[,"Factor [--]"]) + 
            geom_point() + geom_line() + 
            xlab("Day") + ylab("Factor [--]") +
            ggtitle("Weekly factors for water consumption, qs") +
            scale_x_continuous(breaks = qs.weekly.plot[,"Day number"], labels=qs.weekly.plot[,"Day"])
        },
        width = 600, height=500, res = 75)
    }else if(length(input$infile.qs.weekly) == 0){
      updateCheckboxInput(session=session, inputId="checkbox.qs.weekly.plot", value=FALSE )
      output$plot.qs.weekly <- renderPlot({NULL})
      return(NULL)
    }else if(length(input$infile.qs.weekly) > 0 & input$checkbox.qs.weekly.plot == TRUE){
      qs.weekly.file <- input$infile.qs.weekly$datapath
      qs.weekly <<- read.csv(qs.weekly.file, header = TRUE) # actual factors for EmiStatR
      
      output$plot.qs <- renderPlot(
        {
          x11()
          qplot(c(1:nrow(qs.weekly)), qs.weekly[,2]) + geom_point() + geom_line() +
            xlab("Time") + ylab("Factor [--]") +
            ggtitle("Weekly factors for water consumption, qs") +
            scale_x_continuous(breaks = c(1:nrow(qs.weekly)), labels=qs.weekly[,1])
        }, 
        width = 600, height=500, res = 75)
    }else if(input$checkbox.qs.weekly.plot == FALSE){
      output$plot.qs.weekly <- renderPlot({NULL})
    }
    setwd(dir.current)
  })
  #-----------------------------------------------------------------------------------------
  observe({ # render infile for qs.seasonal
    if(input$selectbox.qs.seasonal == "none"){
      output$render.infile.qs.seasonal <- renderUI({NULL})
      return(NULL)
    }else if(input$selectbox.qs.seasonal == "file"){
      output$render.infile.qs.seasonal <- renderUI({
        fileInput('infile.qs.seasonal', '',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'))
      })
      if(length(input$infile.qs.seasonal) == 0){return(NULL)}
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({ # factors for qs.seasonal
    if(input$selectbox.qs.seasonal == "none" & input$checkbox.qs.seasonal.plot == TRUE){
      qs.seasonal <<- list(jan=1, feb=1, mar=1, apr=1, may=1, jun=1, 
                           jul=1, aug=1, sep=1, oct=1, nov=1, dic=1) # actual factors for EmiStatR
      qs.seasonal.plot <- cbind.data.frame(names(qs.seasonal), as.numeric(c(1:12)), as.numeric(1))
      colnames(qs.seasonal.plot) <- c("Month", "Month number", "Factor [--]")
      
      output$plot.qs <- renderPlot(
        {
          x11()
          qplot(qs.seasonal.plot[,"Month number"], qs.seasonal.plot[,"Factor [--]"]) + 
            geom_point() + geom_line() + 
            xlab("Day") + ylab("Factor [--]") +
            ggtitle("Seasonal factors for water consumption, qs") +
            scale_x_continuous(breaks = qs.seasonal.plot[,"Month number"], labels=qs.seasonal.plot[,"Month"])
        },
        width = 600, height=500, res = 75)
    }else if(length(input$infile.qs.seasonal) == 0){
      updateCheckboxInput(session=session, inputId="checkbox.qs.seasonal.plot", value=FALSE )
      output$plot.qs.seasonal <- renderPlot({NULL})
      return(NULL)
    }else if(length(input$infile.qs.seasonal) > 0 & input$checkbox.qs.seasonal.plot == TRUE){
      qs.seasonal.file <- input$infile.qs.seasonal$datapath
      qs.seasonal <<- read.csv(qs.seasonal.file, header = TRUE) # actual factors for EmiStatR
      
      output$plot.qs <- renderPlot(
        {
          x11()
          qplot(c(1:nrow(qs.seasonal)), qs.seasonal[,2]) + geom_point() + geom_line() +
            xlab("Time") + ylab("Factor [--]") +
            ggtitle("Seasonal factors for water consumption, qs") +
            scale_x_continuous(breaks = c(1:nrow(qs.seasonal)), labels=qs.seasonal[,1])
        }, 
        width = 600, height=500, res = 75)
    }else if(input$checkbox.qs.seasonal.plot == FALSE){
      output$plot.qs.seasonal <- renderPlot({NULL})
    }
    setwd(dir.current)
  })
  #-----------------------------------------------------------------------------------------
  observe({ # render infile for pe.daily
    if(input$selectbox.pe.daily == "none"){
      output$render.infile.pe.daily <- renderUI({NULL})
      return(NULL)
    }else if(input$selectbox.pe.daily == "file"){
      output$render.infile.pe.daily <- renderUI({
        fileInput('infile.pe.daily', '',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'))
      })
      if(length(input$infile.pe.daily) == 0){return(NULL)}
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({ # factors for pe.daily
    if(input$selectbox.pe.daily == "none" & input$checkbox.pe.daily.plot == TRUE){
      deltat <- 120 # mins
      time   <- seq(as.POSIXct(as.Date(Sys.time()), format = "%H:%M:%S"), by = deltat*60, length.out = 12)
      daily.factors    <- as.xts(rep(1, 12), order.by = time)
      
      pe.daily <<- data.frame(cbind(index(daily.factors), coredata(daily.factors))) # actual factors for EmiStatR
      colnames(pe.daily) <- c("time", "factor")
      
      xlabels <- index(daily.factors)
      xlabels <- format(xlabels, "%H:%M")
      
      output$plot.pe <- renderPlot(
        {
          x11()
          qplot(pe.daily[,"time"], pe.daily[,"factor"]) + geom_point() + geom_line() +
            xlab("Time") + ylab("Factor [--]") +
            ggtitle("Daily factors for population equivalent, pe") +
            scale_x_continuous(breaks = pe.daily[,"time"], labels=xlabels)
          
        }, 
        width = 300, height=250, res = 75)
    }else if(length(input$infile.pe.daily) == 0){
      updateCheckboxInput(session=session, inputId="checkbox.pe.daily.plot", value=FALSE )
      output$plot.pe.daily <- renderPlot({NULL})
      return(NULL)
    }else if(length(input$infile.pe.daily) > 0 & input$checkbox.pe.daily.plot == TRUE){
      pe.daily.file <- input$infile.pe.daily$datapath
      
      pe.daily <<- read.csv(pe.daily.file, header = TRUE) # actual factors for EmiStatR
      a        <- IsReg(data = pe.daily, format="%H:%M:%S", tz="UTC")
      daily.factors <- a[[2]]
      
      output$plot.pe <- renderPlot(
        {
          x11()
          autoplot(daily.factors) + geom_point() + geom_line() +
            xlab("Time") + ylab("Factor [--]") +
            ggtitle("Daily factors for population equivalent, pe")
        }, 
        width = 300, height=250, res = 75)
    }else if(input$checkbox.pe.daily.plot == FALSE){
      output$plot.pe.daily <- renderPlot({NULL})
    }
    setwd(dir.current)
  })
  #-----------------------------------------------------------------------------------------
  observe({ # render infile for pe.weekly
    if(input$selectbox.pe.weekly == "none"){
      output$render.infile.pe.weekly <- renderUI({NULL})
      return(NULL)
    }else if(input$selectbox.pe.weekly == "file"){
      output$render.infile.pe.weekly <- renderUI({
        fileInput('infile.pe.weekly', '',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'))
      })
      if(length(input$infile.pe.weekly) == 0){return(NULL)}
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({ # factors for pe.weekly
    if(input$selectbox.pe.weekly == "none" & input$checkbox.pe.weekly.plot == TRUE){
      deltat <- 120 # mins
      time   <- seq(as.POSIXct(as.Date(Sys.time()), format = "%H:%M:%S"), by = deltat*60, length.out = 12)
      weekly.factors    <- as.xts(rep(1, 12), order.by = time)
      
      pe.weekly <<- list(mon=1, tue=1, wed=1, thu=1, fri=1, sat=1, sun=1 ) # actual factors for EmiStatR
      pe.weekly.plot <- cbind.data.frame(names(pe.weekly), as.numeric(c(1:7)), as.numeric(1))
      colnames(pe.weekly.plot) <- c("Day", "Day number", "Factor [--]")
      
      output$plot.pe <- renderPlot(
        {
          x11()
          qplot(pe.weekly.plot[,"Day number"], pe.weekly.plot[,"Factor [--]"]) + 
            geom_point() + geom_line() + 
            xlab("Day") + ylab("Factor [--]") +
            ggtitle("Weekly factors for population equivalent, pe") +
            scale_x_continuous(breaks = pe.weekly.plot[,"Day number"], labels=pe.weekly.plot[,"Day"])
        },
        width = 600, height=500, res = 75)
    }else if(length(input$infile.pe.weekly) == 0){
      updateCheckboxInput(session=session, inputId="checkbox.pe.weekly.plot", value=FALSE )
      output$plot.pe.weekly <- renderPlot({NULL})
      return(NULL)
    }else if(length(input$infile.pe.weekly) > 0 & input$checkbox.pe.weekly.plot == TRUE){
      pe.weekly.file <- input$infile.pe.weekly$datapath
      pe.weekly <<- read.csv(pe.weekly.file, header = TRUE) # actual factors for EmiStatR
      
      output$plot.pe <- renderPlot(
        {
          x11()
          qplot(c(1:nrow(pe.weekly)), pe.weekly[,2]) + geom_point() + geom_line() +
            xlab("Time") + ylab("Factor [--]") +
            ggtitle("Weekly factors for population equivalent, pe") +
            scale_x_continuous(breaks = c(1:nrow(pe.weekly)), labels=pe.weekly[,1])
        }, 
        width = 600, height=500, res = 75)
    }else if(input$checkbox.pe.weekly.plot == FALSE){
      output$plot.pe.weekly <- renderPlot({NULL})
    }
    setwd(dir.current)
  })
  #-----------------------------------------------------------------------------------------
  observe({ # render infile for pe.seasonal
    if(input$selectbox.pe.seasonal == "none"){
      output$render.infile.pe.seasonal <- renderUI({NULL})
      return(NULL)
    }else if(input$selectbox.pe.seasonal == "file"){
      output$render.infile.pe.seasonal <- renderUI({
        fileInput('infile.pe.seasonal', '',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'))
      })
      if(length(input$infile.pe.seasonal) == 0){return(NULL)}
    }
  })
  #-----------------------------------------------------------------------------------------
  observe({ # factors for pe.seasonal
    if(input$selectbox.pe.seasonal == "none" & input$checkbox.pe.seasonal.plot == TRUE){
      pe.seasonal <<- list(jan=1, feb=1, mar=1, apr=1, may=1, jun=1, 
                           jul=1, aug=1, sep=1, oct=1, nov=1, dic=1) # actual factors for EmiStatR
      pe.seasonal.plot <- cbind.data.frame(names(pe.seasonal), as.numeric(c(1:12)), as.numeric(1))
      colnames(pe.seasonal.plot) <- c("Month", "Month number", "Factor [--]")
      
      output$plot.pe <- renderPlot(
        {
          x11()
          qplot(pe.seasonal.plot[,"Month number"], pe.seasonal.plot[,"Factor [--]"]) + 
            geom_point() + geom_line() + 
            xlab("Day") + ylab("Factor [--]") +
            ggtitle("Seasonal factors for water consumption, pe") +
            scale_x_continuous(breaks = pe.seasonal.plot[,"Month number"], labels=pe.seasonal.plot[,"Month"])
        },
        width = 600, height=500, res = 75)
    }else if(length(input$infile.pe.seasonal) == 0){
      updateCheckboxInput(session=session, inputId="checkbox.pe.seasonal.plot", value=FALSE )
      output$plot.pe.seasonal <- renderPlot({NULL})
      return(NULL)
    }else if(length(input$infile.pe.seasonal) > 0 & input$checkbox.pe.seasonal.plot == TRUE){
      pe.seasonal.file <- input$infile.pe.seasonal$datapath
      pe.seasonal <<- read.csv(pe.seasonal.file, header = TRUE) # actual factors for EmiStatR
      
      output$plot.pe <- renderPlot(
        {
          x11()
          qplot(c(1:nrow(pe.seasonal)), pe.seasonal[,2]) + geom_point() + geom_line() +
            xlab("Time") + ylab("Factor [--]") +
            ggtitle("Seasonal factors for water consumption, pe") +
            scale_x_continuous(breaks = c(1:nrow(pe.seasonal)), labels=pe.seasonal[,1])
        }, 
        width = 600, height=500, res = 75)
    }else if(input$checkbox.pe.seasonal.plot == FALSE){
      output$plot.pe.seasonal <- renderPlot({NULL})
    }
    setwd(dir.current)
  })
  #-----------------------------------------------------------------------------------------
  observe({
    # dir
    volumes <<- getVolumes()
    
    shinyDirChoose(input, 'directory', roots=volumes)
    directory <- reactive(input$directory)
    output$directory <- renderPrint(directory())
    
    # path
    path <- reactive({
      home <- Nocher-RoutemalizePath("~")
      file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    })
    
    # files
    output$files <- renderPrint(list.files(path()))
    
    if(length(parseDirPath(volumes, input$directory)) < 1){
      # showNotification("Please, choose directory.")
    }else{
      output$actionButton.save <- renderUI({
        actionButton("save", label = "Save")
      }) 
    }
  })
  #-----------------------------------------------------------------------------------------
  output$valorPath <- renderPrint(
    {parseDirPath(volumes, input$directory)}
  )
  #-----------------------------------------------------------------------------------------
  # define current directory
  observe({
    if(length(parseDirPath(volumes, input$directory)) > 0){
      cdir <<- parseDirPath(volumes, input$directory)
    }
  })
  #-----------------------------------------------------------------------------------------
  # take an action, save, when button is clicked
  observeEvent(input$save,
               { id <- isolate(input$id)
               ns   <- isolate(input$ns)
               nm   <- isolate(input$nm)
               nc   <- isolate(input$nc)
               numc <- isolate(input$numc)
               use  <- isolate(input$use)
               Atotal <- isolate(input$Atotal)
               Aimp <- isolate(input$Aimp)
               tfS  <- isolate(input$tfS)
               pe   <- isolate(input$pe)
               V    <- isolate(input$V)
               lev.ini <- isolate(input$lev.ini)
               Qd   <- isolate(input$Qd)
               Ad   <- isolate(input$Ad)
               Cd   <- isolate(input$Cd)
               
               assign(paste0("E",id), list(id=id, ns=ns,nm=nm,nc=nc,numc=numc,use=use,
                                           Atotal=Atotal,Aimp=Aimp,tfS=tfS,pe=pe,
                                           V=V, lev2vol=lev2vol, lev.ini=lev.ini,
                                           Qd=Qd, Ad=Ad, Cd=Cd))
               
               setwd(cdir)
               save(list=ls(pattern = paste0("E", id)), file=paste("E",id,"_inputCSO.RData", sep=""))
               showNotification(paste0("E", id, " saved"))
               setwd(cd)
               }
  )
  #-----------------------------------------------------------------------------------------
  # take an action, close, when button is clicked
  observe({
    ifelse (input$close == 0,
            return(),
            {shiny::stopApp
              stopApp(returnValue = "closed")
            }
            
    )
  })
  #-----------------------------------------------------------------------------------------
})