# creation of input server.R file for UI of the EmiStatR model, CSO structures
# author: Arturo Torres, Kai Klepiszewski
# organization: LIST
# date1: 09.02.2015
# date2: 07.09.2017 - 12.09.2017

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

# Define server logic 
shinyServer(function(input, output, session) {
  #-----------------------------------------------------------------------------------------
  volumes <<- getVolumes()
  cd <- getwd()
  
  #-----------------------------------------------------------------------------------------
  observe({
    # change directory for loading
    # setwd("..")
    nd <- paste0(cd, "/inputCSO")
    setwd(nd)
    
    ## load existing CSO and update fields or create new
    if(input$radio.CSO == "E1"){
      load("E1_inputCSO.RData")
      updateNumericInput(session = session, inputId = "id", value =  as.numeric(E1["id"]))
      updateTextInput(session = session, inputId = "ns", value =  as.character(E1["ns"]))
      updateTextInput(session = session, inputId = "nm", value =  as.character(E1["nm"]))
      updateTextInput(session = session, inputId = "nc", value =  as.character(E1["nc"]))
      updateNumericInput(session = session, inputId = "numc", value =  as.numeric(E1["numc"]))
      updateTextInput(session = session, inputId = "use", value =  as.character(E1["use"]))
      updateNumericInput(session = session, inputId = "Atotal", value =  as.numeric(E1["Atotal"]))
      updateNumericInput(session = session, inputId = "Aimp", value =  as.numeric(E1["Aimp"]))
      updateNumericInput(session = session, inputId = "tfS", value =  as.numeric(E1["tfS"]))
      updateNumericInput(session = session, inputId = "pe", value =  as.numeric(E1["pe"]))
      updateNumericInput(session = session, inputId = "V", value =  as.numeric(E1["V"]))
      
      lev2vol <<- E1[["lev2vol"]]
      updateCheckboxInput(session=session, inputId="checkbox.plot", label = "Plot", value = FALSE)
      
      updateNumericInput(session = session, inputId = "lev.ini", value =  as.numeric(E1["lev.ini"]))
      updateNumericInput(session = session, inputId = "Qd", value =  as.numeric(E1["Qd"]))
      updateNumericInput(session = session, inputId = "Ad", value =  as.numeric(E1["Ad"]))
      updateNumericInput(session = session, inputId = "Cd", value =  as.numeric(E1["Cd"]))
    }else if(input$radio.CSO == "E2"){
      load("E2_inputCSO.RData")
      updateNumericInput(session = session, inputId = "id", value =  as.numeric(E2["id"]))
      updateTextInput(session = session, inputId = "ns", value =  as.character(E2["ns"]))
      updateTextInput(session = session, inputId = "nm", value =  as.character(E2["nm"]))
      updateTextInput(session = session, inputId = "nc", value =  as.character(E2["nc"]))
      updateNumericInput(session = session, inputId = "numc", value =  as.numeric(E2["numc"]))
      updateTextInput(session = session, inputId = "use", value =  as.character(E2["use"]))
      updateNumericInput(session = session, inputId = "Atotal", value =  as.numeric(E2["Atotal"]))
      updateNumericInput(session = session, inputId = "Aimp", value =  as.numeric(E2["Aimp"]))
      updateNumericInput(session = session, inputId = "tfS", value =  as.numeric(E2["tfS"]))
      updateNumericInput(session = session, inputId = "pe", value =  as.numeric(E2["pe"]))
      updateNumericInput(session = session, inputId = "V", value =  as.numeric(E2["V"]))
      
      lev2vol <<- E2[["lev2vol"]]
      updateCheckboxInput(session=session, inputId="checkbox.plot", label = "Plot", value = FALSE)
      
      updateNumericInput(session = session, inputId = "lev.ini", value =  as.numeric(E2["lev.ini"]))
      updateNumericInput(session = session, inputId = "Qd", value =  as.numeric(E2["Qd"]))
      updateNumericInput(session = session, inputId = "Ad", value =  as.numeric(E2["Ad"]))
      updateNumericInput(session = session, inputId = "Cd", value =  as.numeric(E2["Cd"]))
    }else if(input$radio.CSO == "E3"){
      load("E3_inputCSO.RData")
      updateNumericInput(session = session, inputId = "id", value =  as.numeric(E3["id"]))
      updateTextInput(session = session, inputId = "ns", value =  as.character(E3["ns"]))
      updateTextInput(session = session, inputId = "nm", value =  as.character(E3["nm"]))
      updateTextInput(session = session, inputId = "nc", value =  as.character(E3["nc"]))
      updateNumericInput(session = session, inputId = "numc", value =  as.numeric(E3["numc"]))
      updateTextInput(session = session, inputId = "use", value =  as.character(E3["use"]))
      updateNumericInput(session = session, inputId = "Atotal", value =  as.numeric(E3["Atotal"]))
      updateNumericInput(session = session, inputId = "Aimp", value =  as.numeric(E3["Aimp"]))
      updateNumericInput(session = session, inputId = "tfS", value =  as.numeric(E3["tfS"]))
      updateNumericInput(session = session, inputId = "pe", value =  as.numeric(E3["pe"]))
      updateNumericInput(session = session, inputId = "V", value =  as.numeric(E3["V"]))
      
      lev2vol <<- E3[["lev2vol"]]
      updateCheckboxInput(session=session, inputId="checkbox.plot", label = "Plot", value = FALSE)
      
      updateNumericInput(session = session, inputId = "lev.ini", value =  as.numeric(E3["lev.ini"]))
      updateNumericInput(session = session, inputId = "Qd", value =  as.numeric(E3["Qd"]))
      updateNumericInput(session = session, inputId = "Ad", value =  as.numeric(E3["Ad"]))
      updateNumericInput(session = session, inputId = "Cd", value =  as.numeric(E3["Cd"]))
    }else if(input$radio.CSO == "none"){
      updateNumericInput(session = session, inputId = "id", value =  "")
      updateTextInput(session = session, inputId = "ns", value =  "")
      updateTextInput(session = session, inputId = "nm", value =  "")
      updateTextInput(session = session, inputId = "nc", value =  "")
      updateNumericInput(session = session, inputId = "numc", value =  "")
      updateTextInput(session = session, inputId = "use", value =  "")
      updateNumericInput(session = session, inputId = "Atotal", value =  "")
      updateNumericInput(session = session, inputId = "Aimp", value =  "")
      updateNumericInput(session = session, inputId = "tfS", value =  "")
      updateNumericInput(session = session, inputId = "pe", value =  "")
      updateNumericInput(session = session, inputId = "V", value =  "")
      
      lev2vol <<- NULL
      lev2vol.file <<- NULL
      updateCheckboxInput(session=session, inputId="checkbox.plot", label = "Plot", value = FALSE)
      
      updateNumericInput(session = session, inputId = "lev.ini", value =  "")
      updateNumericInput(session = session, inputId = "Qd", value =  "")
      updateNumericInput(session = session, inputId = "Ad", value =  "")
      updateNumericInput(session = session, inputId = "Cd", value =  "")
    }
    setwd(cd)
  })
  #-----------------------------------------------------------------------------------------
  observe({
    ## load existing CSO and update fields or create new
    if(input$new != 0){
      
      updateRadioButtons(session = session, inputId = "radio.CSO", selected = "none")
      
      updateNumericInput(session = session, inputId = "id", value =  "")
      updateTextInput(session = session, inputId = "ns", value =  "")
      updateTextInput(session = session, inputId = "nm", value =  "")
      updateTextInput(session = session, inputId = "nc", value =  "")
      updateNumericInput(session = session, inputId = "numc", value =  "")
      updateTextInput(session = session, inputId = "use", value =  "")
      updateNumericInput(session = session, inputId = "Atotal", value =  "")
      updateNumericInput(session = session, inputId = "Aimp", value =  "")
      updateNumericInput(session = session, inputId = "tfS", value =  "")
      updateNumericInput(session = session, inputId = "pe", value =  "")
      updateNumericInput(session = session, inputId = "V", value =  "")
      
      lev2vol <<- NULL
      lev2vol.file <<- NULL
      updateCheckboxInput(session=session, inputId="checkbox.plot", label = "Plot", value = FALSE)
      
      updateNumericInput(session = session, inputId = "lev.ini", value =  "")
      updateNumericInput(session = session, inputId = "Qd", value =  "")
      updateNumericInput(session = session, inputId = "Ad", value =  "")
      updateNumericInput(session = session, inputId = "Cd", value =  "")
      
      # render file input
      output$render.file.lev2vol <- renderUI({
        fileInput('file.lev2vol', '',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'))
      })
    }
    setwd(cd)
  })
  #-----------------------------------------------------------------------------------------
  # Reactive expression to compose a data frame containing all of
  # the values
  val1 <- reactive({
    # Compose data frame
    data.frame(
      Variable = c("ID","Structure:"),
      Value = as.character(c(input$id,input$ns)), 
      stringsAsFactors=FALSE)
  })
  
  #-----------------------------------------------------------------------------------------
  val2 <- reactive({
    # Compose data frame
    data.frame(
      Variable = c("Municipality:",
               "Catchment name:",
               "Catchment number",
               "Use",
               "Total area, Atotal [ha]",
               "Impervious area, Aimp [ha]",
               "Flow time structure, tfS [min]",
               "Population equivalent [PE]:"),
      Value = as.character(c(input$nm,
                             input$nc,
                             input$numc,
                             input$use,
                             input$Atotal,
                             input$Aimp,
                             input$tfS,
                             input$pe)), 
      stringsAsFactors=FALSE)
  })
  #-----------------------------------------------------------------------------------------
  val3 <- reactive({
    # Compose data frame
    if(input$radio.CSO == "E1"){ 
      lev2vol.file <- "level-volume_E1.csv"
    }else if(input$radio.CSO == "E2"){
      lev2vol.file <- "level-volume_E2.csv"
    }else if(input$radio.CSO == "E3"){
      lev2vol.file <- "level-volume_E3.csv"
    }else if(input$radio.CSO == "none" & length(lev2vol.file)==0){
      lev2vol.file <- "(choose a file)"
    }else lev2vol.file <- as.character(input$file.lev2vol$name)
    
    data.frame(
      Variable = c("Volume, V [m3]:",
               "Relationship level [m] - volume [m3]",
               "Initial water level, Lev_ini [m]:",
               "Maximum throttled outflow, Qd_max [l/s]:",
               "Orifice area, Ad [m2]:",
               "Orifice discharge coefficient, Cd [-]:"),
      Value = as.character(c(input$V,
                             lev2vol.file,
                             input$lev.ini,
                             input$Qd,
                             input$Ad,
                             input$Cd)), 
      stringsAsFactors=FALSE)
  })
  #-----------------------------------------------------------------------------------------
  # Show the values using an HTML table
  
  output$v1 <- renderTable({
    val1()
  })
  #-----------------------------------------------------------------------------------------
  output$v2 <- renderTable({
    val2()
  })
  #-----------------------------------------------------------------------------------------
  output$v3 <- renderTable({
    val3()
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
      home <- normalizePath("~")
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
  observe({
    inFile <<- input$file.lev2vol

    if (is.null(inFile)) return(NULL)

    data <- read.csv(inFile$datapath, header=TRUE)

    # creating lev2vol list
    lev2vol <<- list(lev = data[,1], vol = data[,2])

  })
  #-----------------------------------------------------------------------------------------
  observe({
    if (input$checkbox.plot == FALSE) return(NULL)
    x11()
    
    if(length(lev2vol) == 0){
      return(NULL)
    }else {
      x11()
      plot(lev2vol[["vol"]], lev2vol[["lev"]], typ="b", col = "blue",
           xlab = "Volume [m3]", ylab = "Level [m]",
           main = "Relationship level - volume")
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