# creation of input server.R file for UI of the EmiStat model, CSO structures
# author: Arturo Torres, Kai Klepiszewski
# organization: LIST
# date: 09.02.2015

# ns  : name of the structure

# 1.    catchment data
# id  : identification number [-]
# nm  : name of the municipality [-]
# nc  : name of the catchment [-]
# numc: number of the catchment [-]
# use : use of the soil [-]
# Ages: total area [ha]
# Ared: reduced area [ha]
# tfS : flow time structure [min]
# af  : reduction factor [-]
# pe  : population equivalent [PE]

# 2.    Structure data
# Qd  : throttled outflow [l/s]:
# V   : volume [m3]
#-----------------------------------------------------------------------------------------


library(shiny)

# Define server logic 
shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of
  # the values
  val1 <- reactive({
    # Compose data frame
    data.frame(
      Name = c("ID","Structure:"),
      Value = as.character(c(input$id,input$ns)), 
      stringsAsFactors=FALSE)
  })
  
  
  # change directory for loading af
  setwd("..")
  cd <- getwd()
  nd <- paste(cd, "/EmiStatR_input/input", sep="")
  setwd(nd)
  
  val2 <- reactive({
    # calculate af
    load("flowTimeDelay.RData")
    af <- ifelse(input$tfS <= tf, 1, 0.5+50/(input$tfS+100))
    #af = 100
    
    # Compose data frame
    data.frame(
      Name = c("Municipality:",
               "Catchment name:",
               "Catchment number",
               "Use",
               "Total area, Ages [ha]",
#                tags$div(HTML(paste("Total area, A", 
#                                    tags$sub("ges"), "[ha]:" ,
#                                    sep = "")))
               "Reduced area, Ared [ha]",
               "Flow time structure, tfs [min]",
               "Reduction factor, af [-]:",
               "Population equivalent [PE]:",
               "cd"),
      Value = as.character(c(input$nm,
                           input$nc,
                           input$numc,
                           input$use,
                           input$Ages,
                           input$Ared,
                           input$tfS,
                           round(af, digits =2), 
                           input$pe,
                           cd)), 
      stringsAsFactors=FALSE)
  })

val3 <- reactive({
  # Compose data frame
  data.frame(
    Name = c("Throttled outflow, Qd [l/s]:",
             "Volume, V [m3]:"),
    Value = as.character(c(input$Qd,
                           input$V)), 
    stringsAsFactors=FALSE)
})
  # Show the values using an HTML table
  
  output$v1 <- renderTable({
    val1()
  })
  
  output$v2 <- renderTable({
    val2()
  })

  output$v3 <- renderTable({
  val3()
})

# take an action, save, when button is clicked
observe({
  ifelse (input$save == 0,
          return(),
          c(id <- isolate(input$id),
          ns   <- isolate(input$ns),
          nm   <- isolate(input$nm),
          nc   <- isolate(input$nc),
          numc <- isolate(input$numc),
          use <- isolate(input$use),
          Ages <- isolate(input$Ages),
          Ared <- isolate(input$Ared),
          tfS  <- isolate(input$tfS),
          pe   <- isolate(input$pe),
          Qd   <- isolate(input$Qd),
          V    <- isolate(input$V),
          a <- getwd()
          ))
          
          setwd("..") 
          setwd("..") 
          cd <- getwd()
          nd <- paste(cd, "/EmiStatR_inputCSO", sep="")
          setwd(nd)
          dir.create(paste("inputCSO",sep=""))        
          cd <- getwd()
          nd <- paste(cd, "/","inputCSO", sep="")
          setwd(nd)
          
          #save(id,ns,nm,nc,numc,Ages,Ared,tfS,pe, 
          #     file=paste(ns,"_Catchment.RData", sep=""))  
          #save(Qd,V,file=paste(ns,"_Structure.RData", sep="")) 
  
          #(list=sapply(mget(ls(), .GlobalEnv), is.list))
          assign(paste("E",id,sep=""),setNames(list(id,ns,nm,nc,numc,use,Ages,Ared,tfS,pe,Qd,V), 
                                        c("id","ns","nm","nc","numc","use","Ages","Ared","tfS","pe","Qd","V"))) 
          rm(list=setdiff(ls(), c(paste("E",id,sep=""),"id")))
          save(list=ls(),file=paste("E",id,"_inputCSO.RData", sep=""))
  
          # close the application
          ?shiny::stopApp
          stopApp(returnValue = "saved")
  })
})