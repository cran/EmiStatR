# creation of input server.R file for UI of the EmiStat model
# author: Arturo Torres, Kai Klepiszewski
# organization: LIST
# date: 06.02.2015 - 16.02.2016

# 1.    wastewater
# qs  : individual water consumption of households [l/(PE d)]
# CODs: sewage pollution - COD concentration [g/(PE d)]
# NH4s: sewage pollution - NH4 concentration [g/(PE d)]
# 
# 2.    infiltration water
# qf  : infiltration water inflow [l/(s ha)]
# CODf: infiltration water pollution - COD concentration [g/(PE d)]
# NH4f: infiltration water pollution - NH4 concentration [g/(PE d)]
# 
# 3.    rainwater
# CODr: rainwater pollution - COD concentration [mg/l]
# NH4r: rainwater pollution - NH4 concentration [mg/l]
# stat: name of the rain measurement station
# peri: period of analysis of rainfall 
# dura: duration of the period of analysis of rainfall [year]
# deep: mean height of precipitation [mm]
# pDur: mean duration of the rain [min]
# iMin: minimum intensity [mm/(dt min)]
# dt  : rain time interval [min]
# 
# 4.    stormwater runoff
# tf  : flow time in the sewer system [min], (if tf≤20min, then af = 1)
# 
# P1: data.frame containing columns tt (date and time), P (rain time series), and i (intensity)

#-----------------------------------------------------------------------------------------
library(shiny)

# Define server logic 
shinyServer(function(input, output) {
  
  cd <- getwd()
  dir.create("input")
  nd <- paste(cd, "/input", sep="")
  
  setwd(nd)
  dir.create("tmp")
  nd_tmp <- paste(cd, "/input/tmp", sep="")
  
  
  # Reactive expression to compose a data frame containing all of
  # the values
  sliderValores1 <- reactive({
    # Compose data frame
    data.frame(
      Name = c("Water consumption, qs [l/(PE d)]", 
               "Pollution COD [g/(PE d)]",
               "Pollution NH4 [g/(PE d)]"),
      Value = as.character(c(input$qs, 
                             input$CODs,
                             input$NH4s)), 
      stringsAsFactors=FALSE)   
    }) 
  
  
  sliderValores2 <- reactive({
    # Compose data frame
    data.frame(
      Name = c("Inflow, qf [l/(s ha)]:",
               "Pollution COD [g/(PE d)]:",
               "Pollution NH4 [g/(PE d)]:"),
      Value = as.character(c( 
                             input$qf,
                             input$CODf,
                             input$NH4f)), 
      stringsAsFactors=FALSE)
  }) 
  
 
  sliderValores3 <- reactive({
#     # duration of the period calculation
#     dura = input$peri[2]-input$peri[1]+1                              #  <--------------------
 
    # save variables
    CODr  <- isolate(input$CODr)
    NH4r  <- isolate(input$NH4r)
    stat  <- isolate(input$stat)
    # peri  <- isolate(input$peri)                #  <--------------------
    
    #cd <- getwd()
    #nd <- paste(cd, "/output", sep="")
    setwd(nd_tmp)
    #save(CODr, NH4r, stat, peri, dura, file="rainwater.RData")
    rw <- list(CODr, NH4r, stat)        #  <--------------------
    rw <- setNames(rw, c("CODr", "NH4r", "stat"))
    save(rw, file="rainwater.RData")
    setwd(cd)
    
    
    # Compose data frame    
    data.frame(
      Name = c("Pollution COD [mg/l]:",
               "Pollution NH4 [mg/l]:",
               "Rain measurement station:" #,    #  <--------------------
               #"Period:",    #  <--------------------
               #"Duration of the period [year]:"   #  <--------------------
               ),
      Value = as.character(c( 
                            input$CODr,
                            input$NH4r,
                            input$stat #,
                            #paste(input$peri, collapse=' - '),   #  <--------------------
                            #dura   #  <--------------------
                            )), 
      stringsAsFactors=FALSE)
  }) 
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)  
    })
  
  
  readValores4 <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
    
    ## convert date-time to POSIXct POSIXt
    # tt <- as.data.frame(strptime(data[,1], format='%d/%m/%Y %H:%M')) # original time series
    tt <- strptime(data[,1], format='%Y-%m-%d %H:%M:%S') # final time series
    
    # calculate total height of precipitation
    a <- as.data.frame(data)
    deep <- sum(a[,2])
    
    # calculate time step
    dtt <- difftime(tt[2], tt[1], units="min")
    dt <- as.numeric(dtt)
    
    # calculate intensity
    # f2 <- function(x) ifelse(x==0,1,x)
    #f2 <- function(x) {x/dt}                  # <-------------------
    #i <- as.data.frame(sapply(a[,2], f2))   # <-------------------
    #colnames(i) <- c("i")    # <-------------------
    #ii <- summary(i)         # <-------------------
    
    # calculate duration of the rain
    f1 <- function(x) ifelse(x>0,1,0)
    c <- sapply(a[,2], f1)                         # <=================
    pDur <- sum(c)*dt                              # <=================   
    # pDur <- 120
    
    # save variables
      setwd(nd_tmp)
      load("rainwater.RData")
    #  save(deep, pDur, iMin, CODr, NH4r,
    #     stat, peri, dura, dt, file="rainwater.RData")
       rw <- list(deep, pDur, rw$CODr, rw$NH4r,                         # <-------------------
                   rw$stat, dt)     #  <--------------------
       rw <- setNames(rw, c("deep", "pDur", "CODr", "NH4r",            # <-------------------
                            "stat", "dt"))    #  <--------------------
       save(rw, file="rainwater.RData")
    
      #save(i, file="i.RData")
      setwd(cd)
      
    # Compose data frame
    # dt = 10
    data.frame(
      Name = c("Mean height [mm]:",
               "Mean duration [min]:",
               # "Minimum intensity [mm/min]",     # <-------------------
               # "Mean intensity [mm/min]",        # <-------------------
               # "Maximum intensity [mm/min]",     # <-------------------
               "Time interval [min]"),
      Value = as.character(c( 
                            deep,
                            pDur, 
                            #ii[1],                # <-------------------
                            #ii[4],                # <-------------------
                            #ii[6],                # <-------------------
                            dt
      )), 
      stringsAsFactors=FALSE)
  })

  output$plot1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL) 
   
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                     quote=input$quote)
      # tt <- strptime(data[,1], format='%d/%m/%Y %H:%M') # original time series    # <-------------------
      tt <- strptime(data[,1], format='%Y-%m-%d %H:%M:%S') # final time series
      
      # save data
      data0 <- as.data.frame(tt)
      # dimtt <- dim(as.data.frame(tt))
      # dimdata2 <- dim(as.data.frame(data[,2]))
      data0 <- cbind.data.frame(tt, as.data.frame(data[,2]))
      
      setwd(nd_tmp)
      #i <- 1
      #save(i, file="i.RData")
      #load("i.RData")         # <------------------
      
      P1 <- cbind.data.frame(data0) # include intensity  # <------------------ 
      colnames(P1) <- c("Time [y-m-d h:m:s]", "P [mm]")
      save(P1, file="P1.RData")
#       write.table(P1, file = "data.csv", sep = ",", col.names = c("Time [y-m-d h:m:s]", "P [mm]"), 
#                    qmethod = "double")
      write.table(P1, file = "P1.csv", sep = ",", qmethod = "double")
      setwd(cd)
    
      # creating plot
      daterange <- c(min(tt), max(tt))
      plot(tt, data[,2], type ="l", xaxt="n")
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), 
                   format="%b-%Y") #label the x axis by months
    })


 sliderValores5 <- reactive({
   # Compose data frame    
   data.frame(
     Name = c("Flow time in the sewer system [min]:"),
     Value = as.character(c( 
       input$tf
     )), 
     stringsAsFactors=FALSE)
 })
  
  # Show the values using an HTML table
  output$valores1 <- renderTable({
    sliderValores1()
  })
  
  output$valores2 <- renderTable({
    sliderValores2()
  })
  
  output$valores3 <- renderTable({
    sliderValores3()
  })
  
  output$valores4 <- renderTable({
    readValores4()
  })
 
 output$valores5 <- renderTable({
   sliderValores5()
 })

# take an action, save, when button is clicked
observe({
  ifelse (input$save == 0,
          return(),
          c(qs   <- isolate(input$qs),
            CODs <- isolate(input$CODs),
            NH4s <- isolate(input$NH4s),
            
            qf   <- isolate(input$qf),
            CODf <- isolate(input$CODf),
            NH4f <- isolate(input$NH4f),
 
            tf  <- isolate(input$tf)
        ))
  
  # loading variables for saving
  setwd(nd_tmp)
  load("rainwater.RData")
  load("P1.RData")
  #file.remove(list.files()) # deleting all temporal files
  
  # saving variables
  setwd(nd)
  
  #save(id,ns,nm,nc,numc,Ages,Ared,tfS,pe, 
  #     file=paste(ns,"_Catchment.RData", sep=""))  
  #save(Qd,V,file=paste(ns,"_Structure.RData", sep="")) 
  
  #(list=sapply(mget(ls(), .GlobalEnv), is.list))
  ww <- setNames(list(cd,qs,CODs,NH4s),c("cd","qs","CODs","NH4s"))
  save(ww,file=paste("wastewater.RData", sep=""))

  inf <- setNames(list(qf,CODf,NH4f),c("qf","CODf","NH4f"))
  save(inf,file=paste("infiltration.RData", sep=""))

  rw <- setNames(list(rw$deep, rw$pDur, rw$CODr, rw$NH4r,       # <------------------
                      rw$stat, rw$dt),    #  <--------------------
                 c("deep", "pDur", "CODr", "NH4r",             # <------------------
                  "stat", "dt"))    #  <--------------------
  save(rw,file=paste("rainwater.RData", sep=""))
  
  save(tf, file="flowTimeDelay.RData")
  
  save(P1, file="P1.RData")
  write.table(P1, file = "P1.csv", sep = ",", 
              qmethod = "double")

  shiny::stopApp
  stopApp(returnValue = "saved")
  
  setwd(cd)
})
})