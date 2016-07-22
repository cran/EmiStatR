# model EmiStat-R version 1_2
# author: J.A. Torres-Matallana, K. Klepiszewski
# organization: LIST
# date: 11.02.2015 - 22.07.2016

# ns    : name of the structure
# 
# 1.    catchment data
# id    : identification number [-]
# ns    : name of the structure [-]
# nm    : name of the municipality [-]
# nc    : name of the catchment [-]
# numc  : number of the catchment [-]
# use   : use of the soil [-]
# Ages  : total area [ha]
# Ared  : reduced area - impervious area [ha]
# tfS   : time flow  structure [min]
# af    : reduction factor [-]
# pe    : population equivalent [PE]
# 
# 2.    Structure data
# Qd    : throttled outflow [l/s]:
# V     : volume [m3]
# te    : emptying time [min]
# 
# 3. Dry weather flow calculation
# Qs24  : dry weather flow [l/s]
# Qf  : infiltration flow [l/s] (Qf24)
# Qt24  : total dry weather flow [l/s]
# 
# Mean dry weather pollutants concentration
# CCOD   : Mean dry weather COD concentration [mg/l]
# CNH4   : Mean dry weather NH4 concentration [mg/l]
#
# table "Berechungen" (calculations)
# combined sewer flow, cs
# dt         : delta time [min]
# V_r_i      : rain part [m3]
# V_dw_i     : dry weather part [m3]
# cs_mr_i    : mixing ratio [-]
# overflow data, o
# o_tfyn_i   : is tank filling up/overflows, yes=1/no=0
# V_Tank     : tank filling [m3]        (o_tf_i)
# V_Ov_i     : overflow volume [m3]     (o_ov_i)
# B_COD_Ov_i : overflow COD load [kg]   (o_COD_i)
# B_NH4_Ov_i : overflow NH4 load [kg]   (o_TKN_i)
# C_COD_Ov_i : overflow CCOD [mg/l]	    (o_CCOD_i)
# C_NH4_Ov_i : overflow CNH4	[mg/l]    (o_CTKN_i)
# d_Ov_i      : overflow duration [min]  (o_d_i)
# f_Ov_i      : overflow frequency [ocurrence] (o_f_i)
#
# table overflow data (summary)
# p           : period [year]
# d_Ov        : overflow duration (CSO duration) [min/year] (o_d)
# f_Ov        : CSO frecuency [ocurrence]                      (o_f)
# V_Ov        : volume [m3/year]                            (o_ov)
# Q_Ov        : flow in [l/s]                               (o_of)
# B_COD_Ov    : COD load [kg/year]                          (o_COD)
# o_COD_av    : average COD concentration [mg/l]
# C_COD_Ov_max: maximum COD concentration [mg/l]            (o_CCOD_max)
# B_NH4_Ov    : NH4 load [kg/year]                          (o_TKN)
# C_NH4_Ov_av : average NH4 concentration [mg/l]            (o_CTKN_av)
# C_NH4_Ov_max: maximum NH4 concentration [mg/l]            (o_CTKN_max)

############### not implemented yet #############################################
# identification of Overflow events and their inflow and effluent volumes, io
# io_fyn: fill/overflow phase, yes=1/no=0
# io_eyn: emptying/overflow phase, yes=1/no=0
# io_iv : inflow volume [m3]
# io_ev : effluent volume [m3]
# Inflow and Outflow volume with  tank filled, but without an overflow, iov
# iov_i : inflow volume [m3]
# iov_o : outflow volume [m3]

#================================================================================
# spatial <- 0 # 1 = spatial input, 0 = non-spatial input
# zero <- 10^(-5) # aproximation to zero value
# mc   <- 0 # 0= no MC runs; 1 = MC runs (internal variable)

#================================================================================
setGeneric("EmiStatR", function(x) standardGeneric("EmiStatR"))

setMethod("EmiStatR", signature = "input", 
          
  function(x){
    # x <- input.user
    spatial <- slot(x, "spatial")
    zero <- slot(x, "zero")
    # mc <- slot(x, "mc")
    folder <- slot(x, "folder")
    folderOutput <- slot(x, "folderOutput")
    cores <- slot(x, "cores")
    ww <- slot(x, "ww")
    inf <- slot(x, "inf")
    rw <- slot(x, "rw")
    tf <- slot(x, "tf")
    P1 <- slot(x, "P1")
    st <- slot(x, "st")
    export <- slot(x, "export")
    #================================================================================
    #setwd(folder)
    #================================================================================
    # rm(list=ls())
  #================================================================================
  mc <- 0
  if (mc == 0){  # no MC runs
    # load variables
    dir.shiny <- folder
    dir.input <- paste(dir.shiny, "/EmiStatR_input/input", sep="")
    setwd(dir.input)
    
    e1 <- new.env()
    
    ifelse(length(ww) == 0, 
           {load("wastewater.RData")
           ww <- get("ww")},
           0)
    
    ifelse(length(inf) == 0,
           {load("infiltration.RData")
            inf <- get("inf")},
           0)
    
    ifelse(length(rw) == 0,
           {load("rainwater.RData")
            rw <- get("rw")},
           0)
    
    ifelse(length(tf) == 0,
           {load("flowTimeDelay.RData")
            tf <- get("tf")},
           0)
    
    # load precipitation data
    ifelse(length(P1) == 0, load("P1.RData"), 0)
    
    # load structure characteristics
#     ifelse(length(st) == 0,
#            {dir.inputCSO <- paste(dir.shiny, "/EmiStatR_inputCSO/inputCSO", sep="")
#            setwd(dir.inputCSO)
#            data.sources = list.files(pattern="*.RData")
#            sapply(data.sources, load, envir = e1)},
#            {for(i in 1:length(st)){
#              assign(names(st)[i], st[[i]])
#            }} # rm("E1", "E2")
#            )

    if (length(st) == 0){
      dir.inputCSO <- paste(dir.shiny, "/EmiStatR_inputCSO/inputCSO", sep="")
      setwd(dir.inputCSO)
      data.sources = list.files(pattern="*.RData")
      sapply(data.sources, load, envir = e1)} else {
      for(i in 1:length(st)){
        assign(names(st)[i], st[[i]])
        }} # rm("E1", "E2", "E3")

   }
  
  # E1 # Goesdorf 
  # E2 # Kaundorf
  # E3 # Nocher-Route
 
  # defining structures to read
  
ifelse(length(st)==0,
  lista <- ls(pattern="^[E]{1}[0-9]", envir = e1),
  lista <- ls(pattern="^[E]{1}[0-9]"))
  
 


  #a<-as.name(paste(lista[1],"$nm", sep=""))
  
  # a<-read.csv("test1.csv", header=TRUE, sep="") 
  # class(a)
  
  ncol <- length(lista)+1
  out <- matrix(data=NA, nrow=35, ncol=ncol)

  #my.array <- array(1:24, dim=c(3,4,3), dimnames=c(lista))
  
  
  out[1:24,1] <- c("Input data for structure", "Name of the structure, ns [-]",
                   "Data of the Catchment", "Municipality, nm [-]",
                   "Name of the catchment, nc [-]", "Catchment number, numc [-]",
                   "Use [-]", "Total area, Ages [ha]", 
                   "Reduced area, Ared [ha]", "Flow time structure, tfS [min]", 
                   "Reduction factor, af [-]:", "Population equivalent, pe [PE]:", 
                   "Mean dry weather flow", "Dry weather flow, Qs24 [l/s]", 
                   "Infiltration flow, Qf [l/s]", "Total dry weather flow, Qt24 [l/s]", 
                   "Mean dry weather pollutants concentration", "COD concentration, COD [mg/l]", 
                   "NH4 concentration, NH4 [mg/l]", "Structure data", 
                   "Throttled outflow, Qd [l/s]", "Volume, V [m3]",
                   "Emptying time, te [min]", "Overflow data summary results")
  
  # defining empty lines in titles of the output table
  out[1,2:ncol]  <- c("")
  out[3,2:ncol]  <- c("")
  out[13,2:ncol] <- c("")
  out[17,2:ncol] <- c("")
  out[20,2:ncol] <- c("")
  out[24,2:ncol] <- c("")
    
  # creating a list of the structures (list of lists)
  li <- list(get(lista[1], envir = e1))
  for(i in 1:length(lista)){
    ifelse(i==length(lista),
           break,
           li <- append(li,list(get(lista[i+1], envir = e1))))
  }
  #li
  
  # extracting individual values from li
  ns   <- 0
  nm   <- 0
  nc   <- 0
  numc <- 0
  use  <- 0
  Ages <- 0
  Ared <- 0
  tfS  <- 0
  pe   <- 0
  Qd   <- 0
  V    <- 0
  for(obj in lista){
    i <- gsub("[^0-9]", "", unlist(obj))
    ns[i]   <- li[sapply(li, function(x) x["id"]==i)][[1]][["ns"]]
    nm[i]   <- li[sapply(li, function(x) x["id"]==i)][[1]][["nm"]]  
    nc[i]   <- li[sapply(li, function(x) x["id"]==i)][[1]][["nc"]]
    numc[i] <- li[sapply(li, function(x) x["id"]==i)][[1]][["numc"]]  
    use[i]  <- li[sapply(li, function(x) x["id"]==i)][[1]][["use"]]  
    Ages[i] <- li[sapply(li, function(x) x["id"]==i)][[1]][["Ages"]]  
    Ared[i] <- li[sapply(li, function(x) x["id"]==i)][[1]][["Ared"]]  
    tfS[i]  <- li[sapply(li, function(x) x["id"]==i)][[1]][["tfS"]]  
    pe[i]   <- li[sapply(li, function(x) x["id"]==i)][[1]][["pe"]]  
    Qd[i]   <- li[sapply(li, function(x) x["id"]==i)][[1]][["Qd"]]  
    V[i]    <- li[sapply(li, function(x) x["id"]==i)][[1]][["V"]]  
  }

  # assigning values to the output table
  out[2,2:ncol]  <- ns[2:ncol]
  out[4,2:ncol]  <- nm[2:ncol]
  out[5,2:ncol]  <- nc[2:ncol]
  out[6,2:ncol]  <- numc[2:ncol]
  out[7,2:ncol]  <- use[2:ncol]
  out[8,2:ncol]  <- Ages[2:ncol]
  out[9,2:ncol]  <- Ared[2:ncol]
  out[10,2:ncol] <- tfS[2:ncol]
  out[12,2:ncol] <- pe[2:ncol]
  out[21,2:ncol] <- Qd[2:ncol]
  out[22,2:ncol] <- V[2:ncol]
  
  # calculating af, Qs24, Qf, COD and NH4
  af   <- 0
  Qs24 <- 0
  Qf   <- 0
  CCOD <- 0
  CNH4 <- 0
  # out[10,2] <- as.numeric(45) # checking the value of af
  # out[10,3] <- as.numeric(30) # checking the value of af
  
  environment(e1)
  for(i in 1:(ncol-1)){
    af[i]   <- ifelse(as.numeric(out[10,i+1]) <= tf, 1, 0.5+50/((as.numeric(out[10,i+1]))+100))  
    Qs24[i] <- round(as.numeric(out[12,i+1])*ww[["qs"]]/(1440*60), digits =3)
    Qf[i]   <- round(as.numeric(out[9,i+1])*inf[["qf"]], digits =3)
    CCOD[i] <- round(as.numeric(out[12,i+1])*ww[["CODs"]]/(ww[["qs"]]*as.numeric(out[12,i+1])+
                                                             as.numeric(out[9,i+1])*inf[["qf"]]*3600*24)*1000,digits = 0)
    CNH4[i] <- round(as.numeric(out[12,i+1])*ww[["NH4s"]]/(ww[["qs"]]*as.numeric(out[12,i+1])+
                                                             as.numeric(out[9,i+1])*inf[["qf"]]*3600*24)*1000,digits = 0)
  }
  
  # assigning values to the output table
  out[11,2:ncol] <- af
  out[14,2:ncol] <- Qs24 # [l/s]
  out[15,2:ncol] <- Qf   # [l/s]
  out[18,2:ncol] <- CCOD
  out[19,2:ncol] <- CNH4
  
  # calculating Qt24
  Qt24 <- 0
  for(i in 1:(ncol-1)){
    Qt24[i] <- round(as.numeric(out[14,i+1])+as.numeric(out[15,i+1]), digits =3)
  }
  
  # assigning values to the output table
  out[16,2:ncol] <- Qt24
  
  # emptying time
  # te <- D30/(Qd*0.06*dt-V_dw)*6
  
  # calculating "Berechungen spreadsheet"
  # defining rain time series for estructure
  #rm(P2,P3,P89)
  currentp <- ls(pattern="^P") #  
  listp <- gsub("E", "P", lista) # reemplacing E by P 
  listp
  if(spatial==1) {0   # <========------- spatial P
  } else {for (obj in listp){
    pp <- get(currentp)  
    assign(obj, pp) 
  }}
  
  # defining precipitations to read
  listp <- ls(pattern="^P")  
  #listp
  
  # calculating combined sewer flow, V_r, V_dw, cs_mr
  
  # defining maximum length of precipitation time series
  #dim <-0
  #for ( obj in listp ){
  #  p <- (get(obj))
  #  dim[obj] <- dim(p)[1]
  #  }
  
  # extracting numbers
  # a <- "E1 E2 E3"
  # gsub("[^0-9]", "", unlist(lista))
  
  # assembling output table
  getwd()
  setwd(folderOutput)
  dir.create("EmiStatR_output", showWarnings=FALSE)
  dir.output <- paste(folderOutput, "/EmiStatR_output", sep="")
  setwd(dir.output)
  
  # parallel computing
  # install.packages("foreach")
  # install.packages("doParallel")
  # library(parallel) # required
  # detach("package:parallel", unload=TRUE)
  
  # library(foreach) # required
  # library(doParallel) # required

  # library(doMC)
  # registerDoMC() <--- evaluate alternative

  ini <- proc.time()   # starting timing of process
  
  # initializing parallel computing
  `%op%` <- ifelse(cores > 0, `%dopar%` , `%do%`)
  
  if(cores == 0){
    numCores <- detectCores() 
    #cl <- makePSOCKcluster(cores)
    #cores <- numCores
    #registerDoParallel(cl, cores=cores)
  }else{
    numCores <- detectCores()
  #  cl <- makeCluster(cores)
    cl <- makePSOCKcluster(cores)
    registerDoParallel(cl, cores=cores)
  }
  
# if(cores == 0){stopCluster(cl)
#                closeAllConnections()} 

  #i <- 1
  inputs <- 1:length(listp)
  # obj <- 1
  # foreach(obj = inputs, .packages=c("doParallel","parallel"), .export=c("listp")) %dopar% {
  output <- foreach(obj = inputs, .packages=c("doParallel","parallel"), .export=c("P1", "e1"), 
                    .errorhandling = "pass", .verbose=FALSE) %op% {
                  
    # obj <- 1
    i <- obj
    p <- data.frame(matrix(vector(), dim(get(currentp))[1], 21,    #<======----- check for spatial P 
                           dimnames=list(c(), c())), stringsAsFactors=F)
    names(p) <- c("id", "Time [y-m-d h:m:s]", "P [mm]", "i[mm/(min)]",
                  "V_r [m3]","V_dw [m3]",
                  "cs_mr [-]","o_tfyn [yes=1/no=0]","V_Tank [m3]","V_Ov [m3]",
                  "B_COD_Ov [kg]","B_NH4_Ov [kg]","C_COD_Ov [mg/l]","C_NH4_Ov [mg/l]",
                  "d_Ov [min]","f_Ov [ocurrence]",
                  "V_InTank [m3]","B_COD_InTank [Kg]","B_NH4_InTank [Kg]",
                  "C_COD_InTank [mg/l]","C_NH4_InTank [mg/l]") #, "io_fyn [yes=1/no=0]",
    # "io_eyn [yes=1/no=0]","io_iv [m3]","io_ev [m3]",
    # "iov_i [m3]","iov_o [m3]")
    
    # head(p,10)
    p[,1] <- c(1:dim(p)[1])
    # p[,2:4]     <- get(listp[obj]) # <=========------for spatial P
    p[,2:4]     <- get(currentp)
    
    #head(p,10)
    #object.size(p)
    
    # checking regularity of TS
    
    dtt   <- difftime(p[2,2], p[1,2], units="min")
    dt    <- as.numeric(dtt) # delta time in minutes
    
    p[,5] <- p[,3]*Ared[i+1]*af[i]*10 # V_r_i
    p[,6] <- Qt24[i]*.06*dt # V_dw_i
    p[,7] <- ifelse(p[,5] <= zero,0,p[,6]/p[,5]) # cs_mr_i
    p[,8] <- ifelse((p[,5]+p[,6]) > (Qd[i+1]*.06*dt),1,0) # o_tfyn_i    
    
    #=====================================================================
    # loop1 in Fortran
    nr <- length(p[,2])
    pp <- as.matrix(p[,5:8])
    nc <- dim(pp)[2]
    a <- matrix(data=0, nrow=nr, ncol=1)
    nl <- length(V)
    
    ## compiling Fortran code
    fileName <- "f_otfi" # may be renamed as "V_Tank_i"
  
    # checking if EmiStatR is already installed 
    pac     <- "EmiStatR"
    new.pac <- pac[!(pac %in% installed.packages()[,"Package"])]
    
    ## checking OS and compiling Fortran code if required
    os <- .Platform
    
    if(os[[1]] == "unix" && length(new.pac) == 0){
      ##file_so <- list.files(pattern = "\\EmiStatR.so$", recursive = TRUE)
      ##dyn.load(paste(dir.lib, "/",file_so, sep=""))
      
      library.dynam("EmiStatR", "EmiStatR", lib.loc=.libPaths()[1])
      #library.dynam("emistatr", "EmiStatR", lib.loc=.libPaths()[1])
    }else if(os[[1]] == "unix" && length(new.pac) != 0){
      ## linux Fortran compilation
      getwd()
      setwd(dir.shiny)
      setwd("..")
      setwd("..")
      dir.src <- paste(getwd(), "/src", sep="")
      setwd(dir.src)
      system(paste("R CMD SHLIB", paste(fileName, ".f", sep=""),  sep=" "))
      
      ## dynamic loading Fortran code
      dyn.load(paste(fileName, ".so", sep=""))
      
    }else if (os[[1]]== "windows" && length(new.pac) == 0){
      getwd()
      setwd(dir.shiny)
      setwd("..")
      setwd("..")
      setwd("..")
      file_dll <- list.files(pattern = "\\EmiStatR.dll$", recursive = TRUE)
      #setwd("C:/Users/torres.LIST/Documents/R/win-library/3.2/EmiStatR/libs/i386")
      #dyn.load(paste("EmiStatR.dll", sep=""))
	
	ifelse(os[[8]] == "x64", ifelse(length(file_dll)>1,dyn.load(file_dll[2]),dyn.load(file_dll[1])),dyn.load(file_dll[1])) 

    }else if(os[[1]]== "windows" && length(new.pac) != 0){
      ## windows Fortran compilation
      getwd()
      setwd(dir.shiny)
      setwd("..")
      setwd("..")
      dir.src <- paste(getwd(), "/src", sep="")
      setwd(dir.src)
      system(paste("gfortran -shared -o f_otfi.dll", paste(fileName, ".f", sep=""),  sep=" ")) 
      
      ## dynamic loading Fortran code
      dyn.load(paste("f_otfi.dll", sep=""))
    }
    
    ## run Fortran code
    ini <- proc.time()   # starting timing of process
    
    if (length(new.pac) == 0){
    aa <-.Fortran(fileName, PACKAGE = "EmiStatR",nr=as.integer(nr), nc=as.integer(nc), nl=as.integer(nl), 
                  i=as.integer(i), dV=as.double(0), dt=as.double(dt), 
                  Qd=as.double(Qd), a=as.double(a), p=as.double(pp), V=as.double(V),
                  zero = as.double(zero))} else{
                    aa <-.Fortran(fileName, PACKAGE = "EmiStatR",nr=as.integer(nr), nc=as.integer(nc), nl=as.integer(nl), 
                                  i=as.integer(i), dV=as.double(0), dt=as.double(dt), 
                                  Qd=as.double(Qd), a=as.double(a), p=as.double(pp), V=as.double(V),
                                  zero = as.double(zero))
                  }
    end <- proc.time() # finish timing of process
    elapsed <- end-ini; elapsed #0.006!!
    
    a <- aa$a
    
    #max(dV) # 318.2294
    #sum(dV) # -60132.74
    #mean(dV) # -1.144078
    #sd(dV) # 4.516562
    #rm(dV)
    ###### original code ##########
    #ini <- proc.time()   # starting timing of process
    ##j <- 1
    #for(j in 2:length(a)){
    #  dV <- p[j,5]+p[j,6]-0.06*dt*Qd[i+1]
    #       ifelse(p[j,8]==1, ifelse(a[j-1]<V[i+1], a[j]<-(a[j-1]+dV), a[j]<-V[i+1]), 
    #                ifelse((a[j-1]+dV) <= zero, a[j]<-0, a[j]<-(a[j-1]+dV)))
    #}
    #end <- proc.time()   # finish timing of process
    #elapsed <- end-ini; elapsed # 7.651
    ###############################
    
    # for(j in 2:length(a)){
    #   dV <- p[j,5]+p[j,6]-0.06*dt*Qd[i+1]
    #   a[j] <- min(max(V,a[j-1]+dV),0)
    # }
    #max(a) # 412.9176
    #sum(a) # 1669242
    #mean(a) # 31.75879
    #sd(a) # 70.11083
    #rm(a)
    #=====================================================================
    p[,9]  <- a # o_tf_i => V_Tank
    
    dV <- p[,5]+p[,6]-Qd[i+1]*0.06*dt
    p[,10] <- ifelse(p[,9] == V[i+1], dV, ifelse(p[,9] > V[i+1], p[,9]-V[i+1],0)) # V_Ov_i (o_ov_i)
    #p[,10] <- max(0,p[,9] + dV - V[i+1]) # V_Ov_i (o_ov_i)
    #max(p[,10]) # 143.2294
    #sum(p[,10]) # 8104.395
    #mean(p[,10]) # 0.1541932
    #sd(p[,10]) # 2.491797
    
    #rw[["CODr"]] <- 0 # check this value
    
    # B_COD_Ov_i (o_COD_i)
    p[,11] <- ifelse(p[,10]>zero, p[,10]*p[,7]/(p[,7]+1)*CCOD[i]/1000+p[,10]/(p[,7]+1)*rw[["CODr"]]/1000,0) 
    
    # B_COD_Ov_i (o_COD_i)
    #p[,11] <- max(zero, p[,10]*p[,7]/(p[,7]+1)*COD[i]/1000+p[,10]/(p[,7]+1)*rw[["CODr"]]/1000) 
    
    #max(p[,11]) # 15.6363
    #sum(p[,11]) # 997.8134
    #mean(p[,11]) # 0.01898427
    #sd(p[,11]) # 0.282776
    
    #B_NH4_Ov_i (o_NH4_i)
    p[,12] <- ifelse(p[,10]>zero, p[,10]*p[,7]/(p[,7]+1)*CNH4[i]/1000+p[,10]/(p[,7]+1)*rw[["NH4r"]]/1000,0) 
    
    p[,13] <- ifelse(p[,10]<=zero, 0, p[,11]*1000/p[,10]) # C_COD_Ov_i (o_CCOD_i)
    p[,14] <- ifelse(p[,10]<=zero, 0, p[,12]*1000/p[,10]) # C_NH4_Ov_i (o_CNH4_i)
    p[,15] <- ifelse(p[,10]<=zero, 0, 1) # d_Ov_i (o_d_i)
    
    j <- 1
    a <- matrix(data=0, nrow=length(p[,9]), ncol=1)
    # a <- matrix(data=1:9, nrow=9, ncol=1)
    # min(a[3-27,1]:a[9,1])
    for(j in 1:length(a)){
      if(j == 1)  {a[1] <- 0}
      if(j <  29 && p[j,15] == 1 && p[j-1,15] == 0 && (min(p[1,9]   :p[j,9])) <= 
           ((Qd[i+1] - Qt24[i])*0.06*dt)){a[j] <- 1} 
      if(j >= 29 && p[j,15] == 1 && p[j-1,15] == 0 && (min(p[j-28,9]:p[j,9])) <= 
           ((Qd[i+1] - Qt24[i])*0.06*dt)){a[j] <- 1} 
    }
    p[,16]  <- a # f_Ov_i (o_f_i) [ocurrence]
    
    # new implementation InTank volume
    # dV <- p[,5]+p[,6]-Qd[i+1]*0.06*dt
    p[,17] <- ifelse(p[,9] == V[i+1], Qd[i+1]*0.06*dt, 
                     ifelse(p[,9] > V[i+1], max(p[,5]+p[,6]-(p[,9]-V[i+1]),0), 
                            p[,5]+p[,6])) # V_InTank_i [m3]
    
    # new implementation InTank loads and concentrations
    Vsew      <- as.numeric(out[14,i+1])*0.06*dt # [m3] 
    Vinf      <- as.numeric(out[15,i+1])*0.06*dt # [m3]
    B_COD_sew <- (ww[["CODs"]]*1000/ww[["qs"]])*1000/1000/1000*Vsew # [kg]
    B_COD_inf <- ((inf[["CODf"]]*as.numeric(out[12,i+1])/86400/1000)/(inf[["qf"]]*
                   as.numeric(out[9,i+1])/1000))*Vinf # [kg]
    C_COD_DWF <- (B_COD_sew+B_COD_inf)/(Vsew+Vinf)/1000*1000*1000 # [mg/l]
    B_COD_DWF <- C_COD_DWF*(Vsew+Vinf)*1000/1000/1000 # [kg]
    
    B_NH4_sew <- (ww[["NH4s"]]*1000/ww[["qs"]])*1000/1000/1000*Vsew # [kg]
    B_NH4_inf <- ((inf[["NH4f"]]*as.numeric(out[12,i+1])/86400/1000)/(inf[["qf"]]*
                   as.numeric(out[9,i+1])/1000))*Vinf # [kg]
    C_NH4_DWF <- (B_NH4_sew+B_NH4_inf)/(Vsew+Vinf)/1000*1000*1000 # [mg/l]
    B_NH4_DWF <- C_NH4_DWF*(Vsew+Vinf)*1000/1000/1000 # [kg]
    
    p[,18]    <- ifelse(p[,7] > zero, p[,17]*p[,7]/(p[,7]+1)*CCOD[i]/1000+p[,17]/(p[,7]+1)*
                       rw[["CODr"]]/1000, B_COD_DWF) # B_COD_InTank
    p[,19]    <- ifelse(p[,7] > zero, p[,17]*p[,7]/(p[,7]+1)*CNH4[i]/1000+p[,17]/(p[,7]+1)*
                       rw[["NH4r"]]/1000, B_NH4_DWF) # B_NH4_InTank
    
    bb        <- p[,18]*1000/p[,17] # C_COD_InTank
    bb[bb<0]  <- 0
    p[,20]    <- bb # C_COD_InTank
    
    bb        <- p[,19]*1000/p[,17] # C_NH4_InTank
    bb[bb<0]  <- 0
    p[,21]    <- bb
    
   
    #b=9
    #c <- ifelse(b>=0 & b==9,ifelse(b>=5, print("mayor que 5"), print("entre 0 y 5")),print("menor que 0"))  
    #d <- ifelse(b>=0 & b==9, print("mayor que 0 y es 9"), print("diferente que 9"))  
    
    ## overflow data summary results
    q <- data.frame(matrix(vector(), 11, 2, 
                           dimnames=list(c(), c())), stringsAsFactors=F)
    q[1:11,1] <- c("Period [day]", "Duration, d_Ov, [min]", "Frecuency, f_Ov, [ocurrence] (aprox.)",
                   "Volume, V_Ov, [m3]", "Flow, Q_Ov, [l/s]", "COD load, B_COD_Ov, [kg]",
                   "Average COD concentration, C_COD_ov_av, [mg/l]", 
                   "Maximum COD concentration, C_COD_Ov_max, [mg/l]",
                   "NH4 load, B_NH4_Ov, [kg]", "Average NH4 concentration,C_NH4_Ov_av, [mg/l]",
                   "Maximum NH4 concentration, C_NH4_Ov_max, [mg/l]")
    
    peri   <- difftime(p[dim(p)[1],2], p[2,2], units="days")
    q[1,2] <- peri  #round(peri/365, digits=3)              # period in [day],                  p
    q[2,2] <- sum(p[,15])*dt                                # duration in [min],                d_Ov         (o_d)
    q[3,2] <- round(sum(p[,16]),digits = 15)                # frecuency in [ocurrence],         f_Ov         (o_f)
    q[4,2] <- round(sum(p[,10]), digits=5)                  # volume [m3],                      V_Ov         (o_ov)
    q[5,2] <- round(sum(p[,10])/(q[2,2]*60)*1000, digits=5) # flow in [l/s],                    Q_Ov         (o_of)
    q[6,2] <- round(sum(p[,11]), digits=5)                  # COD load [kg],                    B_COD_Ov     (o_COD)
    q[7,2] <- round(q[6,2]/q[4,2]*1000,digits=5)            # average COD concentration [mg/l]  C_COD_Ov_av  (o_CCOD_av)
    q[8,2] <- round(max(p[,13]), digits=5)                  # Maximum COD concentration [mg/l]  C_COD_Ov_max (o_CCOD_max) 
    q[9,2] <- round(sum(p[,12]), digits=5)                  # NH4 load [kg]                     B_NH4_Ov     (o_NH4)
    q[10,2]<- round(q[9,2]/q[4,2]*1000,digits=5)            # average NH4 concentration [mg/l]  C_NH4_Ov_av  (o_CNH4_av)
    q[11,2]<- round(max(p[,14]), digits=5)                  # Maximum NH4 concentration [mg/l]  C_NH4_Ov_max (o_CNH4_max)       
    
    # creating variable to be saved
    assign(paste("out1_",lista[i], sep=""), p)
    assign(paste("out2_",lista[i], sep=""), q)
    
   
  # writing output                                            <-------------------
  if(export == 1){
    setwd(paste(folderOutput,"/EmiStatR_output", sep=""))
    
    write.table(get(paste("out1_",lista[i], sep="")), file = paste("out1_",lista[i],".csv",sep=""), 
                sep = ",", qmethod = "double", row.names=FALSE)
    write.table(get(paste("out2_",lista[i], sep="")), file = paste("out2_",lista[i],".csv",sep=""), 
                sep = ",", qmethod = "double", row.names=FALSE)
    }
  
  # returning variables
  return(list(out1=get(paste("out1_",lista[i], sep="")), out2=get(paste("out2_",lista[i], sep="")),lista=lista))
  
  # updating counter
  i <- i+1
  
  } ## end foreach
  
  if(cores > 0){
    stopCluster(cl)
    closeAllConnections()}


  end <- proc.time()
  elapsed <- end-ini; elapsed # c(19.419, 21.317) <- c(11.438,11.389)
  
#   # uptating out table with overflow data summary results  
#   setwd(paste(folderOutput,"/EmiStatR_output", sep=""))
  
#   #sources = list.files(pattern="^out2")
#   sources = ls(pattern="^out2")

# i = 2
  # for(i in 1:length(sources)){
  environment(e1)


  for(i in 1:length(output)){
    ## tmpf <- read.csv(file=sources[i],header=TRUE,sep=",")
    tmpf <- output[[i]][[2]]
    #write.csv(tmpf, file=paste("test",i,".csv", sep=""))
    out[25:35,1] <- as.character(tmpf[1:11,1])
    out[25:35,i+1] <- tmpf[,2]  
  }

  # writing output
  if(export == 1){
  write.table(out, file = paste("out.csv",sep=""), 
              sep = ",", qmethod = "double", row.names=FALSE)
  }

  ## plotting output 1
  # sources = list.files(pattern="^out1_E")
  i <- 1
  # for(i in 1:length(sources)){
  for(i in 1:length(output)){
    #tmpf <- read.csv(file=sources[i],header=TRUE,sep=",")
    tmpf <- output[[i]][[1]]
    #     warnings()
    #     head(tmpf, 10)
    #     head(tmpf[,2], 10)
    #     plot(tmpf[1:10,2], tmpf[1:10,3])
    #    
    #     dateini <- as.POSIXct(tmpf[1,2])
    #     dateend <- as.POSIXct(tmpf[dim(tmpf)[1],2])
    #     tt <- as.POSIXct(tmpf[,2])
    # 
    #     old.par <- par(mfrow=c(3, 1))
    #     
    #     plot(tt, tmpf[,3], type ="l", xaxt="n", xlab = "", ylab="Precipitation [mm]")
    #     axis.POSIXct(1, at=seq(dateini, dateend, by="month"), 
    #                  format="%b-%Y") #label the x axis by months
    #     
    #     plot(tt, tmpf[,10], type ="l", xaxt="n", ylab="Overflow volume [m3]")
    #     axis.POSIXct(1, at=seq(dateini, dateend, by="month"), 
    #                  format="%b-%Y") #label the x axis by months
    #     
    #     plot(tt, tmpf[,12], type ="l", xaxt="n", ylab="Overflow COD load [kg]")
    #     axis.POSIXct(1, at=seq(dateini, dateend, by="month"), 
    #                  format="%b-%Y") #label the x axis by months
    #     
    #     par(old.par)
    
    # arraging variables for plotting
    datP <- as.data.frame(tmpf[,1])
    datP[,2] <- "Precipitation, P [mm]" # variable label
    datP[,3] <- tmpf[,2] # time
    datP[,4] <- tmpf[,3] # precipitation, P
    colnames(datP) <- c("id", "variable", "time", "value")
    
    datOV <- as.data.frame(tmpf[,1])
    datOV[,2] <- "Volume, V_Ov [m3]" # variable label
    datOV[,3] <- tmpf[,2] # time
    datOV[,4] <- tmpf[,10] # overflow volume
    colnames(datOV) <- c("id", "variable", "time", "value")
    
    datOCOD <- as.data.frame(tmpf[,1])
    datOCOD[,2] <- "COD load, B_COD_Ov [kg]" # variable label
    datOCOD[,3] <- tmpf[,2] # time
    datOCOD[,4] <- tmpf[,11] # COD load
    colnames(datOCOD) <- c("id", "variable", "time", "value")
    
    datONH4 <- as.data.frame(tmpf[,1])
    datONH4[,2] <- "NH4 load, B_NH4_Ov [kg]" # variable label
    datONH4[,3] <- tmpf[,2] # time
    datONH4[,4] <- tmpf[,12] # NH4 load
    colnames(datONH4) <- c("id", "variable", "time", "value")
    
    datOCCOD <- as.data.frame(tmpf[,1])
    datOCCOD[,2] <- "COD concentration, C_COD_Ov [mg/l]" # variable label
    datOCCOD[,3] <- tmpf[,2] # time
    datOCCOD[,4] <- tmpf[,13] # COD concentration
    colnames(datOCCOD) <- c("id", "variable", "time", "value")
    
    datOCNH4 <- as.data.frame(tmpf[,1])
    datOCNH4[,2] <- "NH4 concentration, C_NH4_Ov [mg/l]" # variable label
    datOCNH4[,3] <- tmpf[,2] # time
    datOCNH4[,4] <- tmpf[,14] # NH4 concentration
    colnames(datOCNH4) <- c("id", "variable", "time", "value")
    
    dat     <- rbind(datP, datOV, datOCOD, datONH4, datOCCOD, datOCNH4)
    
    # creating plots by lattice
    # library(lattice) # required
    if(export == 1){
#     fig <- xyplot(dat[,4]~as.POSIXct(dat[,3])|variable, data = dat,
#                   main="Overflow data",
#                   xlab="Time", ylab="", layout =c(2,3), index.cond=list(c(1,3,2,4,5,6)),
#                   type="l", scales=list(y="free", x=list(at= seq(as.POSIXct(dat[1,3]), by="3 day", length=5), 
#                                   labels=format(seq(as.POSIXct(dat[1,3]), by="3 day", length=5),"%D%M%Y"))))
    fig <- xyplot(dat[,4]~as.POSIXct(dat[,3])|variable, data = dat,
                  main="Overflow data",
                  xlab="Time", ylab="", layout =c(2,3), index.cond=list(c(1,3,2,4,5,6)),
                  type="l", scales=list(y="free"))
    
    # saving the plot
    # name <- sub("^([^.]*).*", "\\1", sources[i]) # extract filename without the extension 
    pdf(paste("plot_out1_", output[[1]][[3]][i], ".pdf", sep=""))
    print(fig)
    dev.off()
    }
  }
  
  
  numCores <- detectCores() 
#   if(cores == 0){
#     cores <- numCores - 1
#   }

  if(export == 1 & cores != 0){
    print(paste("done, please check your output folder (used", cores, "of", numCores, "cores)", sep=" "))
  }
  
  if(export == 1 & cores == 0){
    print(paste("done, please check your output folder (used 1 of", numCores, "cores, no parallel mode, cores=0)", sep=" "))
  }

  if(export == 0 & cores != 0){
    print(paste("done (used", cores, "of", numCores, "cores)", sep=" "))
  }

  if(export == 0 & cores == 0){
    print(paste("done (used 1 of", numCores, "cores, no parallel mode, cores=0)", sep=" "))
  }

  return(output)
  }
)