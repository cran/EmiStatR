# creation of input ui.R file for UI of the EmiStatR model, CSO structures
# author: Arturo Torres, Kai Klepiszewski
# organization: LIST
# date1: 09.02.2015
# date2: 07.09.2017 - 12.09.2017

library(shiny)
library(shinyFiles) # shinyDirButton()

# Define UI for application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("General CSO structure data"),
  #-----------------------------------------------------------------------------------------
  # options
  sidebarLayout(
    sidebarPanel(
      # selection of dataset
      radioButtons("radio.CSO", h5("Load CSO data from available..."),
                   choices = list('None' = "none",
                                  'Goesdorf' = "E1", 
                                  'Kaundorf'= "E2",
                                  'Nocher-Route' = "E3"), 
                   selected = "none")
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h3("")
    )
  ),
  #-----------------------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      h5("Create new CSO structure"),
      
      # make a new action button
      actionButton("new", label = "New")
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h3("")
    )
  ),
  #-----------------------------------------------------------------------------------------
  # options
  sidebarLayout(
    sidebarPanel(
      # numeric input
      h3("1. Identification"),
      numericInput("id", label = h5("ID of the structure:"), 
                value = "1"),
      
      textInput("ns", label = h5("Name of the structure:"), 
                value = "Goesdorf"),
      
      h3("2. Catchment data"),
      # text input
      textInput("nm", label = h5("Name of the municipality:"), 
                value = "Goesdorf"),

      # text input
      textInput("nc", label = h5("Name of the catchment:"), 
                value = "Obersauer"),
      
      # numeric input
      numericInput("numc", label = h5("Number of the catchment:"), 
                value = 1),
      
      # text input
      textInput("use", label = h5("Land use:"), 
                value = "Residential/Industrial"),
         
      # numeric input
      numericInput("Atotal", label = h5(tags$div(
        HTML(paste("Total area, A", tags$sub("total"), " [ha]:" ,sep = "")))), 
                   value = 16.5),
      
      # numeric input
      numericInput("Aimp", label = h5(tags$div(
        HTML(paste0("Impervious area, A", tags$sub("imp"), " [ha]:")))), 
        value = 4.5),
      
      # numeric input
      numericInput("tfS", label = h5(tags$div(
        HTML(paste("Flow time structure, t", tags$sub("fS"), " [time steps]:" ,sep = "")))), 
        value = 2),
      
      # numeric input
      numericInput("pe", label = h5("Population equivalent [PE]:"), 
                   value = 600),
      
      h3("3. Structure data"),
      # numeric input
      numericInput("V", label = h5(tags$div(
        HTML(paste("Volume, V", " [m", tags$sup("3"), "]:" ,sep = "")))), 
        value = 190),
      
      # selection of file 
      h5("Level [m] - volume [m3]:"),
      htmlOutput("render.file.lev2vol"),
      
      # render checkbox plot
      checkboxInput("checkbox.plot", label = "Plot", value = FALSE),
      
      h3(""),
      
      # numeric input
      numericInput("lev.ini", label = h5(tags$div(
        HTML(paste("Initial water level, Lev", tags$sub("ini"), " [m]:" ,sep = "")))), 
        value = 1.8),
      
      # numeric input
      numericInput("Qd", label = h5(tags$div(
        HTML(paste("Maximum throttled outflow, Q", tags$sub("d_max"), " [l/s]:" ,sep = "")))), 
        value = 5),
      
      # numeric input
      numericInput("Ad", label = h5(tags$div(
        HTML(paste("Orifice area, Ad", " [m", tags$sup("2"), "]:" ,sep = "")))),
        value = 0.0180),

      # numeric input
      numericInput("Cd", label = h5(tags$div(
        HTML(paste("Orifice discharge coefficient, Cd", " [-]:" ,sep = "")))),
        value = 0.17)
    ),
      # Show a table summarizing the values entered
      mainPanel(
        tableOutput("v1"),
        h3("1. Catchment data"),
        tableOutput("v2"),
        h3("2. Structure data"),
        tableOutput("v3"),
        h3("3. Directory"),
        textOutput('valorPath')
      )
    ),
  #-----------------------------------------------------------------------------------------
    sidebarLayout(
      sidebarPanel(
        h3("Save"),
        # choose directory
        shinyDirButton("directory", "Choose directory...", "Directory to save data"),
        
        # render save action button
        htmlOutput("actionButton.save")
      ),
      
      # Show a table summarizing the values entered
      mainPanel(
        h3("")
      )
    ),
  #-----------------------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # make a new action button
      actionButton("close", label = "Close")
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h3("")
    )
  )
  #-----------------------------------------------------------------------------------------
))

