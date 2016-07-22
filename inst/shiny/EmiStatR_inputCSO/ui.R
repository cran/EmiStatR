# creation of input ui.R file for UI of the EmiStat model, CSO structures
# author: Arturo Torres, Kai Klepiszewski
# organization: LIST
# date: 09.02.2015

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("General data CSO structure"),
  
  # options
  sidebarLayout(
    sidebarPanel(
      # numeric input
      h3("1. Identification"),
      numericInput("id", label = h5("ID of the structure:"), 
                value = ""),
      
      textInput("ns", label = h5("Name of the structure:"), 
                value = "Enter name ..."),
      
      h3("2. Catchment data"),
      # text input
      textInput("nm", label = h5("Name of the municipality:"), 
                value = "Enter name ..."),

      # text input
      textInput("nc", label = h5("Name of the catchment:"), 
                value = "Enter name ..."),
      
      # numeric input
      numericInput("numc", label = h5("Number of the catchment:"), 
                value = ""),
      
      # text input
      textInput("use", label = h5("Land use:"), 
                value = "Enter use ..."),
         
      # numeric input
      numericInput("Ages", label = h5(tags$div(
        HTML(paste("Total area, A", tags$sub("ges"), " [ha]:" ,sep = "")))), 
                   value = ""),
      
      # numeric input
      numericInput("Ared", label = h5(tags$div(
        HTML(paste("Reduced area, A", tags$sub("red"), " [ha]:" ,sep = "")))), 
        value = ""),
      
      # numeric input
      numericInput("tfS", label = h5(tags$div(
        HTML(paste("Flow time structure, t", tags$sub("fs"), " [min]:" ,sep = "")))), 
        value = 0),
      
      # numeric input
      numericInput("pe", label = h5("Population equivalent [PE]:"), 
                   value = ""),
      
      h3("3. Structure data"),
      # numeric input
      numericInput("Qd", label = h5(tags$div(
        HTML(paste("Throttled outflow, Q", tags$sub("d"), " [l/s]:" ,sep = "")))), 
        value = ""),
      
      # numeric input
      numericInput("V", label = h5(tags$div(
        HTML(paste("Volume, V", " [m", tags$sup("3"), "]:" ,sep = "")))), 
        value = ""),
      
      # make an action button
      actionButton("save", label = "Save & close")
      
      #submitButton("Save")
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      tableOutput("v1"),
      h3("1. Catchment data"),
      tableOutput("v2"),
      h3("2. Structure data"),
      tableOutput("v3")
    )
    
    
    
  )
))

