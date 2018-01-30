# creation of input ui.R file for UI of the EmiStatR model, simulation
# author: Arturo Torres
# organization: LIST
# date: 13.09.2017 - 13.09.2017

library(shiny)
library(shinyFiles) # shinyDirButton()

# Define UI for application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("EmiStatR: simulation"),
  #-----------------------------------------------------------------------------------------
  # options
  sidebarLayout(
    sidebarPanel(
      # selection of dataset
      checkboxGroupInput("checkboxGroup.data", h5("Choose catchment to simulate"), 
                         choices = c(
                           'Goesdorf' = 'Goesdorf', 
                           'Kaundorf' = 'Kaundorf',
                           'Nocher-Route' ='Nocher-Route' ,
                           'User defined' = 'user'),
                         selected = 'goe')
    ),
    
    
    # Show a table summarizing the values entered
    mainPanel(
      h3(""),
      # textOutput('nd'),
      # textOutput('render.checkboxGroup.data')
      
      # make a select box 
      selectInput("selectbox.catchment", label = h3("Show data"), 
                  choices = list("None" = "None"), 
                  selected = "none")
      
    )
  ),
  #-----------------------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      h5("1. General input data"),
      
      # render checkbox ww
      checkboxInput("checkbox.ww", label = "Wastewater", value = FALSE),
      
      # render checkbox inf
      checkboxInput("checkbox.inf", label = "Infiltration water", value = FALSE),
      
      # render checkbox rw
      checkboxInput("checkbox.rw", label = "Rainwater", value = FALSE),
      
      # make a select box 
      selectInput("selectbox.rain", label = h5("Rainfall"), 
                  choices = list("None" = "None", "P1" = "P1", 
                                 "Esch-sur-Sure 2010" = "Esch_Sure2010",
                                 "User defined" = "user"), 
                  selected = "none"),
      
      htmlOutput("render.checkbox.rain.plot")
      
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h2("1. General input data"),
      h3("1.1. Wastewater"),
      tableOutput("render.table.ww"),
      h3("1.2. Infiltration water"),
      tableOutput("render.table.inf"),
      h3("1.3. Rainwater"),
      tableOutput("render.table.rw"),
      plotOutput("plot.rain")
    )
  ),
  #-----------------------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      h5("2. Specific CSO data"),
      
      # render checkbox plot
      checkboxInput("checkbox.CSO", label = "CSO input", value = FALSE)
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h2("2. Specific CSO data"),
      tableOutput("render.table.ns"),
      h3("2.1. Catchment"),
      tableOutput("render.table.catchment"),
      h3("2.2. Structure"),
      tableOutput("render.table.structure")
    )
  ),
  #-----------------------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      h5("3. Water consumptium, qs, factors"),
      
      # select box 
      selectInput("selectbox.qs.daily", label = h5("Daily"), 
                  choices = list("None" = "none", "File" = "file"), 
                  selected = "none"),
      
      htmlOutput("render.infile.qs.daily"),
      
      checkboxInput("checkbox.qs.daily.plot", label = "Plot", value = FALSE),
      
      # select box 
      selectInput("selectbox.qs.weekly", label = h5("Weekly"), 
                  choices = list("None" = "none", "File" = "file"), 
                  selected = "none"),
      
      htmlOutput("render.infile.qs.weekly"),
      
      checkboxInput("checkbox.qs.weekly.plot", label = "Plot", value = FALSE),
      
      # select box 
      selectInput("selectbox.qs.seasonal", label = h5("Seasonal"), 
                  choices = list("None" = "none", "File" = "file"), 
                  selected = "none"),
      
      htmlOutput("render.infile.qs.seasonal"),
      
      checkboxInput("checkbox.qs.seasonal.plot", label = "Plot", value = FALSE)
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h2(""),
      plotOutput("plot.qs")
    )
  ),
  #-----------------------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      h5("Population equivalent, pe, factors"),
      
      # make a select box 
      selectInput("selectbox.pe.daily", label = h5("Daily"), 
                  choices = list("None" = "none", "File" = "file"), 
                  selected = "none"),
      
      
      htmlOutput("render.infile.pe.daily"),
      
      checkboxInput("checkbox.pe.daily.plot", label = "Plot", value = FALSE),
      
      # make a select box 
      selectInput("selectbox.pe.weekly", label = h5("Weekly"), 
                  choices = list("None" = "none", "File" = "file"), 
                  selected = "none"),
      
      htmlOutput("render.infile.pe.weekly"),
      
      checkboxInput("checkbox.pe.weekly.plot", label = "Plot", value = FALSE),
      
      # make a select box 
      selectInput("selectbox.pe.seasonal", label = h5("Seasonal"), 
                  choices = list("None" = "none", "File" = "file"), 
                  selected = "none"),
      
      
      htmlOutput("render.infile.pe.seasonal"),
      
      checkboxInput("checkbox.pe.seasonal.plot", label = "Plot", value = FALSE)
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h2(""),
      plotOutput("plot.pe")
      
    )
  ),
  #-----------------------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      h3("Run"),
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

