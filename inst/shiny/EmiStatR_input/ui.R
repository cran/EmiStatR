# creation of input ui.R file for UI of the EmiStatR model
# author: Arturo Torres, Kai Klepiszewski
# organization: LIST
# date1: 06.02.2015
# date2: 05.09.2017 - 07.09.2017

library(shiny)
library(shinyFiles) # shinyDirButton()

# Define UI for application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("General input data"),
  
  # Sidebar with sliders 
  # options
  sidebarLayout(
    sidebarPanel(
      h3("1. Wastewater"),
      # Simple integer interval
      sliderInput("qs", "Water consumption, qs [l/(PE d)]:", 
                  min=0, max=500, value=155),
      
      # Simple interval interval
      sliderInput("CODs", "Pollution COD [g/(PE d)]:", 
                  min = 0, max = 500, value = 104),
      
      # Simple interval interval
      sliderInput("NH4s", "Pollution NH4 [g/(PE d)]:", 
                  min = 0, max = 50, value = 3.5, step = 0.1),
      
      h3("2. Infiltration water"),
      # Decimal interval with step value
      sliderInput("qf", "Inflow, qf [l/(s ha)]:", 
                  min=0, max=.15, value=0.035, step= 0.001),
      
      # Simple interval interval
      sliderInput("CODf", "Pollution COD [g/(PE d)]:", 
                  min = 0, max = 500, value = 0),
      
      # Simple interval interval
      sliderInput("NH4f", "Pollution NH4 [g/(PE d)]:", 
                  min = 0, max = 50, value = 0, step = 0.1),
      
      
      h3("3. Rainwater"),
      # Simple interval interval
      sliderInput("CODr", "Pollution COD [mg/l]:", 
                  min = 0, max = 500, value = 71),
      
      # Simple interval interval
      sliderInput("NH4r", "Pollution NH4 [mg/l]:", 
                  min = 0, max = 50, value = 0, step = 0.1),
      
      # selection of dataset
      radioButtons("radio.data", h5("Choose rain time series from available..."),
                   choices = list(None="", 
                                  'P1' = "1", 
                                  'Esch-sur-Sure 2010'= "2",
                                  'User defined' = "3"), 
                   selected = ""),
      
      
      htmlOutput("fileInput"),
      
      
      # tags$hr(),
      
      
      htmlOutput("textInput"),

      plotOutput("plot1", width = 400, height = 300),     
      
      shinyDirButton("directory", "Chose directory", "Directory to save data"),
      
      # make an action button
      htmlOutput("actionButton.save"),
      
      # make a new action button
      actionButton("close", label = "Close")
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      h3("1. Wastewater"),
      tableOutput("valores1"),
      # change style:    
      tags$head(tags$style("#valores1 table {background-color: gray30; }", media="screen", type="text/css")),
      h3("2. Infiltration water"),
      tableOutput("valores2"),
      h3("3. Rainwater"),
      tableOutput("valores3"),
      #tableOutput('contents'),
      tableOutput('valores4'), 
      # renderPlot('plot1'),
      h3("4. Directory"),
      textOutput('valores5')
    )
  )
))

