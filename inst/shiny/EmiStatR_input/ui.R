# creation of input ui.R file for UI of the EmiStat model
# author: Arturo Torres, Kai Klepiszewski
# organization: LIST
# date: 06.02.2015

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("General data"),
  
  # Sidebar with sliders 
  # options
  sidebarLayout(
    sidebarPanel(
      h3("1. Wastewater"),
      # Simple integer interval
      sliderInput("qs", "Water consumption, qs [l/(PE d)]:", 
                  min=0, max=500, value=150),
      
      # Simple interval interval
      sliderInput("CODs", "Pollution COD [g/(PE d)]:", 
                  min = 0, max = 500, value = 120),

      # Simple interval interval
      sliderInput("NH4s", "Pollution NH4 [g/(PE d)]:", 
                  min = 0, max = 500, value = 11),
      
      h3("2. Infiltration water"),
      # Decimal interval with step value
      sliderInput("qf", "Inflow, qf [l/(s ha)]:", 
                  min=0, max=.15, value=0.05, step= 0.01),
      
      # Simple interval interval
      sliderInput("CODf", "Pollution COD [g/(PE d)]:", 
                  min = 0, max = 500, value = 0),
      
      # Simple interval interval
      sliderInput("NH4f", "Pollution NH4 [g/(PE d)]:", 
                  min = 0, max = 500, value = 0),
      
      
      h3("3. Rainwater"),
      # Simple interval interval
      sliderInput("CODr", "Pollution COD [mg/l]:", 
                  min = 0, max = 500, value = 107),
      
      # Simple interval interval
      sliderInput("NH4r", "Pollution NH4 [mg/l]:", 
                  min = 0, max = 500, value = 0),
      
      #text input
      textInput("stat", label = h5("Rain measurement station:"), 
                value = "Enter name station..."),
      
#       # Specification of range within an interval
#       sliderInput("peri", "Period:",
#                   min = 1900, max = 2100, value = c(2010,2010)),
      
      
      fileInput('file1', 'Choose rain time series as CSV File...',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      
      plotOutput("plot1", width = 400, height = 300),     
  
      h3("4. Storm water runoff"),
      # Simple interval interval
      sliderInput("tf", "Flow time in the sewer system [min]:", 
                  min = 0, max = 500, value = 20),
      
      # make an action button
       actionButton("save", label = "Save")
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
      #renderPlot('plot1'),
      h3("4. Stormwater runoff"),
      tableOutput('valores5')
      )
  )
))

