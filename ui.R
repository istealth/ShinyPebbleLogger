library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Pebble-Plots (for CompassLog and Acc-log)"),

      fileInput("file", "Select file with logged data:", multiple = FALSE, 
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      includeHTML("include.html"),
      tabsetPanel(type = "tabs", 
                  
                  tabPanel("Acceleration Data", plotOutput("distPlot", width="95%"), 
                           uiOutput("ui_All")),
                  tabPanel("X-axis", plotOutput("plotX", width="95%"),
                           uiOutput("ui_X")),
                  tabPanel("Y-axis", plotOutput("plotY", width="95%"),
                           uiOutput("ui_Y")),
                  tabPanel("Z-axis", plotOutput("plotZ", width="95%"),
                           uiOutput("ui_Z")),
                  tabPanel("Modulus", plotOutput("plotMod", width="95%"),
                           uiOutput("ui_Mod"))
      ),
      includeHTML("version.html")
    )
  )

