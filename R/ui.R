###############################################################################
###############################################################################
##
##  This script contains the UI code for the the SpawnEst app
##
##  Coded by Joseph (Josie, they) L. Simonis, DAPPER Stats
##
##  version 0.0.0.1 2017
##
##  Held under GNU GPL v >= 3	
##
###############################################################################
###############################################################################

##############################
#
#  Table of Contents
#
#  1. load packages
#  2. ui code
#

#1. load packages

  library(shiny)
  library(shinysky)
  library(rhandsontable)
  source("spawnestfunctions.R")

#2. ui code

# navbar page sets up the general format with the navigation bar and tabs, each
# of which is defined by a tabPanel function
#
#  Home: general splash page with image
#  Spawner Estimation: page for data entry and estimations 
#    subdivided by an additional layer set up by tabsetPanel
#      Dataset: data input via hot or file 
#      AUC: graphics and estimation using the AUC method   
#  About: general info

shinyUI(
  navbarPage("SpawnEst",
            
    tabPanel("Home",
      br(),
      HTML('<center><img src="SpawnEst.jpg" height = "500"></center>'),
      br()
    ),

    tabPanel("Spawner Estimation",
      tabsetPanel(

        tabPanel("Dataset", 
          sidebarLayout(
            sidebarPanel(width = 3,
              HTML('<b>Enter data:</b>'),
              br(),
              br(),
              div(class = "table", style = "overflow:auto; ",
                rHandsontableOutput("hotDATA", height = 500)),
              br(),
              br(),
              HTML('<b>Upload data:</b>'),
                fileInput("entryFile", label = "", 
                  accept = c('text/csv', 'text/comma-separated-values', 
                    '.csv')
                )
            ),
            mainPanel(
              plotOutput(outputId = "countPlot", height = "550px")
            ),
          )
        ),
					
        tabPanel("AUC",
          sidebarLayout(
            sidebarPanel(width = 3,
              HTML('<b>Parameter Values</b>'),
		  br(),
              div(class = "table", style = "overflow:auto; ",
                rHandsontableOutput("hotAUCpt", height = 70)),
              div(class = "simpleDiv", verbatimTextOutput("AUCtext"))
            ),
            mainPanel(
              plotOutput(outputId = "countPlotAUC", height = "550px")
            ),
          )
        )
      )
    ),

    tabPanel("About",
      fluidRow(
        column(6, offset = 3,
          HTML('<img src = "SpawnEst.jpg" height = "150">'),
          br(), 
          br(),
          HTML('<b>Authors:</b>  Joseph L. Simonis 
            <a href = "http://www.dapperstats.com">(DAPPER Stats)</a> and 
            Lucius Caldwell <a href = "http://www.fishsciences.net">(Cramer 
            Fish Sciences)</a>'), 
          br(), 
          br(),
          HTML('SpawnEst is an online tool for estimating salmonid spawner 
            escapements from ground surveys. It is currently in development
            and should be considered provisional. SpawnEst is provided under 
            GNU GPL version 3 (and any later versions). 
            <a href="https://dapperstats.github.io/spawnest/">SpawnEst on 
            Github</a>')
        )
      )
    )
  )
)

