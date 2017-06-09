###############################################################################
###############################################################################
##
##  This script contains the server code for the the SpawnEst app
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
#  2. server code
#

#1. load packages

  library(shiny)
  library(shinysky)
  library(rhandsontable)
  source("spawnestfunctions.R")

#2. server code

shinyServer(function(input, output) {

  # reactive values
  #   EF = entry file (logical re: its loading)
  #   ul = data frame created from the entry file

    rv <- reactiveValues(EF = F, ul = NULL)


  # Upload data button

    observeEvent(input$entryFile, {

      if (is.null(input$entryFile)) {
        rv$EF <- F
      } else {
        rv$EF <- T
        ktemp <- read.csv(input$entryFile$datapath, header = T)
        rv$ul <- data.frame(Date = as.Date(as.character(ktemp[,1])), 
          Counts = ktemp[,2])
      }

    })


  # output plot for data entry page

    output$countPlot <- renderPlot({

      if (!is.null(input$hotDATA)) {
        DF <- hot_to_r(input$hotDATA)
      } else {
        ktemp <- read.csv("www//auto_load.csv", header = T)
        DF <- data.frame(Date = as.Date(as.character(ktemp[,1])), 
          Counts = ktemp[,2])	
        if (rv$EF == T) {
          DF <- rv$ul
        }
      }
	
      spawnPlot(dates = DF$Date, counts = DF$Counts, surveylife = 7, 
        extend = F, drawtrap = F)
    })


  # output plot for AUC page

    output$countPlotAUC <- renderPlot({

      if (!is.null(input$hotDATA)) {
        DF <- hot_to_r(input$hotDATA)
      } else {
        ktemp <- read.csv("www//auto_load.csv",header=T)
        DF <- data.frame(Date = as.Date(as.character(ktemp[,1])), 
          Counts <- ktemp[,2])	
        if(rv$EF == T){
          DF <- rv$ul
        }
      }

      if (!is.null(input$hotAUCpt)) {
        PT <- hot_to_r(input$hotAUCpt)
        SurveyLife <- PT$SurveyLife
      } else {
        SurveyLife <- 7
      }

      spawnPlot(dates = DF$Date, counts = DF$Counts, surveylife = SurveyLife,
        extend = T, drawtrap = T)
    })


  # hot for the data

    output$hotDATA <- renderRHandsontable({

      if (rv$EF == T) {
        ktemp <- read.csv(input$entryFile$datapath, header = T)
        bb <- data.frame(Date = as.Date(as.character(ktemp[,1])), 
          Counts = ktemp[,2])
      } else {
        ktemp <- read.csv("www//auto_load.csv", header = T)
        bb <- data.frame(Date = as.Date(as.character(ktemp[,1])), 
          Counts = ktemp[,2])
      }

      rhandsontable(bb, readOnly = F,rowHeaders = NULL)
    })


  # hot for the AUC params

    output$hotAUCpt <- renderRHandsontable({
      cc <- data.frame(
        "SurveyLife" = 18.1,
        "SearcherEfficiency" = 0.499)
      rhandsontable(cc, readOnly = F,rowHeaders = NULL)
    })


  # calculation of AUC and output generation

    output$AUCtext <- renderText({

      if (!is.null(input$hotDATA)) {
        DF <- hot_to_r(input$hotDATA)
      } else {
        ktemp <- read.csv("www//auto_load.csv", header = T)
        DF <- data.frame(Date = as.Date(as.character(ktemp[,1])), 
          Counts = ktemp[,2])	
        if (rv$EF == T) {
          DF <- rv$ul
        }
      }

      if (!is.null(input$hotAUCpt)) {
        PT <- hot_to_r(input$hotAUCpt)
        SurveyLife <- PT$SurveyLife
        SearcherEfficiency <- PT$SearcherEfficiency
	} else {
        SurveyLife <- 7
        SearcherEfficiency <- 0.5
      }

	AUCe <- round(aucest(dates = DF$Date, counts = DF$Counts, 
        surveylife = SurveyLife, detectionrate = SearcherEfficiency),1)
	paste0("Estimated spawner count: ", AUCe)
    })


})

