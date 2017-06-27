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
  library(pomp)
  source("spawnestfunctions.R")

#2. server code

shinyServer(function(input, output) {

  # reactive values
  #   EF = entry file (logical re: its loading)
  #   ul = data frame created from the entry file
  #   ppm = parameter pomp model

    rv <- reactiveValues(EF = F, ul = NULL, ppm = NULL)


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


  # output plot for pomp parameters

    output$pompparmplot <- renderPlot({

      if (!is.null(input$hotpompfigpt)) {
        PT <- hot_to_r(input$hotpompfigpt)
        THETA <- PT$theta
        A0 <- PT$alpha0
        A1 <- PT$alpha1
        A2 <- PT$alpha2
        M0 <- PT$mu0
        M1 <- PT$mu1
      } else {
        THETA <- 0.1
        A0 <- -14
        A1 <- -17
        A2 <- -17
        M0 <- 0.045
        M1 <- 0.02
      }
      parmplots(params = c(theta = THETA, alph0 = A0, alph1 = A1, alph2 = A2,
        mu0 = M0, mu1 = M1))

    })

  # model fit button

    observeEvent(input$modelBuild, {

      if (!is.null(input$hotpompfigpt)) {
        PT <- hot_to_r(input$hotpompfigpt)
        THETA <- PT$theta
        A0 <- PT$alpha0
        A1 <- PT$alpha1
        A2 <- PT$alpha2
        M0 <- PT$mu0
        M1 <- PT$mu1
        RHO <- PT$rho
      } else {
        THETA <- 0.1
        A0 <- -14
        A1 <- -17
        A2 <- -17
        M0 <- 0.045
        M1 <- 0.02
        RHO <- 0.499
      }
     
      rv$ppm <- pompbuild(dates = seq(as.Date("1990-01-01"), as.Date("1990-12-31"), 1), 
        counts = rep(0, length(1:365)), 
        parameters = c(theta = THETA, alph0 = A0, alph1 = A1, alph2 = A2,
        mu0 = M0, mu1 = M1, rho = RHO), year = 1990)

    })


  # output plot for pomp sims

    output$pompsimplot <- renderPlot({

      if (!is.null(rv$ppm)) {
        simpomp(pompobj = rv$ppm, NSims = 1000)
    
      } else {
        
      }

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



  # hot for the pomp exploration figure params

    output$hotpompfigpt <- renderRHandsontable({
      cc <- data.frame(
        "alpha0" = -14,
        "alpha1" = -17,
        "alpha2" = -17,
        "theta" = 0.1,
        "mu0" = 0.045,
        "mu1" = 0.02,
        "rho" = 0.499)
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

