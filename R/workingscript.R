################################################################################
#
#	This script is the active development space of the spawnest package
#
#	Coded by Joseph (Josie, they) L. Simonis, DAPPER Stats
#
#	version 0.0.0.1 2017
#
#
################################################################################





# load dependencies

	library(pomp); library(plyr); library(KernSmooth); library(doParallel); library(caTools)

	source("spawnestfunctions.R")


# example

	# read in data

		xx <- read.csv("www/auto_load.csv", header = T)
		dates <- as.Date(as.character(xx$day))
		counts <- xx$count

	# plot 

		windows(14,8)
		spawnPlot(dates = dates, counts = counts, surveylife = 18.1, extend = T, drawtrap = T)



	# fixed parameter estimates of escapement using AUC with values for 1990

		fpest1 <- aucest(dates = dates, counts = counts, surveylife = 18.1, detectionrate = 0.499)
	
	# fixed parameter estimates of escapement using AUC with average values

		fpest2 <- aucest(dates = dates, counts = counts, surveylife = 18.5, detectionrate = 0.41)
	
	# uncertain parameter estimates of escapement using AUC with parameter inputs that give the means and sds of survey life (18.5 +/- 2.8) and detection rate (0.41 +/- 0.20)

		mean(rgamma(1000, shape = 44.3, scale = 0.418))
		sd(rgamma(1000, shape = 44.3, scale = 0.418))
		mean(rbeta(1000, shape1 = 2.07, shape2 = 2.98))
		sd(rbeta(1000, shape1 = 2.07, shape2 = 2.98))

		vpest <- aucest_vp(dates = dates, counts = counts, surveylife = c(shape = 44.3, scale = 0.418), detectionrate = c(shape1 = 2.07, shape2 = 2.98), ndraws = 10000)
	
		hist(vpest, breaks = seq(0, max(vpest)+1000, 1000))
		abline(v = fpest2, lwd = 3, col = rgb(0,0,0, .5))
		mean(vpest)
		median(vpest)
		fpest2	



  # pull out full year

    dd <- data.frame(counts = 0, time = 1:365)
    pompobj2 <- simulate(pompobj, data = dd, times = "time", t0 = 0)



pompobj <- pompbuild(dates = seq(as.Date("1990-01-01"), as.Date("1990-12-31"), 1), counts = rep(0, length(1:365)), parameters = c(theta = 0.01, alph0 = -12, alph1 = -17, alph2 = -9, mu0 = 0.07, mu1 = 0, rho = 0.45), year = 1990)

simTSA <- simpomp(pompobj = pompobj, NSims = 1000)

hist(simTSA)




parmplots(params = c(theta = 0.01, alph0 = -12, alph1 = -17, alph2 = -9, mu0 = 0.05, mu1 = 0.1))




 


