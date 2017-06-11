###############################################################################
#
# This script contains the functions of the spawnest package
#
# Coded by Joseph (Josie, they) L. Simonis, DAPPER Stats
#
# version 0.0.0.1 2017
#
#
###############################################################################

# need to format everything in the headers better!

#' TITLEHERE
#'
#' DESCRIPTIONHERE
#' @param 
#' @return
#' @keywords 
#' @export
#' @examples
#' 
#


###############################################################################
# aucest	
#  function for calculating area under the curve-based estimate
#
# inputs
#  dates (formatted as dates: "YYYY-mm-dd")
#  counts
#  surveylife
#  detectionrate
#
# outputs
#  estimated auc value
###############################################################################
	
aucest <- function(dates = NULL, counts = NULL, surveylife = NULL, 
  detectionrate = NULL, ...){

  # ammend data set

    counts <- c(0,counts,0)
    dates <- c(min(dates) - surveylife, dates, max(dates) + surveylife)

  # calculate auc 

    counts1 <- counts[1:(length(counts) - 1)]
    dates1 <- dates[1:(length(dates) - 1)]
    counts2 <- counts[2:length(counts)]
    dates2 <- dates[2:length(dates)]
    auc <- sum(as.numeric(dates2 - dates1) * (counts1 + counts2) / 2)

  # correct for detection

    aucc <- auc / detectionrate

  # calculate escapement	

    escapement <- aucc / surveylife

  # return
	
    return(escapement)
}

###############################################################################
# aucest_vp	
#  function for calculating area under the curve-based estimate with 
#  variable/uncertain parameters
#
# inputs
#  dates (formatted as dates: "YYYY-mm-dd")
#  counts
#  surveylife parameters for a gamma distribution 
#  detectionrate parameters for a beta distribution
#  ndraws of how many estimates
#
# outputs
#  estimated auc
###############################################################################

aucest_vp <- function(dates = NULL, counts = NULL, 
  surveylife = c(shape = NULL, scale = NULL), 
  detectionrate = c(shape1 = NULL, shape2 = NULL), ndraws = NULL, ...){

  slvs <- rgamma(ndraws, shape = surveylife["shape"], 
    scale = surveylife["scale"])
  drvs <- rbeta(ndraws, shape1 = detectionrate["shape1"], 
    shape2 = detectionrate["shape2"])

  aucestvs <- rep(NA, ndraws)
  for(i in 1:ndraws){
    aucestvs[i] <- aucest(dates = dates, counts = counts, 
      surveylife = slvs[i], detectionrate = drvs[i])
  }

  return(aucestvs)
}

	
###############################################################################	
# spawnPlot	
#  function for flexible plotting of the observed counts
#
# inputs
#  dates (formatted as dates: "YYYY-mm-dd")
#  counts
#  surveylife 
#  extend
#  drawtrap
#
# outputs
#  figure
###############################################################################
	
spawnPlot <- function(dates = NULL, counts = NULL, surveylife = NULL, 
  extend = NULL, drawtrap = NULL, ...){ 

  # x axis range
		
    x1 <- as.Date(paste(format(min(dates) - ceiling(surveylife), "%Y-%m"), 
      "-01", sep = ""))
    nm <- as.character(as.numeric(format(max(dates) + ceiling(surveylife), 
      "%m")) + 1)
    nm <- paste(rep(0, 2 - nchar(nm)), nm, sep = "")
    x2 <-as.Date(paste(format(max(dates) + ceiling(surveylife), "%Y"), 
      "-", nm, "-01", sep = ""))

  # x axis labels
			
    mos <- seq(as.numeric(format(x1, "%m")), as.numeric(format(x2, "%m")), 1)
    mosd <- rep(NA, length(mos))

    for(i in 1:length(mos))
      mosd[i] <- paste(rep(0, 2 - nchar(mos[i])), mos[i], sep = "")

    xaxlbs <- as.Date(paste("1990-", mosd, "-01", sep = ""))

  # y axis range
			
    y2 <- max(counts) * 1.1 

  # y axis labels
		
    yaxlbs1 <- seq(0, y2, 10^(floor(log10(y2))))
    yaxlbs2 <- seq(0, y2, 5 * (10^(floor(log10(y2)) - 1)))

  # ammend the data for use if desired

    countsA <- c(0,counts,0)
    datesA <- c(min(dates) - surveylife, dates, max(dates) + surveylife)

  # plot
		
    par(mar = c(3,7,1,2))
    plot(dates, counts, type = 'n', ylab= '', xlab = '', xaxt = 'n', 
      yaxt = 'n', bty = "L", xlim = c(x1, x2), ylim = c(0, y2))
    points(datesA, countsA, type = 'l', lwd = 3, lty = 3, col = drawtrap)
    points(datesA[1], countsA[1], pch = 1, cex = 2.25, col = extend, lwd = 2)
    points(datesA[length(datesA)], countsA[length(datesA)], pch = 1, 
      cex = 2.25, col = extend, lwd = 2)
    points(dates, counts, pch = 16, cex = 2.25, col = rgb(.8,.8,.8,.8), 
      lwd = 1)
    points(dates, counts, pch = 1, cex = 2.25, col = rgb(0,0,0,.8), lwd = 2)
    axis(1, xaxlbs, labels = as.character(format(xaxlbs, "%B%e")), 
      cex.axis = 1.25)
    axis(2, yaxlbs1, las = 1, cex.axis = 1.5)
    axis(2, yaxlbs2, labels = F, tck = -0.01)
    mtext(side = 2, "Observed Spawners", line = 5.25, cex = 2)

  return()
}

###############################################################################
# pomp build
#  function for building a pomp object
#
# inputs
#  dates (formatted as dates: "YYYY-mm-dd")
#  counts
#  model parameters (theta, alph0, alph1, alph2, mu0, mu1, rho) 
#  year
#
# outputs
#  built pomp object
#
###############################################################################

pompbuild <- function(dates = NULL, counts = NULL, 
  parameters = c(theta = NULL, alph0 = NULL, alph1 = NULL, alph2 = NULL, 
  mu0 = NULL, mu1 = NULL, rho = NULL), year = NULL, ...){

  # measurement model
  # simulation of counts based on spawners (S) and detection probability (rho)
  # measurement density for a given count, under the number of spawners 
  # and detection probability

    rmeas <- "counts = rbinom(S, rho);"
    dmeas<- "lik = dbinom(counts, S, rho, give_log);"	
	
  # process model
  # model step  

    mod.step<- "double dN[2];
                double alph;
                double yrff; // the amount time incremented with a dt
                double mu = exp(mu0 + mu1 * yrf);

                alph = exp(alph0 + alph1 * cos(M_2PI * yrf) + 
                  alph2 * sin(M_2PI * yrf));  
                if (alph < 0)
                  alph = 0;
						
                dN[0] = rnbinom_mu(theta, alph * dt); // N0 fish arrived
                dN[1] = rbinom(S, 1.0 - exp(-mu * dt)); // N1 fish died

                TSA += dN[0]; // total spawners arrived
                S += dN[0] - dN[1]; // alive spawners
				
                yrff = dt/tdaysYr; 
                yrf += yrff ; // increment the time (in year fraction) forward
                "

  # estimation scales

    toEstimationScale <-   "Ttheta = log(theta);
                            Trho = logit(rho);
                            "				 
  
  
    fromEstimationScale <- "Ttheta = exp(theta);
                            Trho = expit(rho);
                            "

  # state names

    StateNames <- c("TSA", "S", "yrf")

  # parameter names and values

    ParameterNames <- c("theta", "alph0", "alph1", "alph2", "tdaysYr",
                      "mu0", "mu1", "rho", "TSA.0", "S.0", "yrf.0")
 
    tdy <- as.numeric(format(as.Date(paste(year, "-12-31", sep = "")), "%j"))

    ParameterValues <- c(parameters, TSA.0 = 0, S.0 = 0, yrf.0 = 0, 
      tdaysYr = tdy)

  # set the observations

    Obs <- data.frame(counts = counts, time = as.numeric(format(dates, "%j")))

  # construct the object

    pompobj <- pomp(data = Obs, times = "time", t0 = 0,
      dmeasure = Csnippet(dmeas), rmeasure = Csnippet(rmeas),
      rprocess = euler.sim(step.fun = Csnippet(mod.step), delta.t = 1/12),
      statenames = StateNames, paramnames = ParameterNames,
      fromEstimationScale = Csnippet(fromEstimationScale),
      toEstimationScale = Csnippet(toEstimationScale),
      params = ParameterValues, transform = TRUE)

  # return the object

    return(pompobj)

}

################################################################################
# simpomp 
#  function for simulating a built pomp object for graphics
#
# inputs
#  built pomp object
#  number of simulations
#
# outputs
#  figures
#
################################################################################

simpomp <- function (pompobj = NULL, NSims = NULL, ...){

  # simulate pomp, store data, summarize data 

    simm <- simulate(pompobj, nsim = NSims)

    simdO <- matrix(NA, ncol = NSims, nrow = length(simm[[1]]@times))
    simsS <- matrix(NA, ncol = NSims, nrow = length(simm[[1]]@times))
    simsTS <- rep(NA, NSims)
    for (i in 1:NSims) {	
      simdO[ , i] <- t(simm[[i]]@data)
      simsS[ , i] <- t(simm[[i]]@states)[ , 2]
      simsTS[i] <- t(simm[[i]]@states)[length(simm[[1]]@times), 1]
    }
    ps <- c(0.025, 0.25, 0.5, 0.75, 0.975)
    simsS_qs <- apply(simsS, 1, quantile, probs = ps )
    simdO_qs <- apply(simdO, 1, quantile, probs = ps )

  # set up total plot

    par(mar = c(3,7,1,2), mfrow = c(3, 1))

  # plot expected actual 

    # x axis range
		
      x1 <- 1
      x2 <- 365

    # x axis labels

      mds <- as.numeric(format(as.Date(paste("1990-", month.abb, "-01", 
        sep = ""), format="%Y-%b-%d"), "%j")			)
      xaxlbs <- month.abb

    # y axis range
			
      y2 <- max(simsS_qs) * 1.1 

    # y axis labels
		
      yaxlbs1 <- seq(0, y2, 10^(floor(log10(y2))))
      yaxlbs2 <- seq(0, y2, 1 * (10^(floor(log10(y2)) - 1)))

    # plot
		
      plot(1, 1, type = 'n', ylab= '', xlab = '', xaxt = 'n', 
        yaxt = 'n', bty = "L", xlim = c(x1, x2), ylim = c(0, y2))
   
      axis(1, mds, labels = xaxlbs, cex.axis = 1.25)
      axis(2, yaxlbs1, las = 1, cex.axis = 1.5)
      axis(2, yaxlbs2, labels = F, tck = -0.01)
      mtext(side = 2, "Predicted Spawners", line = 5.25, cex = 1.5)

      polygon(c(1:365, 365:1, 1),
        c(simsS_qs[1, ], simsS_qs[5, 365:1], simsS_qs[1, 1]),
        col = rgb(0.8, 0.8, 0.8) )
      polygon(c(1:365, 365:1, 1),
        c(simsS_qs[2, ], simsS_qs[4, 365:1], simsS_qs[2, 1]),
        col = rgb(0.5, 0.5, 0.5) )
      points(1:365, simsS_qs[3, ], type = 'l', lwd = 1)

  # plot expected counts 

    # y axis range
			
      y2 <- max(simdO_qs) * 1.1 

    # y axis labels
  		
      yaxlbs1 <- seq(0, y2, 10^(floor(log10(y2))))
      yaxlbs2 <- seq(0, y2, 5 * (10^(floor(log10(y2)) - 1)))

    # plot
		
      plot(1, 1, type = 'n', ylab= '', xlab = '', xaxt = 'n', 
        yaxt = 'n', bty = "L", xlim = c(x1, x2), ylim = c(0, y2))
   
      axis(1, mds, labels = xaxlbs, cex.axis = 1.25)
      axis(2, yaxlbs1, las = 1, cex.axis = 1.5)
      axis(2, yaxlbs2, labels = F, tck = -0.01)
      mtext(side = 2, "Predicted Counts", line = 5.25, cex = 1.5)

      polygon(c(1:365, 365:1, 1),
        c(simdO_qs[1, ], simdO_qs[5, 365:1], simdO_qs[1, 1]),
        col = rgb(0.8, 0.8, 0.8) )
      polygon(c(1:365, 365:1, 1),
        c(simdO_qs[2, ], simdO_qs[4, 365:1], simdO_qs[2, 1]),
        col = rgb(0.5, 0.5, 0.5) )
      points(1:365, simdO_qs[3, ], type = 'l', lwd = 1)

  # histogram of total counts

    mv <- max(simsTS) * 1.1
    stsp <- 10^(floor(log10(max(simsTS)))-1)
    spts <- seq(0, mv + stsp, stsp)
    talls <- rep(NA, length(spts))
    for (i in 1:length(spts)) {
      talls[i] <- length(simsTS[simsTS >= spts[i] & simsTS < (spts[i] + stsp)])
    }

    y2 <- max(talls) * 1.1 
    yaxlbs1 <- seq(0, y2, 10^(floor(log10(y2))))
    yaxlbs2 <- seq(0, y2, 5 * (10^(floor(log10(y2)) - 1)))

    par(mar = c(5,7,1,2))
    plot(1, 1, type = 'n', xlim = c(0, mv), ylim = c(0, y2), xaxt = 'n', 
        yaxt = 'n', bty = "L", ylab = "", xlab = "")

    axis(1, spts , labels = spts , cex.axis = 1.25)
    axis(2, yaxlbs1, las = 1, cex.axis = 1.5)
    axis(2, yaxlbs2, labels = F, tck = -0.01)
    mtext(side = 2, "Simulations", line = 5, cex = 1.5)
    mtext(side = 1, "Total Spawners", line = 3.75, cex = 1.5)
    for (i in 1:length(spts)) {
      rect(spts[i], 0, spts[i] + stsp, talls[i])
    }

}



################################################################################
# parmplots 
#  function for plotting functional relationships based on parameters
#
# inputs
#  parameters
#
# outputs
#  figures
#
################################################################################

parmplots <- function (params = c(theta = NULL, alph0 = NULL, alph1 = NULL, 
  alph2 = NULL, mu0 = NULL, mu1 = NULL), ...){

  tv <- seq(0, 365, 0.1)

  theta <- params["theta"]
  theta[is.na(theta)] <- 0.01
  theta[theta <= 0] <- 0.01

  # functional relationships

    ALPH <- exp(params["alph0"] + params["alph1"] * cos(2 * pi * tv / 365) + 
      params["alph2"] * sin(2 * pi * tv / 365) )  

    mu <- params["mu0"] + params["mu1"] * tv / 365
    sl <- 1 / mu

  # simulate arrival process

    ap <- matrix(0, nrow = 100, ncol = length(tv))
    for (i in 1:ncol(ap)) {
      alph <- ALPH[i]
      alph[is.na(alph)] <- 0
      ap[ , i] <- rnbinom(100, mu = alph, size = theta)
    }

    ps <- c(0.025, 0.25, 0.5, 0.75, 0.975)
    apqs <- apply(ap, 2, quantile, prob = ps )
    apm <- apply(ap, 2, mean)
    apsd <- apply(ap, 2, sd)

  # set up total plot

    par(mar = c(3,7,1,2), mfrow = c(3, 1))

  # plot arrival rate 

    # x axis range
		
      x1 <- 1
      x2 <- 365

    # x axis labels

      mds <- as.numeric(format(as.Date(paste("1990-", month.abb, "-01", 
        sep = ""), format="%Y-%b-%d"), "%j")			)
      xaxlbs <- month.abb

    # y axis range
			
      y2 <- max(ALPH) * 1.5 
      y2[is.na(y2)] <- 10

    # y axis labels
		
      yaxlbs1 <- seq(0, y2, 10^(floor(log10(y2))))
      yaxlbs2 <- seq(0, y2, 1 * (10^(floor(log10(y2)) - 1)))

    # plot
		
      plot(1, 1, type = 'n', ylab= '', xlab = '', xaxt = 'n', 
        yaxt = 'n', bty = "L", xlim = c(x1, x2), ylim = c(0, y2))
   
      axis(1, mds, labels = xaxlbs, cex.axis = 1.25)
      axis(2, yaxlbs1, las = 1, cex.axis = 1.5)
      axis(2, yaxlbs2, labels = F, tck = -0.01)
      mtext(side = 2, "Arrival Rate", line = 5.25, cex = 1.5)

      points(tv, ALPH, type = 'l', lwd = 3)


  # plot arrivals

    # y axis range
			
      y2 <- max(apqs) * 1.5  

    # y axis labels
		
      yaxlbs1 <- seq(0, y2, 10^(floor(log10(y2))))
      yaxlbs2 <- seq(0, y2, 1 * (10^(floor(log10(y2)) - 1)))

    # plot
		
      plot(1, 1, type = 'n', ylab= '', xlab = '', xaxt = 'n', 
        yaxt = 'n', bty = "L", xlim = c(x1, x2), ylim = c(0, y2))
   
      axis(1, mds, labels = xaxlbs, cex.axis = 1.25)
      axis(2, yaxlbs1, las = 1, cex.axis = 1.5)
      axis(2, yaxlbs2, labels = F, tck = -0.01)
      mtext(side = 2, "Arrivals", line = 5.25, cex = 1.5)
      for (i in 1:100) {
        points(tv, ap[i, ], type = 'l', col = rgb(0.9, 0.9, 0.9, 0.2))
      }
      points(tv, apm, type = 'l', lwd = 2) 

  # plot mortality rate 


    # y axis range
			
      y2 <- max(sl) * 1.2 
      y2[is.na(y2)] <- 20

    # y axis labels
		
      yaxlbs1 <- seq(0, y2, 10^(floor(log10(y2))))
      yaxlbs2 <- seq(0, y2, 1 * (10^(floor(log10(y2)) - 1)))

    # plot
		
      plot(1, 1, type = 'n', ylab= '', xlab = '', xaxt = 'n', 
        yaxt = 'n', bty = "L", xlim = c(x1, x2), ylim = c(0, y2))
   
      axis(1, mds, labels = xaxlbs, cex.axis = 1.25)
      axis(2, yaxlbs1, las = 1, cex.axis = 1.5)
      axis(2, yaxlbs2, labels = F, tck = -0.01)
      mtext(side = 2, "Survey Life", line = 5, cex = 1.5)

      points(tv, sl, type = 'l', lwd = 3)
}
