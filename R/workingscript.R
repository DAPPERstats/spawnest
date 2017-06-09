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

		xx <- read.csv("hilbornetal1999catheadcreek.csv", header = T)
		dates <- as.Date(as.character(xx$day))
		counts <- xx$count

	# plot 

		windows(14,8)
		spawnPlot(dates = dates, counts = counts, surveylife = 18.1, extend = T, drawtrap = T)



xx <- read.table("hilbornetal1999herringcreek.txt", header=T)

xx$day <- as.Date(xx$day, origin ="1990-01-01")
write.csv(xx, "hilbornetal1999herringcreek.csv", row.names=F)





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









# basic model. works for simulation! :D


	# measurement model

		# simulation of counts based on spawners (S) and detection probability (rho)

			rmeas<-	" counts = rbinom(S, rho); "

		# measurement density for a given count, under the number of spawners and detection probability

			dmeas<-	" lik = dbinom(counts, S, rho, give_log); "	
	
	# process model

		# model step  

			mod.step<- "	
						double rate[1];
						double dN[2];
						double Delta;
						double Phaa;			// Pha = time (fraction of the calendar year), Phaa = the amount time is incremented with a dt
	
						rate[0] = mu;			// rate 0 = rate of mortality

						Delta = exp(rnorm(delta0 + delta1*cos(M_2PI * Pha) + delta2*sin(M_2PI *Pha), theta));  

  
						if(Delta < 0 )
							Delta = 0;
						
						dN[0] = rpois(Delta*dt);		// dN 0 = number of fish arrived to the system
 
						reulermultinom(1, S, &rate[0], dt, &dN[1]);  	// dN 1 = number of fish died


  
						TSA  += dN[0];						// total spawners arrived
						S  += dN[0] - dN[1] ;					// alive spawners
						
						Phaa = dt/tdaysYr;					// tdaysYr = total days in the year (365 or 366), so this translates days to years
  						Pha += Phaa; 						// increment the time (in years) forward
					"
  
 
			toEstimationScale<-   "
								Ttheta = log(theta);
								TtdaysYr = log(tdaysYr);
 
								Tmu = log(mu);

								Trho = logit(rho);
							"
  
  
			fromEstimationScale<- "
								Ttheta = exp(theta);
								TtdaysYr = exp(tdaysYr);

								Tmu = exp(mu);

								Trho = expit(rho);
							"

			StateNames<-c("TSA", "S", "Pha")

			ParameterNames<-c("theta", "delta0", "delta1", "delta2","tdaysYr",
			                  "mu",       "rho", 
		            	      "TSA.0", "S.0", "Pha.0")
 


			Observations<-data.frame(counts = NA, time = xx$day)
	
			ParameterValues<-c(theta=1, delta0 = -12, delta1=-17, delta2=-9, tdaysYr=365,
				mu = 1, rho = .41, TSA.0=0, S.0=0, Pha.0=0)


			pompforsim<-pomp(data = Observations, times = "time", t0 = -1/24/60,
						dmeasure = Csnippet(dmeas), rmeasure = Csnippet(rmeas), 
						rprocess = euler.sim(step.fun = Csnippet(mod.step), delta.t = 1/24/60),
						statenames = StateNames, paramnames = ParameterNames,
						fromEstimationScale = Csnippet(fromEstimationScale),
						toEstimationScale = Csnippet(toEstimationScale),							
						params=ParameterValues, transform=TRUE)

			simmed<-simulate(pompforsim)

			plot(simmed)
			simmed@data



xxx<-seq(0,365,1)
yy<-rpois(length(xxx), exp(rnorm(length(xxx), -20 + -17*cos(2*pi * xxx/365) + -9*sin(2*pi *xxx/365),2)))
plot(yy~xxx)
abline(v=180)
abline(v=250)
