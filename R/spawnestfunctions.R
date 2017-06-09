################################################################################
#
#	This script contains the functions of the spawnest package
#
#	Coded by Joseph (Josie, they) L. Simonis, DAPPER Stats
#
#	version 0.0.0.1 2017
#
#
################################################################################



# need for format everything in the headers better!

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



# aucest	
#
#	function for calculating area under the curve-based estimate
#
#	inputs
#			dates (formatted as dates: "YYYY-mm-dd")
#			counts
#			surveylife
#			detectionrate
#
#	outputs
#
	
	aucest<-function(dates=NULL, counts=NULL, surveylife=NULL, detectionrate=NULL){

		# ammend data set

			counts<-c(0,counts,0)
			dates<-c(min(dates)-surveylife, dates, max(dates)+surveylife)

		# calculate auc 

			counts1<-counts[1:(length(counts)-1)]
			dates1<-dates[1:(length(dates)-1)]
			counts2<-counts[2:length(counts)]
			dates2<-dates[2:length(dates)]

			auc<-sum(as.numeric(dates2-dates1)*(counts1+counts2)/2)

		# correct for detection

			aucc<-auc/detectionrate

		# calculate escapement	

			escapement<-aucc/surveylife

		# return
	
			return(escapement)
	}

# aucest_vp	
#
#	function for calculating area under the curve-based estimate with variable/uncertain parameters
#
#	inputs
#			dates (formatted as dates: "YYYY-mm-dd")
#			counts
#			surveylife parameters for a gamma distribution 
#			detectionrate parameters for a beta distribution
#			ndraws of how many estimates
#
#	outputs
#
	aucest_vp <- function(dates = NULL, counts = NULL, surveylife = c(shape = NULL, scale = NULL), detectionrate = c(shape1 = NULL, shape2 = NULL), ndraws = NULL, ...){

		slvs <- rgamma(ndraws, shape = surveylife["shape"], scale = surveylife["scale"])
		drvs <- rbeta(ndraws, shape1 = detectionrate["shape1"], shape2 = detectionrate["shape2"])

		aucestvs <- rep(NA, ndraws)
		for(i in 1:ndraws)
			aucestvs[i] <- aucest(dates = dates, counts = counts, surveylife = slvs[i], detectionrate = drvs[i])

		return(aucestvs)
	}

	
	
# spawnPlot	
#
#	function for flexible plotting of the observed counts
#
#	inputs
#			dates (formatted as dates: "YYYY-mm-dd")
#			counts
#			surveylife 
#			extend
#			drawtrap
#	outputs
#			figure
	
	spawnPlot <- function(dates = NULL, counts = NULL, surveylife = NULL, extend = NULL, drawtrap = NULL, ...){ 

		# x axis range
		
			x1 <- as.Date(paste(format(min(dates) - ceiling(surveylife), "%Y-%m"), "-01", sep = ""))
			nm <- as.character(as.numeric(format(max(dates) + ceiling(surveylife), "%m")) + 1)
			nm <- paste(rep(0, 2 - nchar(nm)), nm, sep = "")
			x2 <-as.Date(paste(format(max(dates) + ceiling(surveylife), "%Y"), "-", nm, "-01", sep = ""))

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
			plot(dates, counts, type = 'n', ylab= '', xlab = '', xaxt = 'n', yaxt = 'n', bty = "L", xlim = c(x1, x2), ylim = c(0, y2))
			points(datesA, countsA, type = 'l', lwd = 3, lty = 3, col = drawtrap)
			points(datesA[1], countsA[1], pch = 1, cex = 2.25, col = extend, lwd = 2)
			points(datesA[length(datesA)], countsA[length(datesA)], pch = 1, cex = 2.25, col = extend, lwd = 2)
			points(dates, counts, pch = 16, cex = 2.25, col = rgb(.8,.8,.8,.8), lwd = 1)
			points(dates, counts, pch = 1, cex = 2.25, col = rgb(0,0,0,.8), lwd = 2)
			axis(1, xaxlbs, labels = as.character(format(xaxlbs, "%B%e")), cex.axis = 1.25)
			axis(2, yaxlbs1, las = 1, cex.axis = 1.5)
			axis(2, yaxlbs2, labels = F, tck = -0.01)
			mtext(side = 2, "Observed Spawners", line = 5.25, cex = 2)

	}
