## EPFR FLOW-PERCENTAGE SECTOR BACKTEST

#DOWNLOAD "FloPctSector-US-daily.csv" FILE FROM EPFR FTP CONNECTION UNDER FOLDER "Strategies/daily"

#IMPORT LIBRARY
rm(list = ls())
library("EPFR.r")

#FLOWS
flow.file <- "C:\\EPFR\\daily\\FloPctSector-US-daily.csv"

#RETURNS
ret.file <- "C:\\EPFR\\returns\\PsuedoReturns-Sector-US-daily.csv"

#READ FILES
x <- mat.read(flow.file) # GET FLOW PERCENTAGE C:\\EPFR\\daily\\FloPctSector-US-daily.csv
y <- mat.read(ret.file)  # GET RETURN C:\\EPFR\\returns\\PsuedoReturns-Sector-US-daily.csv 

#ALIGN
x <- as.matrix(x[, dimnames(y)[[2]]]) # LINE UP INDICATORS WITH RETURNS 

#COMPOUND
lookback <- 20 # FLOW WINDOW (IN WEEKDAYS) - 20 day look back period
x <- compound.flows(x, lookback, F) # COMPOUND FLOWS

#TOTAL RETURN INDEX
y[is.na(y)] <- 0
y <- ret.to.idx(map.rname(y, dimnames(x)[[1]])) # CONVERT TO A TOTAL-RETURN INDEX
y <- ret.idx.gaps.fix(y)

#RANKING
nBin <- 5 # NUMBER OF BINS
delay <- 2 # DELAY IN KNOWING DATA (IN WEEKDAYS) - data takes time to have
doW <- 5 # DAY OF THE WEEK YOU WILL TRADE ON (5 = FRIDAYS)
hz <- c(5, 10, 20, 45, 65, 130) # RETURN HORIZON (IN WEEKDAYS) - holding periods
z <- bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay)

# MODEL
z[["bins"]]
z[["rets"]]

#SUMMARY
fcn <- function(retW) {as.matrix(bbk(x, y, 1, retW, nBin, doW, T, 0, delay)$summ)} # DEFINE SUMMARY FUNCTION
sapply(split(hz, hz), fcn, simplify = "array") # WRITE SUMMARIES
bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay)$annSumm # DISPLAY CALENDAR-YEAR RETURNS    
