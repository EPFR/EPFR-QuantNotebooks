## EPFR ACTIVE/PASSIVE SECTOR BACKTEST

#DOWNLOAD "ActPasSector-US-monthly.csv" FILE FROM EPFR FTP CONNECTION UNDER FOLDER "Strategies/monthly"

#IMPORT LIBRARY
rm(list = ls())
library("EPFR.r")

#INDICATOR
indicator.file <- "C:\\EPFR\\monthly\\ActPasSector-US-monthly.csv"

#RETURNS
ret.file <- "C:\\EPFR\\returns\\PsuedoReturns-Sector-US-daily.csv"

#READ FILES
x <- mat.read(indicator.file) # GET ACTIVE/PASSIVE INDICATOR C:\\EPFR\\monthly\\ActPasSector-US-monthly.csv
y <- mat.read(ret.file)  # GET TOTAL RETURN INDEX C:\\EPFR\\returns\\PsuedoReturns-Sector-US-daily.csv
y <- mat.daily.to.monthly(y, T) #CHANGE RETS FROM DAILY TO MONTHLY

#SUBSET & ALIGN
x <- as.matrix(x[, dimnames(y)[[2]]]) # LINE UP INDICATORS WITH RETURNS         

#COMPOUND
lookback <- 12 # LOOKBACK (IN MONTHS)               
x <- compound.flows(x, lookback, T) # COMPUTE MOVING SUM

#TOTAL RETURN INDEX
y[is.na(y)] <- 0
y <- ret.to.idx(map.rname(y, dimnames(x)[[1]])) # CONVERT TO A TOTAL-RETURN INDEX
y <- ret.idx.gaps.fix(y)

#RANKING
nBin <- 5 # NUMBER OF BINS
delay <- 1 # DELAY IN KNOWING DATA (IN MONTHS)  
doW <- NULL # DAY OF THE WEEK YOU WILL TRADE ON (5 = FRIDAYS)
hz <- c(1, 3, 6, 12) # RETURN HORIZON (IN MONTHS)
z <- bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay)

# MODEL
z[["bins"]]
z[["rets"]]

#SUMMARY
fcn <- function(retW) {as.matrix(bbk(x, y, 1, retW, 5, NULL, T, 0, delay)$summ)} # DEFINE SUMMARY FUNCTION  
sapply(split(hz, hz), fcn, simplify = "array") # WRITE SUMMARIES
bbk(x, y, 1, hz[1], 5, NULL, T, 0, delay)$annSumm # DISPLAY CALENDAR-YEAR RETURNS   
