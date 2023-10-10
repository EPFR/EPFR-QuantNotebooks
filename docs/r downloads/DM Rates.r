## EPFR DM RATES BACKTEST

#DOWNLOAD "FloPctCtry-B-daily.csv" FILE FROM EPFR FTP CONNECTION UNDER FOLDER "Strategies/daily"

#IMPORT LIBRARY
rm(list = ls())
library("EPFR.r")

#FLOWS
flow.file <- "C:\\EPFR\\daily\\FloPctCtry-B-daily.csv"

#RETURNS
ret.file <- "C:\\EPFR\\returns\\daily\\g10rates.csv"

#READ FILES
x <- mat.read(flow.file) # GET FLOW PERCENTAGE C:\\EPFR\\daily\\FloPctCtry-B-daily.csv
y <- mat.read(ret.file) # TOTAL RETURN INDEX C:\\EPFR\\returns\\g10rates.csv

#SUBSET & ALIGN
x <- x[, is.element(dimnames(x)[[2]],dimnames(y)[[2]])] # SUBSET TO G10 COUNTRIES			
y <- y[,dimnames(x)[[2]]]	        

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
z <- bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay, sprds = T)

# MODEL
z[["bins"]]
z[["rets"]]

#SUMMARY
fcn <- function(retW) {as.matrix(bbk(x, y, 1, retW, nBin, doW, T, 0, delay, sprds = T)$summ)} # DEFINE SUMMARY FUNCTION         
sapply(split(hz, hz), fcn, simplify = "array") # WRITE SUMMARIES            
bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay, sprds = T)$annSumm # DISPLAY CALENDAR-YEAR RETURNS   
