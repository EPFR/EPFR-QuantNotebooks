## EPFR FLOW-PERCENTAGE COUNTRY BACKTEST

#DOWNLOAD "FloPctCtry-daily.csv" FILE FROM EPFR FTP CONNECTION UNDER FOLDER "Strategies/daily"

#IMPORT LIBRARY
rm(list = ls())
library("EPFR.r")

#FLOWS
flow.file <- "C:\\EPFR\\daily\\FloPctCtry-daily.csv"

#RETURNS
ret.file <- "C:\\EPFR\\returns\\PsuedoReturns-Country-ETF-daily.csv"

#READ FILES
x <- mat.read(flow.file) # GET FLOW PERCENTAGE C:\\EPFR\\daily\\FloPctCtry-daily.csv
y <- mat.read(ret.file)  # GET PERCENTAGE RETURNs C:\\EPFR\\returns\\PsuedoReturns-Country-ETF-daily.csv  

#SUBSET & ALIGN
idx <- "ACWI"
x <- x[, is.element(dimnames(x)[[2]], Ctry.msci.members.rng(idx, dimnames(x)[[1]][1], dimnames(x)[[1]][dim(x)[1]]))] # SUBSET TO INDEX COUNTRIES

startdate <- "20150512"
x <- x[rownames(x)>=startdate, ] # SUBSET TIME PERIOD
x <- x[, !(dimnames(x)[[2]] %in% "JO")] # REMOVE CTRY
y <- y[, dimnames(x)[[2]]] # SUBSET TOTAL RETURN INDEX

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
z <- bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay, idx)

# MODEL
z[["bins"]]
z[["rets"]]

#SUMMARY
fcn <- function(retW) {as.matrix(bbk(x, y, 1, retW, nBin, doW, T, 0, delay, idx)$summ)} # DEFINE SUMMARY FUNCTION
sapply(split(hz, hz), fcn, simplify = "array") # WRITE SUMMARIES
bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay, idx)$annSumm # DISPLAY CALENDAR-YEAR RETURNS    
