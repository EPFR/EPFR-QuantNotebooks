## EPFR ACTIVE/PASSIVE COUNTRY BACKTEST

#DOWNLOAD "ActPasCtry-monthly.csv" FILE FROM EPFR FTP CONNECTION UNDER FOLDER "Strategies/monthly"

#IMPORT LIBRARY
rm(list = ls())
library("EPFR.r")

#INDICATOR
indicator.file <- "C:\\EPFR\\monthly\\ActPasCtry-monthly.csv"

#RETURNS
ret.file <- "C:\\EPFR\\returns\\PsuedoReturns-Country-ETF-daily.csv"

#READ FILES
x <- as.matrix(mat.read(indicator.file)) # GET ACTIVE/PASSIVE INDICATOR C:\\EPFR\\monthly\\ActPasCtry-monthly.csv
y <- mat.read(ret.file)  # GET TOTAL RETURN INDEX C:\\EPFR\\returns\\PsuedoReturns-Country-ETF-daily.csv
y <- mat.daily.to.monthly(y, T) #CHANGE RETS FROM DAILY TO MONTHLY

#SUBSET & ALIGN
idx <- "ACWI"
x <- x[, is.element(dimnames(x)[[2]], Ctry.msci.members.rng(idx, dimnames(x)[[1]][1], dimnames(x)[[1]][dim(x)[1]]))] # SUBSET TO INDEX COUNTRIES

startdate <- "20150512"
x <- x[rownames(x)>=startdate, ] # SUBSET TIME PERIOD
x <- x[, !(dimnames(x)[[2]] %in% c('JO', 'VE'))] # REMOVE CTRYS
y <- y[, dimnames(x)[[2]]] # SUBSET TOTAL RETURN INDEX

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
z <- bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay, idx)

# MODEL
z[["bins"]]
z[["rets"]]

#SUMMARY
fcn <- function(retW) {as.matrix(bbk(x, y, 1, retW, nBin, doW, T, 0, delay, idx)$summ)} # DEFINE SUMMARY FUNCTION
sapply(split(hz, hz), fcn, simplify = "array") # WRITE SUMMARIES
bbk(x, y, 1, hz[1], nBin, doW, T, 0, delay, idx)$annSumm # DISPLAY CALENDAR-YEAR RETURNS   
