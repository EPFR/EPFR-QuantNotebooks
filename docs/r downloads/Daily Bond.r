## EPFR MULTI-ASSET BACKTEST

#DOWNLOAD "Premium Daily Bond" STRATEGY FILES FROM EPFR FTP CONNECTION UNDER FOLDER "PremiumDailyFundFlows/Strategies/PremDaily"

#IMPORT LIBRARY
rm(list = ls())
library("EPFR.r")

#FLOWS
flow.file <- "C:\\EPFR\\PremDaily\\FloPctBond-US-PremDaily700.csv"

#RETURNS
ret.file <- "C:\\EPFR\\returns\\PremDaily\\USBond_ETFS.txt" 

#READ FILES
x <- mat.read(flow.file) # GET FLOW PERCENTAGE
y <- mat.read(ret.file, "\t") # GET DAILY RETURNS

# MISSING FLOW DATES (GETS PRIOR-DAY'S FLOW PERCENTAGE)
w <- dimnames(x)[[1]]
x <- map.rname(x, flowdate.seq(w[1], rev(w)[1]))
x <- fcn.mat.vec(fix.gaps, x, , T)

# TOTAL-RETURN INDEX
y <- ret.to.idx(map.rname(y, dimnames(x)[[1]])) 
y <- ret.idx.gaps.fix(y)

#RANKING
lookback <- 1 # FLOW WINDOW (IN WEEKDAYS) - 1 day look back period
nBin <- 5 # NUMBER OF BINS
delay <- 0 # DELAY IN KNOWING DATA (IN WEEKDAYS) - data takes time to have
doW <- NULL # DAY OF THE WEEK YOU WILL TRADE ON (5 = FRIDAYS)
z <- bbk(x, y, 1, 1, 5, NULL, T, 0, delay)

# MODEL
z[["bins"]]
z[["rets"]]

#SUMMARY
z$summ # WRITE SUMMARIES
z$annSumm # DISPLAY CALENDAR-YEAR RETURNS