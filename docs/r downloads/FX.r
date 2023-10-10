## EPFR FX BACKTEST

#DOWNLOAD "FX-daily.csv" FILE FROM EPFR FTP CONNECTION UNDER FOLDER "Strategies/daily"

#IMPORT LIBRARY
rm(list = ls())
library("EPFR.r")

#FLOWS
flow.file <- "C:\\EPFR\\daily\\FX-daily.csv"

#RETURNS
ret.file <- "C:\\EPFR\\returns\\ExchRates-daily.csv"

#READ FILES
x <- mat.read(flow.file) # GET FLOW PERCENTAGE "C:\\EPFR\\daily\\FX-daily.csv"
y <- 1/mat.read(ret.file) # GET EXCHANGE RATES "C:\\EPFR\\returns\\ExchRates-daily.csv" 

#SUBSET & ALIGN
idx <- "ACWI"
y$CNY <- ifelse(is.na(y$CNH), y$CNY, y$CNH) # USE CNH WHENEVER POSSIBLE 
y$USD <- rep(1, dim(y)[1]) # ADD IN USD 
idx.curr <- unique(Ctry.info(Ctry.msci.members(idx, ""), "Curr")) # CURRENCY CLASSIFICATION 2016
if (idx != "G10") idx.curr <- union(Ctry.info(Ctry.msci(idx)[, "CCODE"], "Curr"), idx.curr) else idx <- NULL # CAPTURE INDEX CHANGES
if (is.element("EM", idx)) idx.curr <- setdiff(idx.curr, c("USD", "EUR")) # ENSURE NO OVERLAP BETWEEN DEVELOPED AND EM CURRENCIES

x <- x[, is.element(dimnames(x)[[2]], idx.curr)] # SUBSET TO CURRENCIES OF INTEREST     
y <- y[, dimnames(x)[[2]]]/y[, "XDR"] # CURRENCIES OF INTEREST ON AN SDR BASE (OTHERWISE <get.fwdRet> THINKS THE USD NEVER TRADES!)     

#COMPOUND
lookback <- 20 # FLOW WINDOW (IN WEEKDAYS) - 20 day look back period
x <- compound.flows(x, lookback, F) # COMPOUND FLOWS

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
