## EPFR MULTI-ASSET BACKTEST

#DOWNLOAD "MultiAsset Daily" FILES FROM EPFR FTP CONNECTION UNDER FOLDER "Strategies/daily"

#FLOWS
flow.file.rgn <- "C:\\EPFR\\daily\\MultiAsset-Rgn-daily.csv"
flow.file.FI <- "C:\\EPFR\\daily\\MultiAsset-FI-daily.csv"

x.rgn <- mat.read(flow.file.rgn) # GET RGN FLOW PERCENTAGE C:\\EPFR\\daily\\MultiAsset-Rgn-daily.csv
x.FI <- mat.read(flow.file.FI) # GET FI FLOW PERCENTAGE C:\\EPFR\\daily\\MultiAsset-FI-daily.csv

x.rgn <- x.rgn[is.element(dimnames(x.rgn)[[1]], dimnames(x.FI)[[1]]), ] # ENSURES RGN MATCHES FI ROWS
x.FI <- x.FI[is.element(dimnames(x.FI)[[1]], dimnames(x.rgn)[[1]]), ] # ENSURES FI MATCHES RGN ROWS
x <- cbind(x.rgn, x.FI) # COMBINES RGN AND FI DATA

#COMPOUND
lookback <- 20 # FLOW WINDOW (IN WEEKDAYS) - 20 day look back period
x <- compound.flows(x, lookback, F) # COMPOUND FLOWS

#RETURNS
ret.file.rgn <- "C:\\EPFR\\returns\\PsuedoReturns-MultiAsset-Rgn-daily.csv"
ret.file.FI <- "C:\\EPFR\\returns\\PsuedoReturns-MultiAsset-FI-daily.csv"

#RGN RETURNS
y <- mat.read(ret.file.rgn) # GET RGN PERCENTAGE RETURNS C:\\EPFR\\returns\\PsuedoReturns-MultiAsset-Rgn-daily.csv
y <- ret.to.idx(map.rname(y, dimnames(x)[[1]])) # CONVERT TO A TOTAL-RETURN INDEX
y <- ret.idx.gaps.fix(y)

#F.I. RETURNS
z <- mat.read(ret.file.FI) # GET FI PERCENTAGE RETURNS C:\\EPFR\\returns\\PsuedoReturns-MultiAsset-FI-daily.csv
z <- ret.to.idx(map.rname(z, dimnames(y)[[1]])) # CONVERT TO A TOTAL-RETURN INDEX   

#COMBINE RETURN FILES
y <- data.frame(y, z)[, dimnames(x)[[2]]] # FINAL TOTAL-RETURN INDEX TABLE
y <- y[max(simplify2array(lapply(y, function(x) {find.data(!is.na(x), T)}))):min(simplify2array(lapply(y, function(x) {find.data(!is.na(x), F)}))), ] # ENSURE ALL PIECES HAVE RETURN OVER THE SAME HORIZON

#SUBSET & ALIGN
idx <- "Multi" # Multi/Rgn/FI
if (idx == "Rgn") { 
  x <- x[, 1:7]
  y <- y[, 1:7]
} else if (idx == "FI") {   
  x <- x[, 8:dim(x)[2]]
  y <- y[, 8:dim(y)[2]]
}  

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