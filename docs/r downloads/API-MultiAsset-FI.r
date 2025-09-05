#  INSTALL PACKAGES
# install.packages ("epfr", repos = "https://downloads.epfrglobal.com/sdks/R/", type = "source") 
library(epfr)
library(dplyr)

# LOGIN TO API
username <- "Your EPFR username"
password <- "Your EPFR password"

epfr.login(username, password)

# HELPER FUNCTIONS FOR API CALLS
wait_for_completion <- function(report_id, dataset, inital_time_offset=5, time_offset=10) {
  
  time_taken <- 0
  
  print(paste0("Inital wait for report to complete, delay ", inital_time_offset))
  Sys.sleep(inital_time_offset)
  time_taken <- time_taken + inital_time_offset
  
  st <- epfr.get.reportstatus(dataset, report_id)$data$status # CHECK STATUS
  
  while (st != "Completed") {
    st <- epfr.get.reportstatus(dataset, report_id)$data$status
    
    if (st=="Completed") { 
      break # REPORT COMPLETE
    } else if (st %in% c("Errored","Failed")) {
      stop("EPFR job failed") # REPORT FAILED
    }
    
    Sys.sleep(time_offset)
    time_taken <- time_taken + time_offset
    print(paste0("Report ", st, " time taken: ", time_taken))
  } 
  
  print(paste0("Report complete: ", time_taken, " seconds taken"))
}

# CREATE REPORT ID
dataset <- "FF"
from_date <- "01-01-2025"
to_date <- "07-28-2025"  # TODAY'S DATE
frequency <- "Daily"
level  <- 0
categories <- list(13, 3) # ASSETS START, FLOW USD
assets <- "true"

# FILTERS IN THE FORM OF NAME, ASSET CLASS ID, BOND FILTER ID (CAN BE NULL)
filters <- list(list("GLOBEM", "FF223002", NULL), 
                list("HYIELD", "FF200000", "B100013"), 
                list("FLOATS", "FF200000", "B100016"), 
                list("USTRIN", "FF213002", "B100009"), 
                list("USTRLT", "FF213002", "B100008"), 
                list("USTRST", "FF213002", "B100010"), 
                list("CASH", "FF400000", NULL), 
                list("USMUNI", "FF213002", "B100019"), 
                list("GLOFIX", "FF212000", NULL))

result_multiAsset_FI_daily <- NULL

# FOR EACH ASSET CLASS FILTER COMBINATION
for (i in 1:length(filters)) {
  
  print(paste0("Calling data for: ", filters[[i]][[1]]))
  
  # CREATE THE REPORT ID
  report <- epfr.get.reportid(dataset,
                              from_date,
                              to_date,
                              list(filters[[i]][[2]]), # ASSET CLASS
                              frequency,
                              level,
                              categories,
                              NULL,
                              NULL,
                              assets,
                              NULL,
                              NULL,
                              if (is.null(filters[[i]][[3]])) NULL else list(filters[[i]][[3]]),
                              NULL) # BOND FILTER
  
  # WAIT FOR REPORT COMPLETION
  report_id <- report$data$reportId
  wait_for_completion(report_id, dataset, 20, 30)
  result_catagory <- epfr.get.reportoutput(dataset = "FF",  report_id = toString(report_id))
  
  # CALCULATE FLOW PERCENT
  restult_catagory_clean <-  result_catagory %>%
    mutate(FloPct = 100 *`Flow US$ mill` / `Total Net Assets Start`,
           Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    select(Date, FloPct) %>%
    rename(!!filters[[i]][[1]] := FloPct)
  
  # ADD DATA TO RESULT DATAFRAME
  if (is.null(result_multiAsset_FI_daily)) {
    result_multiAsset_FI_daily <- restult_catagory_clean
  } else {
    result_multiAsset_FI_daily <- result_multiAsset_FI_daily %>% inner_join(restult_catagory_clean, by = "Date")
  }
  
}

# SAVE DATA TO DISK
dir <- "path to your save directory"
write.csv(result_multiAsset_FI_daily, paste0(dir, "result_multiAsset_FI_daily.csv"), row.names = FALSE)
