#  INSTALL PACKAGES
# install.packages ("epfr", repos = "https://downloads.epfrglobal.com/sdks/R/", type = "source") 
library(epfr)
library(dplyr)

# LOGIN TO API
username <- "your EPFR username"
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

# CREATE THE REPORT ID
dataset <- "FF"  
from_date <- "01-01-2025"  
to_date <- "07-28-2025" # TODAYS DATE

asset_classes <- list("FF121000", # ASIA EX JAPAN
                      "FF114004", # EUROPE EX GB
                      "FF111003", # JAPAN
                      "FF124000", # LATAM
                      "FF114017", # UK
                      "FF113002", # USA
                      "FF111001", # AUSTRALIA
                      "FF111002", # HONG KONG
                      "FF111004", # PACIFIC REGIONAL
                      "FF111005", # SINGAPORE
                      "FF111006"  # NEW ZELAND
                      
)

frequency <- "Daily"  
level  <- 0  # AGGREGATE
categories <- list(13, 3) # ASSETS START, FLOW USD
assets <- "true"  
universal_filters <- list("U100005") # ACTIVE FUNDS
equity_filters <- NULL

report <- epfr.get.reportid(dataset, from_date, to_date, asset_classes, frequency, level,
                            categories, NULL, NULL, assets, universal_filters, equity_filters, NULL, NULL)

# WAIT FOR REPORT TO COMPLETE
report_id <- report$data$reportId
wait_for_completion(report_id, "FF", 30, 30)
# RETURN RESULTING DATA
result <- epfr.get.reportoutput(dataset, report_id)

# TRANSFORM DATA TO DESIERED REGIONS
result_multiAsset_Rgn_daily <- result %>%
  mutate(Ctry = recode(`Asset Class`,
                       "Asia Ex-Japan-FF-Equity" = "AsiaXJP",
                       "Europe ex-UK Regional-Western Europe-FF-Equity" = "EurXGB",
                       "Japan-Asia Pacific-FF-Equity" = "Japan",
                       "LatAm-FF-Equity" = "LatAm",
                       "United Kingdom-Western Europe-FF-Equity" = "UK",
                       "USA-North America-FF-Equity" = "USA",
                       "Australia-Asia Pacific-FF-Equity" = "PacXJP",
                       "Hong Kong Special Administrative Region of China-Asia Pacific-FF-Equity" = "PacXJP",
                       "Singapore-Asia Pacific-FF-Equity" = "PacXJP",
                       "Pacific Regional-Asia Pacific-FF-Equity" = "PacXJP",
                       "New Zealand-Asia Pacific-FF-Equity" = "PacXJP",
                       .default = `Asset Class`) 
  ) %>%
  group_by(Date, Ctry) %>% 
  summarise(`Flow US$ mill` = sum(`Flow US$ mill`), 
            `Total Net Assets Start` = sum(`Total Net Assets Start`)) %>%
  mutate(floPct = 100 *`Flow US$ mill` / `Total Net Assets Start`) %>%
  select(Date, Ctry, floPct) %>% 
  pivot_wider(names_from = Ctry, values_from = floPct) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  arrange(Date)

# SAVE DATA TO DISK
dir <- "path to your save directory"
write.csv(result_multiAsset_Rgn_daily, paste0(dir, "MultiAsset-Rgn-daily-API.csv"), row.names = FALSE)