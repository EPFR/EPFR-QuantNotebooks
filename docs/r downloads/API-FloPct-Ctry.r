#  INSTALL PACKAGES
# install.packages ("epfr", repos = "https://downloads.epfrglobal.com/sdks/R/", type = "source")   
library(epfr)
library(EPFR.r)
library(dplyr)
library(tidyr)

# LOGIN TO API
username <- "Your EPFR username"
password <- "Your EPFr password"

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

flow_to_api_date <- function(flow_date) {
  y <- substr(flow_date, 1, 4) # INPUT FORMAT "YYYYMMDD"
  m <- substr(flow_date, 5, 6)
  d <- substr(flow_date, 7, 8)
  return(paste0(m, "-", d, "-", y)) #OUTPUT FORMAT "MM-DD-YYYY"
}

chrdate_to_flowdate <- function(date) {
  split <- strsplit(date, split="/")[[1]] # INPUT FORMAT "M/D/YYYY"
  y <- split[3]
  m <- ifelse(nchar(split[1]) == 1, paste0("0", split[1]), split[1])
  d <- ifelse(nchar(split[2]) == 1, paste0("0", split[2]), split[2])
  return(paste0(y, m, d)) # OUTPUT FORMATT "YYYYMMDD"
}
chrdate_to_flowdate_vec <- Vectorize(chrdate_to_flowdate)

# GET COUNTRY ALLOCATIONS DATA
alloc_month <- "202505"
date <- flow_to_api_date(yyyymm.to.day(alloc_month))

all_acs <- as.list(epfr.get.dictionary.assetclasses("CA")$data %>%
                     filter(grepl("Equity", name)) %>% pull(id) %>% as.list()) # ALL AVALIABLE ASSET CLASSES

# CREATE THE COUNTRY ALLOCATIONS REPORT ID
report_ac <- epfr.get.reportid(dataset = "CA", 
                               from_date = date, 
                               to_date = date, 
                               asset_classes = all_acs, 
                               frequency =  "Monthly", 
                               level = 2, # FUND LEVEL
                               categories = list(85), # FUND ID
                               average_type = 1, 
                               allocation_ids = as.list(seq(1, 187)), # ALL COUNTRIES
                               assets = T, 
                               universal_filters = NULL,
                               equity_filters = NULL,
                               bond_filters = NULL,
                               alternative_filters = NULL)

# WAIT FOR REPORT COMPLETION
report_id_ac <- report_ac$data$reportId
wait_for_completion(report_id_ac, "CA", inital_time_offset = 20, time_offset = 10) 
# RETURN REPORT DATA
report_result_ca <- epfr.get.reportoutput("CA", report_id_ac) 

# CHANGE ALLOCATION % TO DECIMAL
curr_alloc_data <- report_result_ca %>%
  select(!c(Date, Fund, `Total Net Assets`, `Asset Class`, Filters)) %>%
  mutate(across(
    .cols = -c(FundID),
    .fns = ~ . / 100
  ))

# GET FLOW DATA
flow_dates <- flowdate.ex.AllocMo(alloc_month)
start_date_api <- flow_to_api_date(flow_dates[1]) 
end_date_api <- flow_to_api_date(flow_dates[length(flow_dates)])


# RETURN ALL XBORDER ASSET CLASSES
classif_geoid <- mat.read(x="read in file from ftp located at Classifications/classif-GeoId.txt", y="\t")
xborder <- classif_geoid %>%
  filter(xBord == 1) %>%
  select(ApiEquityId) %>%
  pull()

# CREATE THE FLOW REPORT ID
report_ff <- epfr.get.reportid(dataset = "FF", 
                               from_date = start_date_api, 
                               to_date = end_date_api,  
                               asset_classes = xborder, 
                               frequency =  "Daily", 
                               level = 2, 
                               categories = list(53, 3, 13), #  FUND ID, FLOW USD, NET ASSETS START 
                               average_type = NULL, 
                               allocation_ids = NULL, 
                               assets = NULL, 
                               universal_filters = NULL, 
                               equity_filters = NULL, 
                               bond_filters = NULL, 
                               alternative_filters = NULL)

# WAIT FOR REPORT COMPLETION
report_id_ff <- report_ff$data$reportId
wait_for_completion(report_id_ff, "FF", 30, 10)
# RETURN THE REPORT DATA
report_result_ff <- epfr.get.reportoutput("FF", report_id_ff)

# SELECT RELEVANT FLOW COLUMNS
curr_flow_data <- report_result_ff %>%
  select(c(Date, FundID,`Flow US$ mill`, `Total Net Assets Start`))

# SCALE NET ASSETS START
net_asset_start <- curr_alloc_data %>%
  mutate(FundID = as.character(FundID)) %>%
  inner_join(curr_flow_data, by="FundID") %>%
  mutate(across(
    .cols = -c(Date, FundID, `Total Net Assets Start`, `Flow US$ mill`),
    .fns = ~.* `Total Net Assets Start`)) %>%
  select(!c(FundID, `Total Net Assets Start`, `Flow US$ mill`)) %>% 
  group_by(Date) %>%
  summarise(across(everything(), sum))

# SCALE FLOW
flow <- curr_alloc_data %>%
  mutate(FundID = as.character(FundID)) %>%
  inner_join(curr_flow_data, by="FundID") %>%
  mutate(across(
    .cols = -c(Date, FundID, `Total Net Assets Start`, `Flow US$ mill`),
    .fns = ~.* `Flow US$ mill`)) %>%
  select(!c(FundID, `Total Net Assets Start`, `Flow US$ mill`)) %>% 
  group_by(Date) %>%
  summarise(across(everything(), sum))

# CALCULATE FLOW PERCENTAGE
flowpct <- net_asset_start %>%
  inner_join(flow, by="Date", suffix = c("_NAS", "_flow")) %>%
  mutate(across(
    .cols = ends_with("_flow"),
    .fns = ~. / get(sub("_flow$", "_NAS", cur_column())) * 100,
    .names = "{sub('_flow$', '', .col)}_flowpct")) %>%
  select(Date, ends_with("_flowpct")) %>%
  mutate(Date = chrdate_to_flowdate_vec(Date)) %>%
  arrange(Date) %>%
  tibble::column_to_rownames(var = "Date")

names(flowpct) <- sub("_flowpct$", "", names(flowpct))

# SAVE DATA TO DISK
dir <- "path to your save directory"
mat.write(flowpct, paste0(dir, "FlowCtryPct_", alloc_month ,".csv"))

