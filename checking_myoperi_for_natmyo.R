TSD_install_path <- "/ess/p1921/data/durable/vac4eu/R_v4.3_packages_linux"
save_to <- "/ess/p1921/data/durable/vac4eu/"
library(data.table)
library(zoo)
library(dplyr, lib.loc = TSD_install_path)
library(tidyr, lib.loc = TSD_install_path)

## get the codelist
codelist <- fread("/ess/p1921/data/durable/vac4eu/Data characterisation/natmyo-main_20231108/Analysis-scripts/ALL_full_codelist.csv")

## subset the vocabulary
uosl <- codelist[coding_system == "ICD10" | coding_system == "ICPC2"]
uosl <- uosl[,.(event_definition,
                   event_abbreviation,
                   code,
                   coding_system)]

## get the relevant code
myo <- uosl[grepl("myocarditis", event_definition, ignore.case = T)]
peri <- uosl[grepl("pericarditis", event_definition, ignore.case = T)]

## check if they are ICD10 only or have both
table(myo$coding_system)
# ICD10 
# 17 
table(peri$coding_system)
# ICD10 
# 54 

## check if code-event is 1-1 or others
length(unique(myo$code)) # 15
length(unique(peri$code)) # 45

## put the code together
myo_peri <- rbind(myo, peri)

## check the total number of unique codes
length(unique(myo_peri$code)) # 58

## remove the dot
myo_peri$event_code <- gsub("\\.", "", myo_peri$code)

## set word directory to the CDM folder
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")

## extrac the cases
files <- list.files(pattern = "EVENTS") # list all EVENTS tables
for ( i in seq_along(files)){  # for each table
  df <- fread(files[i]) # load
  df <- df[event_record_vocabulary == "ICD10"] # get ICD10 only
  df <- df[,.(person_id,   # get the useful columns
              start_date_record,
              event_code)]
  gc()   # clean garbage
  case <- df[event_code %in%  # get the rows containing the event codes
               unique(myo_peri$event_code)]
  gc()  
  if (i == 1) case_all <- case  # put all rows together
  else case_all <- rbind(case, case_all)
}

# counting step-by-step by year-month
cases <- merge(case_all, myo_peri, by = "event_code", allow.cartesian = T)
cases$y_m <- paste0(substr(cases$start_date_record, 1,4),"-",
                    substr(cases$start_date_record, 5,6))
cases$y_m <- as.yearmon(cases$y_m)

abbr_counts_y_m <- cases[, counts := length(unique(person_id)),
                    by = list(y_m, event_abbreviation)]
abbr_counts_y_m <- abbr_counts_y_m[,.(event_abbreviation, y_m, counts)]
abbr_counts_y_m <- unique(abbr_counts_y_m)
abbr_counts_y_m <- pivot_wider(abbr_counts_y_m, 
                               names_from = y_m, 
                               values_from = counts)
setwd(save_to)
write.csv2(abbr_counts_y_m, "event_abbr_counts_by_ym.csv",
           row.names = F)
code_counts_y_m <- cases[, counts := length(unique(person_id)),
                     by = list(y_m, event_code)]

code_counts_y_m <- code_counts_y_m[,.(event_code, y_m, counts)]
code_counts_y_m <- unique(code_counts_y_m)
code_counts_y_m <- pivot_wider(code_counts_y_m, 
                               names_from = y_m, 
                               values_from = counts)

write.csv2(code_counts_y_m, "event_code_counts_by_ym.csv",
           row.names = F)
