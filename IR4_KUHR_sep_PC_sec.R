library(data.table)
library(dplyr)
library(stringr)


setwd("N:/durable/vac4eu/CDMInstances/vac4eu/")
origin <- "KUHR"
files <- list.files(pattern = "KUHR")
sep_dir <- "N:/durable/vac4eu/CDMInstances/vac4eu_sep_kuhr/"

for (i in seq_along(files)){
  df <- fread(files[i])
  if (grepl("EVENTS", files[i])){ # meaning_of_event
    df_pc <- df[meaning_of_event == "primary_care"]
    df_other <- df[!meaning_of_event == "primary_care"]
    rm(df)
    gc()
    fwrite(df_pc, "EVENTS_KUHR_primary_care.csv")
    fwrite(df_other, paste0(sep_dir, "EVENTS_KUHR_other.csv"))
    rm(df_pc, df_other)
  }else{ # meaning_of_visit
    df_pc <- df[meaning_of_visit == "primary_care"]
    df_other <- df[!meaning_of_visit == "primary_care"]
    df_other[meaning_of_visit == "outpatient"]$meaning_of_visit <- 
      "outpatient_contact"
    rm(df)
    gc()
    fwrite(df_pc, "VISIT_OCCURRENCE_KUHR_primary_care.csv")
    fwrite(df_other, paste0(sep_dir, "VISIT_OCCURRENCE_KUHR_other.csv"))
    rm(df_pc, df_other)
    gc()
  }
}