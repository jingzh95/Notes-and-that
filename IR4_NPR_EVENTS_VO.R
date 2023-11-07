TSD_install_path <- "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux"
library(data.table)
ref_date <- fread("/ess/p1921/data/durable/vac4eu/ref_date_2023.csv")
# get dates
setnames(ref_date, "lnr", "PASIENTLOPENUMMER")
library(ellipsis, lib.loc = TSD_install_path)
library(withr, lib.loc = TSD_install_path)
library(dplyr, lib.loc = TSD_install_path)
library(vctrs, lib.loc = TSD_install_path)
library(stringr, lib.loc = TSD_install_path)
library(tidyr, lib.loc = TSD_install_path)
library(reshape2, lib.loc = TSD_install_path)

ref_date <- fread("/ess/p1921/data/durable/vac4eu/ref_date_2023.csv")
setnames(ref_date, "lnr", "person_id")
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
raw_path <- "/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/"
origin <- "NPR"
format <- ".csv"
table_path <- paste0(raw_path, origin, "/")
files <- list.files(path = table_path, pattern = format)
files
# [1] "22_8877_Aktivitetsfil_AVT.csv"  "22_8877_Aktivitetsfil_SOM.csv"  "22_8877_Aktivitetsfil_TSB.csv"  "22_8877_prosedyre_ATC_avt.csv"  "22_8877_prosedyre_ATC_som.csv" 
# [6] "22_8877_prosedyre_ATC_tsb.csv"  "22_8877_tilstandskoder_avt.csv" "22_8877_Tilstandskoder_som.csv" "22_8877_tilstandskoder_tsb.csv"


# df1 <- fread(paste0(table_path, files[i]))

# checking which of oppholdLopenr and lopenr is ID via gender info in national registry 
# nr <- fread("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/National registry/Uttrekk Resten.csv")
# head(nr)
# setnames(nr, "koblingsnoekkel", "oppholdLopenr" )
# 
# df0 <- merge(df1, nr, by = "oppholdLopenr", all.x = TRUE)
# df0[1:6]$kjonn_navn_folkeregistrert
# df0[1:6]$kjoenn
# df0[, kjoen/ess/p1921/data=NULL]
# df0[,c("foedselsaar",
#        "foedested",
#        "statsborgerskap"):=NULL]
# setnames(nr, "oppholdLopenr", "lopenr")
# df0 <- merge(df1, nr, by = "lopenr", all.x = TRUE)
# head(df0)
# df0[1:6]$kjonn_navn_folkeregistrert
# df0[1:6]$kjoenn


##################################
# lopenr is ID
# oppholdLopenr is connecting key
##################################

types = "AVT"
types = "SOM"
types = "TSB"


##################################
# lopenr is ID
# oppholdLopenr is connecting key
##################################
# types = "AVT"
# VO + EVENTS
for ( types in c("AVT", "SOM", "TSB")){
  print(types)
  df <- fread(paste0(table_path, files[grepl(paste0("AKTIVITETSFIL_", types),
                                              files,
                                              ignore.case = TRUE)]))
  
  df <- df[,.(oppholdLopenr,
                lopenr,
                differansedager_InnDato,
                differansedager_UtDato,
                omsorgsniva)]
  gc()
  
  df$meaning_of_visit <- ""
  df[omsorgsniva==1]$meaning_of_visit <- "hospitalisation"
  df[omsorgsniva==2]$meaning_of_visit <- "hospitalisation_not_overnight"
  df[omsorgsniva==3]$meaning_of_visit <- "outpatient_contact"
  df[omsorgsniva==0]$meaning_of_visit <- "hospital_encounter"
  df[meaning_of_visit==""]$meaning_of_visit <- "hospital_encounter"
  df[is.na(meaning_of_visit)]$meaning_of_visit <- "hospital_encounter"
  
  # table(df$meaning_of_visit)
  sum(is.na(df$meaning_of_visit)) # 0
  sum(df$meaning_of_visit=="") # 0
  
  setnames(df, c("lopenr",
                  "oppholdLopenr",
                  "differansedager_InnDato",
                  "differansedager_UtDato"),
           c("person_id",
             "visit_occurrence_id",
             "visit_start_date",
             "visit_end_date"))
  
  sum(is.na(df$visit_start_date))
  sum(is.na(df$visit_end_date)) # 262
  df <- df[!is.na(visit_start_date)]
  
  
  df <- merge(df, ref_date, by = "person_id", all.x = TRUE)
  df$visit_start_date <- df$ref_date + df$visit_start_date
  df <- df[!is.na(visit_start_date)]
  
  summary(df$visit_start_date)
  df <- df[visit_start_date <= as.Date("2023-01-14")]
  df <- df[visit_start_date >= as.Date("2009-12-16")]
  df$visit_start_date <- gsub("-", "", df$visit_start_date)
  
  df$visit_end_date <- df$ref_date + df$visit_end_date
  df <- df[visit_end_date <= as.Date("2023-01-14") |
             is.na(visit_end_date)]
  df <- df[visit_end_date >= as.Date("2009-12-16")]
  df$visit_end_date <- gsub("-", "", df$visit_end_date)
  
  df$specialty_of_visit <- NA
  df$specialty_of_visit_vocabulary <- NA
  df$status_at_discharge <- NA
  df$status_at_discharge_vocabulary <- NA
  
  df$origin_of_visit <- paste0("Akt_", types)
  
  df <- df[,.(person_id,
                visit_occurrence_id,
                visit_start_date,
                visit_end_date,
                specialty_of_visit,
                specialty_of_visit_vocabulary,
                status_at_discharge,
                status_at_discharge_vocabulary,
                meaning_of_visit,
                origin_of_visit)]
  df <- unique(df)
  vo_row_0 <- nrow(df)
  vo_row_1 <- ifelse(types == "AVT",
                     vo_row_0,
                     vo_row_0 + vo_row_1)
  fwrite(df, paste0("VISIT_OCCURRENCE_", origin, "_", types, ".csv"))
  
  
  
  ##############################################
  ##############################################
  ## EVENTS ####################################
  ##############################################
  df1 <-df[,.(visit_occurrence_id,
              visit_start_date,
              visit_end_date,
              meaning_of_visit)]
  gc()

  df <- fread(paste0(table_path, files[grepl(paste0("TILSTANDSKODER_", types),
                                              files,
                                              ignore.case = TRUE)]))
  df <- df[!is.na(kode)]
  df <- df[!kode==""]
  setnames(df, c("lopenr", "oppholdlopenr",
                  "kodenavn", "kode"),
           c("person_id", "visit_occurrence_id",
             "event_record_vocabulary", "event_code"))
  df$event_record_vocabulary <- gsub("-", "", df$event_record_vocabulary)
  str(df)
  head(df[event_record_vocabulary=="ICPC2B"])
  df <- merge(df, df1, by = "visit_occurrence_id", all.x = TRUE)
  head(df)
  setnames(df, c("visit_start_date",
                  "visit_end_date",
                  "meaning_of_visit"),
           c("start_date_record",
             "end_date_record",
             "meaning_of_event"))

  df$meaning_of_event <- ifelse(df$Hovedtilstand == 1,
                                 paste0(df$meaning_of_event, "_main_diagnosis"),
                                 paste0(df$meaning_of_event, "_secondary_diagnosis"))
  
  df[grepl("NA", meaning_of_event)]$meaning_of_event <- 
    gsub("NA_", "hospital_encounter_", 
         df[grepl("NA", meaning_of_event)]$meaning_of_event)
  df$text_linked_to_event_code <- NA
  df$event_free_text <- NA
  df$present_on_admission <- NA
  df$laterality_of_event <- NA
  df$origin_of_event <- paste0("Tilstandskoder_", types)

  df <- df[,.(person_id,
              start_date_record,
              end_date_record,
              event_code,
              event_record_vocabulary,
              text_linked_to_event_code,
              event_free_text,
              present_on_admission,
              laterality_of_event,
              meaning_of_event,
              origin_of_event,
              visit_occurrence_id)]
  str(df)
  df <- unique(df)
  
  ev_row_0 <- nrow(df)
  ev_row_1 <- ifelse(types == "AVT",
                     ev_row_0,
                     ev_row_0 + ev_row_1)
  fwrite(df, paste0("EVENTS_", origin, "_", types, ".csv"))
  rm(df)
  gc()
}

# source("/ess/p1921/data/durable/vac4eu/scripts/IR4_NPR_pros.R")  
