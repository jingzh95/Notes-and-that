library(data.table, lib.loc = TSD_install_path)
library(haven, lib.loc = TSD_install_path)

library(tzdb, lib.loc = TSD_install_path)
library(readr, lib.loc = TSD_install_path)
df <- read_sav("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/Cause of death registry/DAARdata_220919_inkl2022.sav")
df <- data.table(df)
head(df)
# library(haven)
ref_date_2023 <- fread("/ess/p1921/data/durable/vac4eu/ref_date_2023.csv")
setnames(ref_date_2023, "lnr", "person_id")
table(df$DAAR)
# 2017  2018  2019  2020  2021  2022 
# 67 26961 40598 40589 41745 45969 
# extract from 2019

df <- df[DAAR>2018,]
names(df)
df <- df[,.(LOPENR, 
            DIFF_DAGER_DOD, 
            DIAGNOSE_UNDERLIGGENDE_K,
            TYPE_DIAGNOSE_KODEVERK_K)]

oldvars <- c("LOPENR", "DIFF_DAGER_DOD", "DIAGNOSE_UNDERLIGGENDE_K",
            "TYPE_DIAGNOSE_KODEVERK_K")
newvars <-  c("person_id", "start_date_record", "event_code", 
              "event_record_vocabulary")

setnames(df, oldvars, newvars)


unique(df$event_record_vocabulary)
table(df$event_record_vocabulary)
# 10     T_UKJENT 
# 4450   191410        2 
unique(df$event_code[df$event_record_vocabulary==""]) # ""
unique(df$event_code[df$event_record_vocabulary=="T_UKJENT"]) # ""
# we keep or remove individuals who don't have diagnose code?
# death is a death, so shall keep it
df$event_record_vocabulary[df$event_record_vocabulary==""] <- NA
df$event_record_vocabulary[df$event_record_vocabulary=="T_UKJENT"] <- NA
df$event_record_vocabulary[df$event_record_vocabulary=="10"] <- "ICD10"

df$event_code[df$event_code==""] <- NA
df <- df[!is.na(event_code)]
df <- df[!event_code==""]
df <- merge(df, ref_date_2023, all.x = TRUE)

df$start_date_record <- df$start_date_record + df$ref_date
summary(df$start_date_record)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2018-12-16" "2020-01-12" "2021-01-27" "2021-01-18" "2022-01-28" "2023-01-14" 
df$start_date_record <- gsub("-", "", as.character(df$start_date_record))
df$end_date_record <- NA
df$text_linked_to_event_code <- NA
df$event_free_text <- NA
df$present_on_admission <- NA
df$laterality_of_event <- NA
df$meaning_of_event <- "underlying_cause_of_death"
df$origin_of_event <- "DAR"
df$visit_occurrence_id <- NA
df$person_id <- as.character(df$person_id)


df<-  df[,.(person_id, 
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
gc()
df <- unique(df)

fwrite(df,"/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/EVENTS_DAR.csv", row.names = FALSE)
head(df)
gc()

setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
files <- list.files(pattern = ".csv")
ins <- fread(files[grepl("INS", files)])
chunk <- ins[source_table_name=="DAR"]
chunk$since_when_data_complete <- 
  min(as.numeric(df$start_date_record))

chunk$up_to_when_data_complete <- 
  max(as.numeric(df$start_date_record))

ins <- ins[!source_table_name=="DAR"]
ins <- rbind(ins, chunk)
fwrite(ins, files[grepl("INS", files)])
