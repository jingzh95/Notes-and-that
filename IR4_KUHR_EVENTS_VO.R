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


setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
raw_path <- "/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/"
origin <- "KUHR"
table_path <- paste0(raw_path, paste0(origin, "_complete"), "/")
files <- list.files(path = table_path, pattern = ".dsv")
for (i in 3:13 ){# seq_along(files)
  df <- fread(paste0(table_path, files[i]))
  yr <- str_split(files[i], " ")[[1]][1]
  df <- df[,.(PASIENTLOPENUMMER, # has no ERHOVEDKODE, 
              DIFFERANSEDAGER,
              DIAGNOSER,
              DIAGNOSEKODEVERK,
              FAGOMRAADE_KODE,
              PRAKSISTYPE)] #,
  
  # remove obs. without date
  df <- df[!is.na(DIFFERANSEDAGER)]
  df <- df[!DIFFERANSEDAGER == ""]
  df <- df[!is.na(DIAGNOSER)]
  df <- df[!DIAGNOSER == ""]
  # NCRPKODE, # have NCRPKODE, PROSEDYREKODER, OSV.
  # PROSEDYREKODER,
  # PROSEDYREKODEVERK
  df <- df %>% 
    distinct() %>% #kvinner: 84874605
    group_by(PASIENTLOPENUMMER, DIFFERANSEDAGER) %>% 
    mutate(visit_occurrence_id = paste0("KUHR_", yr,
                                        cur_group_id())) 
  
  
  df <- data.table(df)
  df <- merge(df, ref_date,
              by = "PASIENTLOPENUMMER",
              all.x = TRUE)
  df$DIFFERANSEDAGER <- df$DIFFERANSEDAGER + df$ref_date
  df$DIFFERANSEDAGER <- gsub("-", "", df$DIFFERANSEDAGER)
  
  # PROCEDURE
  # sum(df$NCRPKODE=="") # 19236
  # sum(df$DIAGNOSER=="") # 5358
  # sum(df$PROSEDYREKODER=="") # 19734
  # assign vo_id for same ID & same date for each
  ## add KUHR_DIAG_, KUHR_NCRP and KUHR_PROS as prefix resp.
  # dfp <- df[nchar(df$DIAGNOSER<=2)]
  
  # EVENTS AND VO
  df <- df[nchar(df$DIAGNOSER)>2] # the rest has NCRPKODE or so
  df <- df[,.(PASIENTLOPENUMMER, # has no ERHOVEDKODE
              DIFFERANSEDAGER,
              DIAGNOSER,
              DIAGNOSEKODEVERK,
              PRAKSISTYPE,
              FAGOMRAADE_KODE,
              visit_occurrence_id)]
  gc()
  df <- df[grepl("Fast|Lege|Syk|Tur", ignore.case=FALSE, df$PRAKSISTYPE)]
  # summary(df$DIFFERANSEDAGER)
  
  
  
  setnames(df, c("PASIENTLOPENUMMER", # has no ERHOVEDKODE, 
                 "DIFFERANSEDAGER",
                 "DIAGNOSER",
                 "DIAGNOSEKODEVERK"),
           c("person_id",
             "start_date_record",
             "event_code",
             "event_record_vocabulary"))
  df$event_record_vocabulary <- 
    gsub("-", "", df$event_record_vocabulary)
  df$event_record_vocabulary <- 
    gsub(" ", "", df$event_record_vocabulary)
  df$event_record_vocabulary <- 
    ifelse(grepl("ICD", df$event_record_vocabulary), 
           "ICD10", "ICPC")
  head(df)
  
  # separate those with one diagnostic code from many
  df1 <- df[!grepl(",",df$event_code)]
  
  df10 <- df[grepl(",",df$event_code)]
  
  # for those with more than 1, separate them
  
  df10 <- df10 %>% 
    separate_rows(event_code, sep = ",")
  df10$event_code <- gsub(" ", "", df10$event_code)
  head(df10)
  head(df1)
  
  df <- rbind(df1,df10)
  rm(df1,df10)
  gc()
  
  names(df)
  
  #dup1 <- sum(duplicated(df)==TRUE)
  
  # truncate 7 and 8 digits code into 3 and 4
  # by cutting off 4 digits from the tails
  df[nchar(event_code)>6]$event_code <- 
    substr(df[nchar(event_code)>6]$event_code, 1, 
           nchar(df[nchar(event_code)>6]$event_code)-4)
  
  # use PRAKSISTYPE to define "meaning_of_visit"
  df$meaning_of_visit<-"primary_care"
  # df$meaning_of_visit[grepl("Fast|Lege|Syk|Tur", ignore.case=FALSE, df$PRAKSISTYPE)==TRUE]<-"primary_care"
  # df$meaning_of_visit[grepl("Spesialist", ignore.case=FALSE, df$PRAKSISTYPE)==TRUE ]<-"secondary_care_specialist"
  # df$meaning_of_visit[grepl("rehab", ignore.case=FALSE, df$PRAKSISTYPE)==TRUE ]<-"secondary_med_rehab"
  # df$meaning_of_visit[grepl("Nevropsykolog", ignore.case=FALSE, df$PRAKSISTYPE)==TRUE ]<-"secondary_care_neuropsychologist"
  # df$meaning_of_visit[grepl("Psykolog", ignore.case=FALSE, df$PRAKSISTYPE)==TRUE ]<-"secondary_care_psychologist"
  # df$meaning_of_visit[grepl("Poliklinikk", ignore.case=FALSE, df$PRAKSISTYPE)==TRUE ]<-"outpatient"
  # df$meaning_of_visit[grepl("Ut", ignore.case=FALSE, df$PRAKSISTYPE)==TRUE ]<-"secondary_care"
  # df$meaning_of_visit[grepl("LabR", ignore.case=FALSE, df$PRAKSISTYPE)==TRUE ]<-"secondary_care_xray"
  
  
  # unique(df$PRAKSISTYPE[df$meaning_of_event=="primary_care"])
  # unique(df$PRAKSISTYPE[df$meaning_of_event=="secondary_care_specialist"])
  # unique(df$PRAKSISTYPE[df$meaning_of_event=="secondary_med_rehab"])
  # unique(df$PRAKSISTYPE[df$meaning_of_event=="secondary_care_neuropsychologist"])
  # unique(df$PRAKSISTYPE[df$meaning_of_event=="secondary_care_psychologist"])
  # unique(df$PRAKSISTYPE[df$meaning_of_event=="outpatient"])
  # unique(df$PRAKSISTYPE[df$meaning_of_event=="secondary_care"])
  # # unique(df$PRAKSISTYPE[df$meaning_of_event=="secondary_care_xray"])
  
  
  # change the vocabulary
  df[grepl("ICPC", df$DIAGNOSEKODEVERK)]$DIAGNOSEKODEVERK <- "ICPC"
  df[grepl("ICD", df$DIAGNOSEKODEVERK)]$DIAGNOSEKODEVERK <- "ICD10"
  # unique(df$DIAGNOSEKODEVERK)
  # "ICPC"                       "ICD10"                      "Akser i BUP-klassifikasjon"
  
  
  # unique(df$value[df$DIAGNOSEKODEVERK=="Akser i BUP-klassifikasjon"])
  # unique(nchar(df$value[df$DIAGNOSEKODEVERK=="ICD10"])) # 4 3 5
  # unique(nchar(df$value[df$DIAGNOSEKODEVERK=="ICPC"])) # 3 5 4
  
  
  
  
  # remove PRAKSISTYPE
  
  df <- df[, PRAKSISTYPE:=NULL]
  
  # add other variables in EVENTS
  
  df$end_date_record <- NA
  df$text_linked_to_event_code <- NA
  df$event_free_text <- NA
  df$present_on_admission <- NA
  df$laterality_of_event <- NA
  df$origin_of_event <- "KUHR"
  
  df$meaning_of_event <- df$meaning_of_visit
  df$meaning_of_event <- df$meaning_of_visit
  
  names(df)
  # add other variables in VISIT_OCCURRENCE
  
  df$visit_start_date <- df$start_date_record
  df$visit_end_date <- NA
  df$specialty_of_visit <- df$FAGOMRAADE_KODE
  df$specialty_of_visit_vocabulary <- "KUHR.FAGOMRAADE"
  df$status_at_discharge <- NA
  df$status_at_discharge_vocabulary <- NA
  df$origin_of_visit <- "KUHR"
  
  # head(df)
  
  # unique(df$event_code[(df$event_record_vocabulary=="ICPC")==TRUE])
  
  # get the diagnose code Angela applied for
  # to see if they are all here 
  # icpc_a <- read.csv("//tsd-evs/P1921/data/durable/vac4eu/kuhr_icpc_a.csv")
  # icpc_a <- icpc_a$Codes.ICPC
  # icd10_a <- read.csv("//tsd-evs/P1921/data/durable/vac4eu/kuhr_icd10_a.csv")
  # icd10_a <- icd10_a$Codes.ICD10
  
  
  # df_icpc <- subset(df, df$event_record_vocabulary=="ICPC")
  # df_icd10 <- subset(df, df$event_record_vocabulary=="ICD10")
  #df_bup <- subset(df, df$event_record_vocabulary=="Akser i BUP-klassifikasjon")
  
  # rm(df)
  
  # no need to do remove but the dataset is much bigger than ..
  
  # df_icpc <- subset(df_icpc, (df_icpc$event_code %in% icpc_a)==TRUE)
  # df_icd10 <- subset(df_icd10, (df_icd10$event_code %in% icd10_a)==TRUE)
  
  # want to check if ICPC or ICD10 contains "specialist_med_rehab"
  
  # unique(df_icd10$meaning_of_event)
  # unique(df_icpc$meaning_of_event)
  # unique(df_bup$meaning_of_event)
  
  
  
  # create an event table for BUP as mental disease covariates
  
  #df_bup <- df_bup %>% 
  #select(person_id, start_date_record, end_date_record,
  #      event_code, event_record_vocabulary, text_linked_to_event_code,
  #     event_free_text, present_on_admission, laterality_of_event,
  #    meaning_of_event, origin_of_event, visit_occurrence_id)
  
  # might need to eliminate more event_code 
  
  #fwrite(df_bup, "//tsd-evs/P1921/data/durable/vac4eu/CDMInstances/vac4eu/EVENTS_BUP_2016.csv", row.names = FALSE)
  
  # save the event_code for possible review later
  #akse <- unique(df_bup$event_code) 
  #icd10 <- unique(df_icd10$event_code)
  ## how to write these diagnose code out?
  
  #rm(df_bup)
  
  # rbind ICPC and ICD10 to create EVENTS and V_O
  
  # df <- rbind(df_icpc,df_icd10,df_bup)
  
  # df <- rbind(df_icpc,df_icd10)
  
  # save the diagnose code to check after run for 5 years
  # kuhr_icpc_2016 <- unique(df_icpc$event_code) 
  # kuhr_icd10_2016 <- unique(df_icd10$event_code) 
  # 
  # rm(df_icpc,df_icd10)
  
  
  df_event <-  df %>% 
    select(person_id, start_date_record, end_date_record,
           event_code, event_record_vocabulary, text_linked_to_event_code,
           event_free_text, present_on_admission, laterality_of_event,
           meaning_of_event, origin_of_event, visit_occurrence_id)
  
  df_event <- df_event %>% distinct()
  
  df_vo <-  df %>% 
    select(person_id, visit_start_date, visit_end_date,
           specialty_of_visit, specialty_of_visit_vocabulary,
           status_at_discharge, status_at_discharge_vocabulary,
           meaning_of_visit, origin_of_visit, visit_occurrence_id)
  df_vo <- df_vo %>% distinct()
  
  
  fwrite(df_event,  paste0("EVENTS_KUHR_", yr, ".csv"), row.names = FALSE )
  rm(df_event)
  gc()
  
  # Only keep distinct rows (ca. 5% duplicate)
  fwrite(df_vo,  paste0("VISIT_OCCURRENCE_KUHR_", yr, ".csv"), row.names = FALSE )
  rm(df_vo)
  gc()
  
  rm(df)
  gc()
  
}
  
