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

##################################
# lopenr is ID
# oppholdLopenr is connecting key
##################################

# VO + PROS (AFTER VO PRODUCED)
VO <- list.files(pattern = "RENCE_NPR")
VO
for ( types in c("avt", "som", "tsb")){
  print(types)
  df <- fread(VO[grepl(paste0("RENCE_NPR_", types),
                       VO,ignore.case = TRUE)])
  
  df <- unique(df)
  
  df1 <- df[,.(visit_occurrence_id,
               visit_start_date)]
  
  gc()
  
  
  df0 <- fread(paste0(table_path, files[grepl(paste0("ATC_", types),
                                             files,
                                             ignore.case = TRUE)]))
  
  setnames(df0, c("lopenr", "oppholdlopenr", 
                 "kode"),
           c("person_id", "visit_occurrence_id", 
              "procedure_code"))
  df0 <- df0[!is.na(procedure_code)]
  df0 <- df0[!procedure_code==""]
  df0$procedure_code_vocabulary <- "NCMPNCSPNCRP"
  
  df0 <- merge(df0, df1, by = "visit_occurrence_id", all.x = TRUE)
  
  print(max(df0$visit_start_date, na.rm = T))
  # "AVT", 20230113
  # "SOM", 20230113
  # "TSB", 20221231
  
  head(df0)
  setnames(df0, "visit_start_date",
           "procedure_date")
  df0 <- df0[!is.na(procedure_date)]
  
  # vo_id <- unique(df0$visit_occurrence_id)
  
  df0$meaning_of_procedure <- "medicine_and_surgical_measures"
  df0$origin_of_procedure <- paste0("prosedyre_ATC_", types)
  df0 <- df0[,.(person_id,
                procedure_date,
                procedure_code,
                procedure_code_vocabulary,
                meaning_of_procedure,
                origin_of_procedure,
                visit_occurrence_id)]
  
  df0 <- unique(df0)
  
  fwrite(df0, paste0("PROCEDURES_", origin, "_", types, 
                     format))
  
  # 
  # vo_pros <- df[visit_occurrence_id %in% vo_id]
  # vo_pros$meaning_of_visit <- "procedure_during_hospitalisation"
  # fwrite(vo_pros, paste0("VISIT_OCCURRENCE_", origin, "_", types, 
  #                        "_pros",".csv"))
  
}
  






