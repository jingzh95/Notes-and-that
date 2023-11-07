TSD_install_path <- 
  ifelse(toupper(Sys.info())["sysname"]=="LINUX",
         "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux/",
         "N:/durable/vac4eu/R_v4.1_packages/")
bla <- 
  ifelse(toupper(Sys.info())["sysname"]=="LINUX",
         "/ess/p1921/data",
         "N:")
library(data.table)
if (toupper(Sys.info())["sysname"]=="LINUX"){
  library(stringr, lib.loc = TSD_install_path)
}else{library(stringr)}

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
# 29082023 notes
# investigate the BARN with missing IDs.
library(haven, lib.loc = TSD_install_path)
library(readr, lib.loc = TSD_install_path)
library(tzdb, lib.loc = TSD_install_path)

# 2023.10.09
meta <- fread("METADATA.csv")
meta[type_of_metadata == "list_of_values" &
       tablename == "PERSONS" &
       columnname == "sex_at_instance_creation"]$values <- "M F U O"
fwrite(meta, "METADATA.csv")

df <- fread("OBSERVATION_PERIODS_SSB.csv")
table(df$op_meaning)
# child_from_birth_registry_not_in_population_registry 
# 34 
# legal_resident 
# 5841073 

df[op_meaning == "child_from_birth_registry_not_in_population_registry"]
df$op_meaning <- "legal_resident"
fwrite(df, "OBSERVATION_PERIODS_SSB.csv")

df1 <- fread("OBSERVATION_PERIODS_MBRN.csv")
summary(df1$person_id)


# child_from_birth_registry_not_in_population_registry 
# 34 
# legal_resident 
# 5841073 

# 2023.10.02
# Matters
## check if MSIS contain other than 713
msis <- fread("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/MSIS/PDB3198_MSIS-data_2023-03.csv")
table(msis$DiagnoseNr)
# 713 
# 1048575 
all_tables <- list.files(pattern = ".csv")
# checking MEDICINES
tablename <- "MEDICINES"
tables <- all_tables[grepl(tablename, all_tables)]
df <- fread(tables[1])
str(df)
## the format of "disp_number_medicinal_product" is "num" which is fine.


# change origins with underscore
tablename <- "EVENTS_Tilstandskoder"
tables <- all_tables[grepl(tablename, all_tables)]
for (i in seq_along(tables)){
  df <- fread(tables[i])
  origin <- substr(tables[i], 8, 25)
  df$origin_of_event <- origin
  fwrite(df, tables[i])
}


tablename <- "RRENCE_Akt"
tables <- all_tables[grepl(tablename, all_tables)]
for (i in seq_along(tables)){
  df <- fread(tables[i])
  origin <- substr(tables[i], 18, 24)
  df$origin_of_visit <- origin
  fwrite(df, tables[i])
}

tablename <- "_ATC_"
tables <- all_tables[grepl(tablename, all_tables)]
for (i in seq_along(tables)){
  df <- fread(tables[i])
  origin <- substr(tables[i], 12, 28)
  df$origin_of_procedure <- origin
  df$procedure_code_vocabulary <- gsub("-", "", df$procedure_code_vocabulary)
  print(unique(df$origin_of_procedure))
  print(unique(df$procedure_code_vocabulary))
  fwrite(df, tables[i])
}

# two tables for PERSONS and OP
# from previous work, the MBRN children has ID>5 900 000

tablename <- "PERSONS"
tables <- all_tables[grepl(tablename, all_tables)]
df <- fread(tables, colClasses = "character")
str(df)
summary(as.integer(df$person_id))
# Min. 1st Qu.  Median files 3rd Qu.    Max. 
#    0 1456346 2912668 2912819 4368981 5910788 
p_FASTE_OPPL <- df[as.integer(df$person_id)<5900000]
table(p_FASTE_OPPL$sex_at_instance_creation)
fwrite(p_FASTE_OPPL, "PERSONS_FASTE_OPPL.csv")
p_MBRN <- df[as.integer(df$person_id)>=5900000] # 10788 observations
table(p_MBRN$sex_at_instance_creation)
fwrite(p_MBRN, "PERSONS_MBRN.csv")


df <- fread("OBSERVATION_PERIODS.csv", colClasses = "character")
str(df)
summary(as.integer(df$person_id))
op_SSB <- df[as.integer(df$person_id)<5900000]
fwrite(op_SSB, "OBSERVATION_PERIODS_SSB.csv")
op_MBRN <- df[as.integer(df$person_id)>=5900000]
nrow(op_MBRN[as.integer(op_MBRN$op_start_date)==as.integer(op_MBRN$op_end_date)])
op_MBRN$op_origin <- "MBRN"
fwrite(op_MBRN, "OBSERVATION_PERIODS_MBRN.csv")

so <- fread(all_tables[grepl("VEY_OBSERVATIONS_CHILDREN", all_tables)])
table(so$so_source_column)


ins <- fread("INSTANCE.csv")
ins[source_table_name=="MBRN"]$up_to_when_data_complete <- max(up_to_when_data_complete)
ins[source_table_name=="MBRN"]$since_when_data_complete <- min(ins$since_when_data_complete)
ins$restriction_in_values <- "no"
ins[source_table_name == "MBRN"]
add_row <- data.table(source_table_name = "MBRN",
                      source_column_name = "HYPERTENSJON_ALENE",
                      included_in_instance = "yes",
                      date_when_data_last_updated = 20230505,
                      since_when_data_complete = 20091216,
                      up_to_when_data_complete = 20230114,
                      restriction_in_values = "no",
                      list_of_values = NA,
                      restriction_condition = NA)
ins <- rbind(ins, add_row)
ins[source_table_name == "MSIS" & source_column_name == "Pr\xf8vedatodiff"]$source_column_name <- "Proevedatodiff"
View(ins)
fwrite(ins, "INSTANCE.csv")

meta <- fread("METADATA.csv")
cdm <- fread("CDM_SOURCE.csv")
cdm$cdm_version <- "01"
cdm$etl_link <- "to be included when available"
cdm$cdm_vocabulary_version <- "v2.2vac4eu"
cdm$instance_number <- 2
cdm$date_creation <- "20230501"
fwrite(cdm, "CDM_SOURCE.csv")


meta <- fread("METADATA.csv")
meta[type_of_metadata == "list_of_values" &
       tablename == "CDM_SOURCE" &
       columnname == "cdm_vocabulary_version"]$values <- "v2.2vac4eu"
fwrite(meta, "METADATA.csv")



df <- fread("PERSON_RELATIONSHIPS.csv")
sum(is.na(df$related_id)) # 1737
id <- df[is.na(df$related_id)]$person_id
length(unique(id))

si <- fread("SURVEY_ID.csv")

raw <- read_sav("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/MBRN/p222359_mfr.sav")
raw <- data.table(raw)
sum(is.na(raw$ID_MOR)) #0
sum(raw$ID_MOR=="") # 4804
sum(raw$FDATO_DIFFERANSEDAGER_MOR=="") #NA
sum(is.na(raw$FDATO_DIFFERANSEDAGER_MOR)) # 4804
sum(is.na(raw$ID_BARN))
sum(raw$ID_BARN=="") # 13781
sum(raw[raw$ID_BARN==""]$ID_MOR == "") # 3607
missing_both <- raw[ID_BARN=="" & ID_MOR == ""]
missing_mor <- raw[!ID_BARN=="" & ID_MOR == ""]
missing_barn <- raw[ID_BARN=="" & !ID_MOR == ""]

table(missing_barn$APGAR5)
table(missing_barn$APGAR10)
table(missing_barn$DODKAT)
table(missing_both$DODKAT)

# anyway
df <- fread("SURVEY_OBSERVATIONS_MOTHER.csv")
sum(is.na(df$person_id)) # 0
sum(df$person_id=="") # 0
df <- fread("SURVEY_OBSERVATIONS_CHILDREN.csv")
sum(is.na(df$person_id)) # 227010 
head(df[is.na(df$person_id)])
length(unique(df[is.na(person_id)]$survey_id)) # 10592


sid <- fread("SURVEY_ID.csv")
sum(is.na(sid$person_id)) # 10592


# 01082023 notes
# for METADATA vocabulary variables:
meta <- fread("METADATA.csv")
########################################################
########################################################
########################################################
########################################################
## 1 remove unused mo_record_vocabulary from METADATA 
meta[type_of_metadata == "list_of_values" &
       tablename == "MEDICAL_OBSERVATIONS" &
       columnname == "mo_record_vocabulary"]$values

meta[type_of_metadata == "presence_of_column" &
       tablename == "MEDICINES" &
       columnname == "visit_occurrence_id"]$values <- "No"

meta[type_of_metadata == "list_of_values" &
       tablename == "OBSERVATION_PERIODS" &
       columnname == "op_meaning"]$values <- "legal_resident child_from_birth_registry_not_in_population_registry"
fwrite(meta, "METADATA.csv")
# "NO_lab_coding_system  NO_ssb_coding_system EU_equivalent_scale NO_occupation_code_list NO_civil_status"

files <- list.files(pattern = "MEDICAL_OB")
for ( i in seq_along(files)){
  df <- fread(files[i], nrow=1)
  v0 <- df$mo_record_vocabulary
  if (i == 1){v <- v0
  }else{v <- paste(v0, v, collapse = " ")}
}
v
# "NA NA NA NO_lab_coding_system"
meta[type_of_metadata == "list_of_values" &
       tablename == "MEDICAL_OBSERVATIONS" &
       columnname == "mo_record_vocabulary"]$values <- "NO_lab_coding_system" 


## 2 remove SSB from METADATA for mo_source_table
meta[type_of_metadata == "list_of_values" &
       tablename == "MEDICAL_OBSERVATIONS" &
       columnname == "mo_source_table"]$values
# "MSIS SSB INNT SIVILSTAND BOSTEDSFYLKE"
meta[type_of_metadata == "list_of_values" &
       tablename == "MEDICAL_OBSERVATIONS" &
       columnname == "mo_source_table"]$values <- "MSIS INNT SIVILSTAND BOSTEDSFYLKE"

## 3 remove EURO from METADATA for mo_unit
meta[type_of_metadata == "list_of_values" &
       tablename == "MEDICAL_OBSERVATIONS" &
       columnname == "mo_unit"]$values <- "NOK" 

## 4 remove FDATO_DIFFERENSEDAGER for so_source_column
v <- unique(fread("SURVEY_OBSERVATIONS.csv")$so_source_column)
v <- paste(v, collapse = " ")
v
meta[type_of_metadata == "list_of_values" &
       tablename == "SURVEY_OBSERVATIONS" &
       columnname == "so_source_column"]$values <- "SVLEN_DG FDATO_DIFFERANSEDAGER_MOR PREEKL DIABETES_MELLITUS BLODN_F13 BLODN_13_28 BLODN_E28 BLODNING_O500 APGAR5 ZSCORE_BW_GA VEKT DODKAT KMI_FOER MOR_ROYKTE_FOER_SVSK KSNITT KSNITT_PLANLAGT DODFODTE_5 PLACENTA_PREVIA MISD NEVRALRORSDEFEKTER SPINAB ENCEPH ANENCEPH LEPPE_LEPPEGANESPALTE GANESPALTE HJERTE_MISD GASTROS OMPHALO KLUMPFOT KROMOSOMFEIL DOWNS BARNETS_HELSE PARITET_5 HYPERTENSJON_ALENE"

v <- unique(ins$source_table_name)
v <- paste(v, collapse = " ")
meta[type_of_metadata == "list_of_values" &
       tablename == "INSTANCE" &
       columnname == "source_table_name"]$values <- v


meta[type_of_metadata == "list_of_values" &
       tablename == "INSTANCE" &
       columnname == "source_table_name"]$values <- gsub("UTVANDRING", "SSB", meta[type_of_metadata == "list_of_values" &
                                                               tablename == "INSTANCE" &
                                                               columnname == "source_table_name"]$values)
meta[type_of_metadata == "list_of_values" &
       tablename == "INSTANCE" &
       columnname == "restriction_in_values"]$values <- "no"

meta[type_of_metadata == "list_of_values" &
       tablename == "EVENTS" &
       columnname == "meaning_of_event"]$values <- "primary_care underlying_cause_of_death hospitalisation_main_diagnosis  hospitalisation_secondary_diagnosis hospitalisation_not_overnight_main_diagnosis hospitalisation_not_overnight_secondary_diagnosis outpatient_contact_main_diagnosis outpatient_contact_secondary_diagnosis hospital_encounter_main_diagnosis hospital_encounter_secondary_diagnosis"

meta[type_of_metadata == "list_of_values" &
       tablename == "VISIT_OCCURRENCE" &
       columnname == "meaning_of_visit"]$values <- "primary_care outpatient_contact hospitalisation hospitalisation_not_overnight hospital_encounter"




meta[tablename == "PERSONS" &
       columnname == "sex_at_instance_creation"]$values <- "M F U Unsure Unknown Unspecified" # for imputed date

meta[type_of_metadata == "list_of_values" &
       tablename == "CDM_SOURCE" &
       columnname == "cdm_vocabulary_version"]$values <- "v2.2vac4eu"

ins[source_table_name=="UTVANDRING"]$source_table_name <- "SSB"
ins <- ins[!source_column_name=="FDATO_DIFFERANSEDAGER_BARN"] # remove FDATO_DIFFERANSEDAGER_BARN

meta[tablename == "PROCEDURES" &
       columnname == "procedure_code_vocabulary"]$values <- "NCMPNCSPNCRP"




## 5 vx_manufacturer & vx_type clean in METADATA
v <- unique(fread("VACCINES.csv")$vx_manufacturer)
v <- paste(v, collapse = " ")
v
meta[type_of_metadata == "list_of_values" &
       tablename == "VACCINES" &
       columnname == "vx_manufacturer"]$values <- v


v <- unique(fread("VACCINES.csv")$vx_type)
v <- paste(v, collapse = " ")
v
meta[type_of_metadata == "list_of_values" &
       tablename == "VACCINES" &
       columnname == "vx_type"]$values <- v

## 6 aargang matter
meta[type_of_metadata == "list_of_values" &
       tablename == "MEDICAL_OBSERVATIONS" &
       columnname == "mo_source_column"]$values

files <- list.files(pattern = "MEDICAL_OB")
for ( i in seq_along(files) ){
  df <- fread(files[i])
  v0 <- unique(df$mo_source_column)
  if (i == 1){v <- v0
  }else{v <- paste(unique(union(v0, v)), collapse = " ")}
}
v
meta[type_of_metadata == "list_of_values" &
       tablename == "MEDICAL_OBSERVATIONS" &
       columnname == "mo_source_column"]$values <- v


meta[type_of_metadata == "list_of_values" &
       tablename == "MEDICAL_OBSERVATIONS" &
       columnname == "mo_meaning"]$values

meta[type_of_metadata == "list_of_values" &
       tablename == "EVENTS" &
       columnname == "meaning_of_event"]$values <- "primary_care underlying_cause_of_death hospitalisation_main_diagnosis  hospitalisation_secondary_diagnosis hospitalisation_not_overnight_main_diagnosis hospitalisation_not_overnight_secondary_diagnosis outpatient_contact_main_diagnosis outpatient_contact_secondary_diagnosis hospital_encounter_main_diagnosis hospital_encounter_secondary_diagnosis"
meta[type_of_metadata == "list_of_values" &
       tablename == "VISIT_OCCURRENCE" &
       columnname == "meaning_of_visit"]$values <- "primary_care outpatient_contact hospitalisation hospitalisation_not_overnight hospital_encounter"
fwrite(meta, "METADATA.csv")
########################################################
########################################################
########################################################
########################################################


# for INSTANCE
ins <- fread("INSTANCE.csv")
max_end <- min(ins$since_when_data_complete)
########################################################
########################################################
########################################################
########################################################
## 7 double check INSTANCE and CDM date

## 8 change 20220505 to 20230505
ins$date_when_data_last_updated <- 20230505
fwrite(ins, "INSTANCE.csv")
## 9* update sivilstant and bostedsfylke



## up_to_... use 2023-01-14 shortest or longest or keep it the way it is

########################################################
########################################################
########################################################
########################################################

# for OP
########################################################
########################################################
########################################################
########################################################
## checking for op_end_date > op_start_date
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
df <- fread("OBSERVATION_PERIODS.csv")

nrow(df[duplicated(person_id)])
dup_op <- df[duplicated(person_id)]
summary(dup_op[!is.na(op_end_date)]$op_end_date-dup_op[!is.na(op_end_date)]$op_start_date)
sum(is.na(dup_op$op_end_date))
nrow(df[op_end_date < op_start_date])



## remove obs have op_end_date before 2010 (or 20091214)
### for 100k sample then for full
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_100k/"))
op <- fread("OBSERVATION_PERIODS.csv")
summary(op$op_start_date)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 19171115 19650715 19900915 19881603 20090815 20230111 
summary(op$op_end_date)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 19960121 20230114 20230114 20228198 20230114 20230114 
op <- op[!op_end_date < max_end]
fwrite(op, "OBSERVATION_PERIODS.csv")


## check the outside OP
## primary_care 746
library(dplyr)
op_1 <- op[!duplicated(person_id)]
op_2 <- op[duplicated(person_id)]

  
tb <- "EVENTS" 
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  df <- df[,.(person_id,
              start_date_record,
              meaning_of_event)]
  df_op1 <- merge(df, op_1, by = "person_id")
  df_op2 <- merge(df, op_2, by = "person_id")
  df_op_out_1 <- df_op1[start_date_record <= op_start_date|start_date_record >= op_end_date]
  df_op_out_1 <- df_op_out_1[,.(person_id,
                            start_date_record,
                            meaning_of_event)]
  df_op_in_2 <- df_op2[start_date_record >= op_start_date|start_date_record <= op_end_date]
  df_op_in_2 <- df_op_in_2[,.(person_id,
                              start_date_record,
                              meaning_of_event)]
  df_op_out_0 <- setdiff(df_op_out_1, df_op_in_2) # 
  if (i == 1){df_op_out <- df_op_out_0
  }else{df_op_out <- rbind(df_op_out_0, df_op_out)}
}
table(df_op_out$meaning_of_event)
# hospitalisation_main_diagnosis      hospitalisation_not_overnight_main_diagnosis 
# 187                                                65 
# hospitalisation_not_overnight_secondary_diagnosis               hospitalisation_secondary_diagnosis 
# 7                                               182 
# outpatient_contact_main_diagnosis            outpatient_contact_secondary_diagnosis 
# 1133                                               148 
# primary_care                         underlying_cause_of_death 
# 397                                                40


## remove all obs, with date var before max_end
tb <- "VISIT_OCCURRENCE"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!is.na(visit_start_date)]
  df <- df[!visit_start_date==""]
  new <- nrow(df)
  if (old == new) next
  removed <- old - new
  fwrite(df, files[i])
  print(paste("From ", files[i], " we removed ", removed, " rows."))
  gc()
}
# "From  VISIT_OCCURRENCE_KUHR_2010.csv  we removed  77  rows."
# "From  VISIT_OCCURRENCE_KUHR_2011.csv  we removed  104  rows."
# "From  VISIT_OCCURRENCE_KUHR_2013.csv  we removed  119  rows."
# "From  VISIT_OCCURRENCE_KUHR_2014.csv  we removed  182  rows."
#  "From  VISIT_OCCURRENCE_KUHR_2015.csv  we removed  188  rows."
# "From  VISIT_OCCURRENCE_KUHR_2012.csv  we removed  128  rows."
# "From  VISIT_OCCURRENCE_KUHR_2016.csv  we removed  188  rows."
# "From  VISIT_OCCURRENCE_KUHR_2017.csv  we removed  196  rows."
# "From  VISIT_OCCURRENCE_KUHR_2018.csv  we removed  199  rows."
# [1] "From  VISIT_OCCURRENCE_KUHR_2019.csv  we removed  220  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  VISIT_OCCURRENCE_KUHR_2020.csv  we removed  236  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  VISIT_OCCURRENCE_KUHR_2021.csv  we removed  329  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  VISIT_OCCURRENCE_KUHR_2022.csv  we removed  345  rows."
tb <- "EVENTS"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!is.na(start_date_record)]
  df <- df[!start_date_record==""]
  new <- nrow(df)
  if (old == new) next
  removed <- old - new
  fwrite(df, files[i])
  print(paste("From ", files[i], " we removed ", removed, " rows."))
}
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2010.csv  we removed  88  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2011.csv  we removed  122  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2012.csv  we removed  146  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2013.csv  we removed  136  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2014.csv  we removed  203  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2015.csv  we removed  219  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2016.csv  we removed  234  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2017.csv  we removed  213  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2018.csv  we removed  215  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2019.csv  we removed  248  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2020.csv  we removed  275  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2021.csv  we removed  360  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_KUHR_2022.csv  we removed  400  rows."
# [1] "From  EVENTS_NPR_AVT.csv  we removed  58297  rows."
# |--------------------------------------------------|
#   |==================================================|
#   [1] "From  EVENTS_NPR_SOM.csv  we removed  1543  rows."
# [1] "From  EVENTS_NPR_TSB.csv  we removed  18679  rows."

tb <- "SURVEY_OBSERVATIONS"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!so_date<max_end]
  new <- nrow(df)
  removed <- old - new
  print(paste("From ", files[i], " we removed ", removed, " rows."))
  fwrite(df, files[i])
  gc()
}

tb <- "SURVEY_ID"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!survey_date<max_end]
  new <- nrow(df)
  removed <- old - new
  print(paste("From ", files[i], " we removed ", removed, " rows."))
  fwrite(df, files[i])
  gc()
}

tb <- "VACCINES"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!vx_admin_date<max_end]
  new <- nrow(df)
  removed <- old - new
  print(paste("From ", files[i], " we removed ", removed, " rows."))
  fwrite(df, files[i])
  gc()
}

# remove for NA dates
tb <- "VISIT_OCCURRENCE"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!is.na(visit_start_date)]
  new <- nrow(df)
  removed <- old - new
  fwrite(df, files[i])
  print(paste("From ", files[i], " we removed ", removed, " rows."))
  gc()
}

tb <- "EVENTS"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!is.na(start_date_record)]
  new <- nrow(df)
  removed <- old - new
  fwrite(df, files[i])
  print(paste("From ", files[i], " we removed ", removed, " rows."))
}
# [1] "From  EVENTS_NPR_AVT.csv  we removed  994  rows."
# [1] "From  EVENTS_NPR_SOM.csv  we removed  17  rows."
# [1] "From  EVENTS_NPR_TSB.csv  we removed  305  rows."

tb <- "SURVEY_OBSERVATIONS"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!is.na(so_date)]
  new <- nrow(df)
  removed <- old - new
  print(paste("From ", files[i], " we removed ", removed, " rows."))
  fwrite(df, files[i])
  gc()
}

tb <- "SURVEY_ID"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!is.na(survey_date)]
  new <- nrow(df)
  removed <- old - new
  print(paste("From ", files[i], " we removed ", removed, " rows."))
  fwrite(df, files[i])
  gc()
}

tb <- "VACCINES"
files <- list.files(pattern = tb)
for ( i in seq_along(files)){
  df <- fread(files[i])
  old <- nrow(df)
  df <- df[!is.na(vx_admin_date)]
  new <- nrow(df)
  removed <- old - new
  print(paste("From ", files[i], " we removed ", removed, " rows."))
  fwrite(df, files[i])
  gc()
}


## remove all persons without a birth date from all tables
p <- fread("N:/durable/VAC4EU datasets/Delivery Feb-May 2023/SSB/W22_0605_FASTE_OPPLYSNINGER.csv")
sum(is.na(p$foedsels_aar_mnd)) # 371
remove_id <- unique(p[is.na(foedsels_aar_mnd)]$lnr)
files <- list.files(pattern = ".csv")
for ( i in seq_along(files)){
  if (!"person_id" %in% names(fread(files[i], nrow=1))) next
  else{
    df <- fread(files[i], colClass = 'character' )
    if(length(intersect(as.integer(unique(df$person_id)), remove_id))==0) next
    else{
      old <- nrow(df)
      df <- df[!person_id %in% remove_id]
      new <- nrow(df)
      removed <- old - new
      print(paste("From ", files[i], " we removed ", removed, " rows."))
      fwrite(df, files[i])
    } # has person_id but ? has birth date
  } # has person_id var
}
 
  

# remove vo_id from MEDICINES
getwd()
files <- list.files(pattern = "MEDICINES")
for (i in seq_along(files)){
  df <- fread(files[i])
  df$visit_occurrence_id <- NA
  fwrite(df, files[i])
}

# MO_INNT
df <- fread(list.files(pattern = "INNT"))
head(df)
sum(is.na(df$mo_source_value))
sum(df$mo_source_value=="")
sum(df$mo_unit=="")
sum(is.na(df$mo_unit))
str(df)
