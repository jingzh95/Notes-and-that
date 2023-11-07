# after reviewing ETL with Signe on July 7
# it is decided to not to use the registration date as record date
# we shall assign NA to leave it blank.

library(data.table)
library(stringr)
bla <- ifelse(toupper(Sys.info())["sysname"]=="LINUX",
              "/ess/p1921/data",
              "N:")

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))

###########################################
##### PERSONS #24.10.2023 ######
df <- fread("PERSONS_FASTE_OPPL.csv", colClasses = "character")
df$day_of_birth <- ""
sum(df$day_of_death=="")
sum(df$month_of_death=="")
sum(df$year_of_death=="") # all 5617973
fwrite(df, "PERSONS_FASTE_OPPL.csv")

df <- fread("PERSONS_MBRN.csv", colClasses = "character")
sum(is.na(df$day_of_death))
sum(df$day_of_death=="")
sum(df$year_of_death=="")

df <- p[, "so_date":=NULL]
sum(df$day_of_birth=="")
sum(is.na(df$day_of_birth))
fwrite(df, "PERSONS_MBRN.csv")

str(df)

so <- fread("SURVEY_OBSERVATIONS_CHILDREN.csv")
str(so)
barn <- so[person_id>=5900000,.(person_id, so_date)]
barn <- unique(barn)
df$person_id <- as.integer(df$person_id)
p <- merge(df, barn, by = "person_id")
str(p)
p$so_date <- as.character(p$so_date)
p$day_of_birth <- substr(p$so_date, 7,8)
p$day_of_death <- substr(p$so_date, 7,8)
p$month_of_birth <- substr(p$so_date, 5,6)
p$month_of_death <- substr(p$so_date, 5,6)
p$year_of_birth <- substr(p$so_date, 1,4)
p$year_of_death <- substr(p$so_date, 1,4)
str(p)

###########################################
##### INSTANCE & METADATA #24.10.2023######

ins <- fread("INSTANCE.csv")

ins[source_table_name=="KUHR" & source_column_name=="DIAGNOSER"]$restriction_in_values <- "yes"

# have to change this column to empty strings in order to assign characters in
ins$restriction_condition <- as.character(ins$restriction_condition)
str(ins)
ins$restriction_condition <- ""
ins[source_table_name=="KUHR" & source_column_name=="DIAGNOSER"]$restriction_condition <- "Yes, primary care only via filtering on PRAKSISTYPE."
fwrite(ins, "INSTANCE.csv")

meta <- fread("METADATA.csv")
meta[type_of_metadata == "list_of_values" & tablename == "INSTANCE" & columnname == "restriction_in_values"]$values <- "yes no"
meta[type_of_metadata == "list_of_values" & tablename == "VACCINES" & columnname == "vx_manufacturer"]$values <- "pfizer moderna astrazeneca janssen coronavac convidecia sinopharm covishield nuvaxovid cureVac covaxin covovax"
str(ins)
meta[type_of_metadata == "list_of_values" & tablename == "EVENTS" & columnname == "meaning_of_event"]$values <- "primary_care underlying_cause_of_death hospitalisation_main_diagnosis  hospitalisation_secondary_diagnosis hospitalisation_not_overnight_main_diagnosis hospitalisation_not_overnight_secondary_diagnosis outpatient_contact_main_diagnosis outpatient_contact_secondary_diagnosis hospital_encounter_main_diagnosis"
fwrite(meta, "METADATA.csv")


files <- list.files(pattern = "MEDICINES")
for ( i in seq_along(files)){
  df <- fread(files[i])
  print(unique(df$origin_of_drug_record))
  df$visit_occurrence_id <- NA
  df$origin_of_drug_record <- "LMR"
  fwrite(df, files[i])
  gc()
}
########################################################
############ sex_at_instance_creation ##################
df <- fread("PERSONS_MBRN.csv", colClasses = "character")
table(df$sex_at_instance_creation)
#    F           M     Unknown Unspecified      Unsure 
# 3499        4267        2286          86         651
library(stringr)
df$sex_at_instance_creation <- str_replace_all(df$sex_at_instance_creation, 
                                      c("Unsure" = "O", 
                                        "Unknown" = "U", 
                                        "Unspecified" = "U"))
table(df$sex_at_instance_creation)
#    F    M    O    U 
# 3499 4267  651 2372 
fwrite(df, "PERSONS_MBRN.csv")
########################################################
########################################################





#######################################################
##### HYPERTENSJON_ALENE ? ############################
df <- fread("SURVEY_OBSERVATIONS_MOTHER.csv")
table(df$so_source_column)
# rerun the script and make sure HYPERTENSJON_ALENE is included.

########################################################
############ format in CDM_SOURCE  #####################

###########################################
##### INSTANCE ############################

ins <- fread("INSTANCE.csv")

ins[source_table_name=="BOSTEDSFYLKE"]$since_when_data_complete <- 20200101
ins[source_table_name=="BOSTEDSFYLKE"]$up_to_when_data_complete <- 20220101

ins[source_table_name=="SIVILSTAND"]$since_when_data_complete <- 20200101
ins[source_table_name=="SIVILSTAND"]$up_to_when_data_complete <- 20220101

fwrite(ins, "INSTANCE.csv")

###########################################
###########################################


cdm <- fread("CDM_SOURCE.csv", colClasses = "character")
cdm$data_access_provider_code <- "01"
cdm$cdm_version <- "2.2"
fwrite(cdm, "CDM_SOURCE.csv")
####################################
# MEDICINES
# prescriber_specialty_vocabulary
# to prescriber_speciality_vocabulary
# make up your mind UMCU
####################################
files <- list.files(pattern = "MEDICINES")
for (i in seq_along(files)){
  df <- fread(files[i], colClasses = "character")
  setnames(df, "prescriber_specialty_vocabulary", 
           "prescriber_speciality_vocabulary")
  fwrite(df, files[i])
  rm(df)
  gc()
}


####################################
# forgot what this was for
df <- fread("VACCINES.csv")

str(df)
df$vx_record_date <- NA
summary(df$vx_admin_date)
sum(df[grepl("covid",  vx_type)]$vx_admin_date<20201212)
# 305
fwrite(df, "VACCINES.csv")


# after reviewing ETL with Signe on July 7
# they want mo_code = 713 in MO table
# so we put these info at both mo_source_value
# and mo_code, for Pifzer script pick up differently

df <- fread(list.files(pattern = "COVID"))
str(df)

# shit, i already did. i knew they can be ...
# good for me!
####################################

####################################
# PERSONS
# date format after everything open and save
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
df <- fread("CDM_SOURCE.csv")
str(df)
df$data_access_provider_code <- "01"
df$instance_number <- "01"
str(df)
fwrite(df, "CDM_SOURCE.csv")

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_10k/"))
df <- fread("CDM_SOURCE.csv")
str(df)
df$data_access_provider_code <- "01"
df$instance_number <- "01"
str(df)
fwrite(df, "CDM_SOURCE.csv")

####################################
# PERSONS
# date format after everything open and save
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
df <- fread("PERSONS.csv")
str(df)

df1 <- df
df1$month_of_death <- as.character(df1$month_of_death)
df1[!is.na(month_of_death) &
      as.integer(month_of_death)<10]$month_of_death <- 
  paste0("0", df1[!is.na(month_of_death) &
                    as.integer(month_of_death)<10]$month_of_death)
str(df1[!is.na(year_of_death)])

df1$day_of_death <- as.character(df1$day_of_death)
df1[!is.na(day_of_death) &
      as.integer(day_of_death)<10]$day_of_death <- 
  paste0("0", df1[!is.na(day_of_death) &
                    as.integer(day_of_death)<10]$day_of_death)

df1$month_of_birth <- as.character(df1$month_of_birth)
df1[!is.na(month_of_birth) &
      as.integer(month_of_birth)<10]$month_of_birth <- 
  paste0("0", df1[!is.na(month_of_birth) &
                    as.integer(month_of_birth)<10]$month_of_birth)
str(df1[!is.na(year_of_birth)])

df1$day_of_birth <- as.character(df1$day_of_birth)
df1[!is.na(day_of_birth) &
      as.integer(day_of_birth)<10]$day_of_birth <- 
  paste0("0", df1[!is.na(day_of_birth) &
                    as.integer(day_of_birth)<10]$day_of_birth)

fwrite(df1, "PERSONS.csv")

####################################
# SURVEY ID
# forgot to rename the column=V1?
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
df <- fread("SURVEY_ID.csv")
head(df)
df <- df[,-1]
fwrite(df,"SURVEY_ID.csv")
survey_id <- unique(df$person_id)
survey_id <- unique(df$survey_id)

df <- fread("SURVEY_OBSERVATIONS_MOTHER.csv")
mother_id <- unique(df$person_id)
mother_survey <- unique(df$survey_id)
length(setdiff(mother_id, survey_id)) # 2135
length(setdiff(mother_survey, survey_id)) # 5
missing_survey <- setdiff(mother_survey, survey_id)
to_be_added <- df[survey_id %in% missing_survey]
to_be_added <- to_be_added[,.(person_id, so_date, survey_id, so_meaning)]
to_be_added <- unique(to_be_added)
df <- fread("SURVEY_OBSERVATIONS_CHILDREN.csv")
children_id <- unique(df$person_id)
children_survey <- unique(df$survey_id)
length(setdiff(children_id, survey_id)) # 35
length(setdiff(children_survey, survey_id)) # 5
to_be_added_survey_children <- setdiff(children_survey, survey_id)
to_be_added_children <- df[survey_id %in% to_be_added_survey_children]
to_be_added_children <- to_be_added_children[,.(person_id, so_date, survey_id, so_meaning)]
to_be_added_children <- unique(to_be_added_children)
to_be_added <- rbind(to_be_added, to_be_added_children)
setnames(to_be_added, c("so_date", "so_meaning"),
         c("survey_date", "survey_meaning"))
to_be_added$survey_origin <- "MBRN"
df <- rbind(df, to_be_added)
rm(df)
gc()
####################################
# INSTANCE
# restriction_condtion
# in METADATA not in INSTANCE
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
ins <- fread("INSTANCE.csv")
meta <- fread("METADATA.csv")
meta <- data.table(meta)
meta[tablename=="EVENTS" & columnname == "meaning_of_event"]$values <- 
  gsub("over_night", "overnight", meta[tablename=="EVENTS" & columnname == "meaning_of_event"]$values)

meta[tablename=="SURVEY_OBSERVATIONS" & columnname == "so_meaning"]$values <- "birth_registry_mother birth_registry_child"

meta[tablename=="SURVEY_OBSERVATIONS" & columnname == "so_source_column"]$values <- "SVLEN_DG PREEKL DIABETES_MELLITUS BLODN_F13 BLODN_13_28 BLODN_E28 BLODNING_O500 APGAR5 ZSCORE_BW_GA VEKT DODKAT KMI_FOER MOR_ROYKTE_FOER_SVSK KSNITT KSNITT_PLANLAGT DODFODTE_5 PLACENTA_PREVIA MISD NEVRALRORSDEFEKTER SPINAB ENCEPH ANENCEPH LEPPE_LEPPEGANESPALTE GANESPALTE HJERTE_MISD GASTROS OMPHALO KLUMPFOT KROMOSOMFEIL DOWNS BARNETS_HELSE PARITET_5"
meta[tablename=="PERSONS" & columnname == "sex_at_instance_creation"]$values <- "M F U"
str(meta)
meta[columnname=="restriction_condition"]$values <- "No"
fwrite(meta, "METADATA.csv")


setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_10k/"))
ins <- fread("INSTANCE.csv")
str(ins)
meta <- fread("METADATA.csv")
str(meta)
meta[columnname=="restriction_condition"]$values <- "No"
fwrite(meta, "METADATA.csv")

ins[source_table_name=="SSB"]$since_when_data_complete <- 20091216
ins[source_table_name=="SSB"]$up_to_when_data_complete <- 20221215

fwrite(ins, "INSTANCE.csv")

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
meta <- fread("METADATA.csv")
str(meta)
meta[columnname=="restriction_condition"]$values <- "No"
fwrite(meta, "METADATA.csv")

rm(ins,meta)
gc()
####################################
# EVENTS_DAR
## missing event_code
####################################

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_10k/"))
df <- fread("EVENTS_DAR.csv")
sum(is.na(df$event_code))
sum(df$event_code == "") # 7
df <- df[!event_code == ""]
fwrite(df, "EVENTS_DAR.csv")

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
df <- fread("EVENTS_DAR.csv")
sum(is.na(df$event_code))
sum(df$event_code == "") # 3773
df <- df[!event_code == ""]
fwrite(df, "EVENTS_DAR.csv")


####################################
# EVENTS_KUHR
## missing event_record_vocabulary
# all == ICPC
####################################

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
df <- fread("EVENTS_KUHR_2018.csv")
df <- df[!event_code==""]
fwrite(df, "EVENTS_KUHR_2018.csv")

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
files <- list.files(pattern = "EVENTS_KUHR")
for (i in seq_along(files)){
  df <- fread(files[i])
  df <- df[!is.na(event_code)]
  df <- df[!event_code==""]
  fwrite(df, files[i])
}

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_100k/"))
files <- list.files(pattern = "EVENTS_KUHR")
for (i in seq_along(files)){
  df <- fread(files[i])
  df <- df[!is.na(event_code)]
  fwrite(df, files[i])
}

####################################
# EVENTS_NPR
## NA_main_diagnosis
## over_night to overnight
####################################
## NA_main_diagnosis
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
files <- list.files(pattern = "EVENTS_NPR")
for ( i in seq_along(files)){
  df <- fread(files[i])
  # table(df$meaning_of_event)
  df[grepl("NA", meaning_of_event)]$meaning_of_event <- 
    str_replace(df[grepl("NA", meaning_of_event)]$meaning_of_event, 
                "NA", "hospital_encounter")
  fwrite(df, files[i])
}
rm(df)
gc()

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_100k/"))
files <- list.files(pattern = "EVENTS_NPR")
for ( i in seq_along(files)){
  df <- fread(files[i])
  # table(df$meaning_of_event)
  df[grepl("NA", meaning_of_event)]$meaning_of_event <- 
    str_replace(df[grepl("NA", meaning_of_event)]$meaning_of_event, 
                "NA", "hospital_encounter")
  fwrite(df, files[i])
}
rm(df)
gc()

## over_night to overnight

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_100k/"))
files <- list.files(pattern = "EVENTS_NPR")
for ( i in seq_along(files)){
  df <- fread(files[i])
  table(df$meaning_of_event)
  df[grepl("over_night", meaning_of_event)]$meaning_of_event <- 
    str_replace(df[grepl("over_night", meaning_of_event)]$meaning_of_event, 
                "over_night", "overnight")
  fwrite(df, files[i])
}
rm(df)
gc()

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
files <- list.files(pattern = "EVENTS_NPR")
for ( i in seq_along(files)){
  df <- fread(files[i])
  table(df$meaning_of_event)
  df[grepl("over_night", meaning_of_event)]$meaning_of_event <- 
    str_replace(df[grepl("over_night", meaning_of_event)]$meaning_of_event, 
                "over_night", "overnight")
  fwrite(df, files[i])
}
rm(df)
gc()


setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_100k/"))
files <- list.files(pattern = "OCCURRENCE_NPR")
for ( i in seq_along(files)){
  df <- fread(files[i])
  table(df$meaning_of_visit)
  df[grepl("over_night", meaning_of_visit)]$meaning_of_visit <- 
    gsub("over_night", "overnight",
         df[grepl("over_night", meaning_of_visit)]$meaning_of_visit)
  fwrite(df, files[i])
}
rm(df)
gc()

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
files <- list.files(pattern = "OCCURRENCE_NPR")
for ( i in seq_along(files)){
  df <- fread(files[i])
  table(df$meaning_of_visit)
  df[grepl("over_night", meaning_of_visit)]$meaning_of_visit <- 
    gsub("over_night", "overnight",
         df[grepl("over_night", meaning_of_visit)]$meaning_of_visit)
  fwrite(df, files[i])
}
rm(df)
gc()
####################################
# VISIT_OCCURRENCE_NPR
## over_night to overnight
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_10k/"))
files <- list.files(pattern = "VISIT_OCCURRENCE_NPR")
for ( i in seq_along(files)){
  df <- fread(files[i])
  table(df$meaning_of_visit)
  df[grepl("over_night", meaning_of_visit)]$meaning_of_visit <- 
    str_replace(df[grepl("over_night", meaning_of_visit)]$meaning_of_visit, 
                "over_night", "overnight")
  fwrite(df, files[i])
}
rm(df)
gc()

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
files <- list.files(pattern = "VISIT_OCCURRENCE_NPR")
for ( i in seq_along(files)){
  df <- fread(files[i])
  table(df$meaning_of_visit)
  df[grepl("over_night", meaning_of_visit)]$meaning_of_visit <- 
    str_replace(df[grepl("over_night", meaning_of_visit)]$meaning_of_visit, 
                "over_night", "overnight")
  fwrite(df, files[i])
}
rm(df)
gc()



####################################
# METADATA
# mo_source_column updates
# adding procedure_code_vocabulary	NCMP-NCSP-NCRP
# adding meaning_of_procedure	medicine_and_surgical_measures
# adding survey_meaning to list_of_values
# adding so_unit days gram kg/m2 to list_of_values

####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_10k/"))
meta <- fread("METADATA.csv")
old <- meta[type_of_metadata == "list_of_values" & 
            columnname=="mo_source_column"]$values
old
new <- "DiagnoseNr aargang wlonn 2020 2021 sivilstand_2020 sivilstand_2021 sivilstand_2022 bostedsfylke_2020 bostedsfylke_2021 bostedsfylke_2022"
meta[type_of_metadata == "list_of_values" & 
       columnname=="mo_source_column"]$values <- new

meta[type_of_metadata == "list_of_values" &
       columnname=="procedure_code_vocabulary"]$values

# check the PROCEDURE chunck
meta[tablename=="PROCEDURES"]
meta[tablename=="PROCEDURES" & 
       columnname == "visit_occurrence_id"]$values <- "No"

# check the procedure_code_vocabulary
meta[type_of_metadata == "list_of_values" &
       tablename=="PROCEDURES"]

# to add two rows
names(meta)
proc <- data.table(type_of_metadata="list_of_values",
                   tablename="PROCEDURES",
                   columnname=c("procedure_code_vocabulary",
                                "meaning_of_procedure"),
                   other=NA,
                   values=c("NCMP-NCSP-NCRP",
                            "medicine_and_surgical_measures")
                   )
meta <- rbind(meta, proc)

# adding survey_meaning birth_registry_child to list_of_values

meta[type_of_metadata == "list_of_values" & 
       columnname=="survey_meaning"]$values <- 
  "birth_registry_mother birth_registry_child"

# adding so_unit days gram kg/m2 to list_of_values
meta[tablename=="SURVEY_OBSERVATIONS"]
  # adding a row for list_of_values for so_unit
  so_ui <- data.table(type_of_metadata="list_of_values",
                   tablename="SURVEY_OBSERVATIONS",
                   columnname= "so_unit",
                   other=NA,
                   values="days gram kg/m2"
                   )
meta <- rbind(meta, so_ui)
meta[tablename=="SURVEY_OBSERVATIONS"]
fwrite(meta, "METADATA.csv")
# removing vx_manufacturer for non-covid vaccines

# check the PERSONS chunk
meta[tablename=="PERSONS"]
# have checked the previous lv1 
# that's one of the 3 false flags.
# shall try to fix
fwrite(meta, "METADATA.csv")

####################################
# VACCINES
## rm vx_manufacturer for non-covid
## add match everything precisely with 
## METADATA
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
df <- fread("VACCINES.csv")
vx_type <- unique(df$vx_type)
vx_type_comb <- paste(vx_type, collapse = " ")

meta <- fread("METADATA.csv")
str(meta)
meta[tablename=="VACCINES" & 
       type_of_metadata=="list_of_values" &
       columnname == "vx_type"]$values <- vx_type_comb
fwrite(meta, "METADATA.csv")

vx_manu_cov <- 
  unique(df[grepl("covid", vx_type)]$vx_manufacturer)
vx_manu_cov <- paste(vx_manu_cov, collapse = " ")
meta[tablename=="VACCINES" & 
       type_of_metadata=="list_of_values" &
       columnname == "vx_manufacturer"]$values <- vx_manu_cov
fwrite(meta, "METADATA.csv")

# remove vx_manufacturer for non_covid vx
unique(df[!grepl("covid", vx_type)]$vx_manufacturer)
df[!grepl("covid", vx_type)]$vx_manufacturer <- NA
getwd() # 10k, change it to full
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu"))
fwrite(df, "VACCINES.csv")

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_10k"))
df <- fread("VACCINES.csv")
df[!grepl("covid", vx_type)]$vx_manufacturer <- NA
getwd() # 10k
fwrite(df, "VACCINES.csv")


####################################
# FALSE FLAG in STEP_1TO3
# MO: mo_source_column,
# MO: mo_meaning full of regions?
# PERSONS: race
# PERSON_RELATIONSHIPS: origin_of_relationship
# SURVEY_OBSERVATIONS: so_source_column
####################################
#fixed in the lv1 step1to3



####################################
# MEDICAL_OBSERVATIONS: "region"
####################################

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
tbs <- list.files(pattern = "MEDICAL")
df <- fread(tbs[grepl("BOSTED", tbs)])
str(df)
df$mo_source_value <- df$mo_meaning
df$mo_meaning <- "region"
str(df)
fwrite(df, tbs[grepl("BOSTED", tbs)])


####################################
# OBSERVATIONS_PERIOD: 1995 BIZ
## op_start_date red	3189544
## op_end_date	0	5456114
## aux end_date 20241231 (5455246) to NA
## rm op_end_date < min(INS$since)
## recommended_end_date <- max(INS$up_to)
## CDM_SOURCE
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
tbs <- list.files(pattern = "VATION_")
df <- fread(tbs)
str(df)
sum(df$op_start_date>20221216) # 1798

sum(df$op_end_date==20241231) # 5455246
sum(is.na(df$op_end_date)) # NA
df[op_end_date==20241231]$op_end_date <- NA

summary(df$op_end_date)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19640916 20190322 20200716 20197453 20211019 20230114  5455246 

nrow(df[op_start_date < min(ins$since_when_data_complete)])
#4381108

nrow(df[op_start_date < 19950101])
# 3189544 bingo

nrow(df[op_end_date < 19950101])
# 868

source <- fread("CDM_SOURCE.csv", colClasses = "character")
ins <- fread("INSTANCE.csv")
source$recommended_end_date <- 
  max(ins$up_to_when_data_complete)

fwrite(source, "CDM_SOURCE.csv")
fwrite(df, tbs)


setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_10k/"))
tbs <- list.files(pattern = "VATION_")
df <- fread(tbs)
str(df)
sum(df$op_start_date>20221216) # 1798

sum(df$op_end_date==20241231) # 5455246
sum(is.na(df$op_end_date)) # NA
df[op_end_date==20241231]$op_end_date <- NA

summary(df$op_end_date)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 19640916 20190322 20200716 20197453 20211019 20230114  5455246 

nrow(df[op_start_date < min(ins$since_when_data_complete)])
#4381108

nrow(df[op_start_date < 19950101])
# 3189544 bingo

nrow(df[op_end_date < 19950101])
# 868

source <- fread("CDM_SOURCE.csv")
ins <- fread("INSTANCE.csv")
source$recommended_end_date <- 
  max(ins$up_to_when_data_complete)

fwrite(source, "CDM_SOURCE.csv")
fwrite(df, tbs)


####################################
####################################
###### DUPLICATES ##################
####################################
####################################

####################################
# VISIT_OCCURRENCE:  DUP VO_IDs?
####################################

setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_100k/")
tbs <- list.files(pattern = "RENCE")
for (i in seq_along(tbs)){
  df <- fread(tbs[i])
  old <- nrow(df)
  old
  df <- unique(df)
  new <- nrow(df)
  new
  print(paste("There were", old-new, "duplicated rows."))
  if (old == new) next
  else fwrite(df, tbs[i])
}



####################################
# MEDICAL_OBSERVATIONS: 1 DUP
####################################

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
tbs <- list.files(pattern = "MEDICAL")
for (i in seq_along(tbs)){
  df <- unique(fread(tbs[i], colClasses = "character"))
  fwrite(df, tbs[i])
}

####################################
# PROCEDURES: 387 DUP from LV1
####################################

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
tbs <- list.files(pattern = "PROC")
for (i in seq_along(tbs)){
  df <- fread(tbs[i], colClasses = "character")
  dup <- df[duplicated(df)]
  if (nrow(dup) == 0) next
  else{
    print(paste(tbs[i], "has", nrow(dup), "dups." ))
    df <- unique(df)
    fwrite(df, tbs[i])
  }
}
# "PROCEDURES_NPR_SOM.csv has 3836 dups."


####################################
# VACCINES: 1204 DUP from LV1
####################################

setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
tbs <- list.files(pattern = "VACC")
df <- fread(tbs, colClasses = "character")
dup <- df[duplicated(df)]
dup_type <- unique(dup$vx_type)
dup_type_covid <- 
  dup_type[grepl("covid", dup_type)]
dup_covid <- dup[vx_type == dup_type_covid]
if (nrow(dup)>0){
  print(paste(tbs, "has", nrow(dup), 
              "dups. and", nrow(dup_covid), 
              "are", dup_type_covid, "." ))
  }
df <- unique(df)
fwrite(df, tbs)

# "VACCINES.csv has 1204 dups. and 55 are covid19_original ."

####################################
# OBSERVATION_PERIODS: 4 DUP from LV1
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
tbs <- list.files(pattern = "VATION_PER")
df <- fread(tbs, colClasses = "character")
dup <- df[duplicated(df)]

if (nrow(dup)>0){
  print(paste(tbs, "has", nrow(dup), 
              "dups." ))
}
df <- unique(df)
fwrite(df, tbs)

# "OBSERVATION_PERIODS.csv has 4 dups."


####################################
# SURVEY_OBSERVATIONS: 62787812 DUP from LV1
## 1TO3 FORMAT ERROR???
####################################
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu_100k/"))
tbs <- list.files(pattern = "VEY_OBS")
df <- fread(tbs[2], colClasses = "character")
unique(df$so_source_column)
df <- df[!so_source_column == "FDATO_DIFFERANSEDAGER_MOR"]
df <- df[!so_source_column == "FDATO_DIFFERANSEDAGER_BARN"]
df <- df[!so_source_column == "birth_date"]
df <- df[!so_source_column == "ref_date"]
df$so_date <- gsub("-", "", df$so_date)
str(df)
dup <- df[duplicated(df)]

if (nrow(dup)>0){
  print(paste(tbs, "has", nrow(dup), 
              "dups." ))
}

unique(dup$so_source_column)
unique(dup[!so_source_column=="ID_BARN"]$so_source_value)
head(dup[!so_source_column=="ID_BARN"])
head(dup[so_source_column=="BLODN_F13"])
df <- unique(df)
fwrite(df, tbs[2])
rm(dup)
rm(df)
gc()
# "SURVEY_OBSERVATIONS.csv has 62787812 dups."

