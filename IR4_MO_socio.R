library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)

ref_date <- fread("N:/durable/vac4eu/ref_date_2023.csv")
setwd("N:/durable/vac4eu/CDMInstances/vac4eu/")
raw_path <- "N:/durable/VAC4EU datasets/Delivery Feb-May 2023/"
origin <- "SSB"
format <- ".csv"
table_path <- paste0(raw_path, origin, "/")
files <- list.files(path = table_path, pattern = format)
files

######################
# MO_INCOME_2019_2020
######################

tables <- list.files(path = table_path, pattern = "INNT")
if (length(tables)==1) table <- tables
df <- fread(paste0(table_path, table))

str(df)

df <- df[aargang>2019] # 2020 onwards

setnames(df, "lnr", "person_id")

df <- melt(df, id.vars = "person_id")

df <- data.table(df)
setnames(df, c("variable", "value"),
         c("mo_source_column", "mo_source_value"))

df$person_id <- as.character(df$person_id)
df$mo_source_column <- as.character(df$mo_source_column)
# str(df)

df$mo_date <- paste0(df$mo_source_column, "0101") #Luigi: remove quotes around df$mo_source_column
df$mo_code <- NA #Luigi: can later change to NA
df$mo_record_vocabulary <- NA #Luigi: can later change to NA
df$mo_source_table <- "INNT"
df$mo_unit <- "NOK"
df$mo_meaning <- "gross_income"
df$mo_origin <- "SSB"
df$visit_occurrence_id <- NA

df <- df[,.(person_id,
            mo_date,
            mo_code,
            mo_record_vocabulary,
            mo_source_table,
            mo_source_column,
            mo_source_value,
            mo_unit,
            mo_meaning,
            mo_origin,
            visit_occurrence_id)]
df <- unique(df)

str(df)

fwrite(df, "MEDICAL_OBSERVATIONS_SSB_INNT_2020_2021.csv")
rm(df)
gc()
  
  

############################
# MO_marialstatus 2020-2022
############################


tables <- list.files(path = table_path, pattern = "SIVIL")
tables
if (length(tables)==1) table <- tables
df <- fread(paste0(table_path, table))
head(df)

df[, c("sivilstand_2015", 
       "sivilstand_2016", 
       "sivilstand_2017",
       "sivilstand_2018",
       "sivilstand_2019"):=NULL]

setnames(df, "lnr", "person_id")

df <- melt(df, id.vars = "person_id")

gc()
head(df)

setnames(df, c("variable", "value"),
         c("mo_source_column", "mo_source_value"))
df$mo_source_column <- as.character(df$mo_source_column)
df$person_id <- as.character(df$person_id)


df$mo_date <- paste0(extract_numeric(df$mo_source_column),
                     "0101")

# unique(df$mo_date)
# 
# table(df$mo_source_value)

# 0-9 and NA in mo_source_value
# we only have explanation for 1-9
# https://www.ssb.no/klass/klassifikasjoner/19

df <- subset(df, !is.na(df$mo_source_value))
# df <- subset(df, df$mo_source_value > 0)

df$mo_code <- NA #Luigi: can later change to NA
df$mo_record_vocabulary <- NA #Luigi: can later change to NA
df$mo_source_table <- "SIVILSTAND"
df$mo_meaning <- "marital_status"

df$mo_unit <- NA #Luigi: can later change to NA

# df$mo_meaning[df$mo_source_value==1] <- "unmarried"
# df$mo_meaning[df$mo_source_value==2] <- "married"
# df$mo_meaning[df$mo_source_value==3] <- "widow_or_widower"
# df$mo_meaning[df$mo_source_value==4] <- "divorced"
# df$mo_meaning[df$mo_source_value==5] <- "separated"
# df$mo_meaning[df$mo_source_value==6] <- "civil_partner"
# df$mo_meaning[df$mo_source_value==7] <- "separated_civil_partner"
# df$mo_meaning[df$mo_source_value==8] <- "divorced_partner"
# df$mo_meaning[df$mo_source_value==9] <- "surviving_partner"



df$mo_origin <- "SSB"



df$mo_source_value <- as.character(df$mo_source_value)
df$visit_occurrence_id <- NA

df <- df[,.(person_id,
            mo_date,
            mo_code,
            mo_record_vocabulary,
            mo_source_table,
            mo_source_column,
            mo_source_value,
            mo_unit,
            mo_meaning,
            mo_origin,
            visit_occurrence_id)]
df <- unique(df)
str(df)
fwrite(df,"N:\\durable\\vac4eu\\CDMInstances\\vac4eu\\MEDICAL_OBSERVATIONS_SSB_SIVILSTAND_2020_2022.csv")
rm(df)


############################
# MO_REGION 2020-2022
############################

table <- list.files(path = table_path, pattern = "BOSTEDS")
df <- fread(paste0(table_path, table))
head(df)  # 2015-2022
setnames(df, "lnr", "person_id")

df[, c("bostedsfylke_2015", 
       "bostedsfylke_2016", 
       "bostedsfylke_2017",
       "bostedsfylke_2018",
       "bostedsfylke_2019"):=NULL]

df <- melt(df, id.vars = "person_id")
df <- data.table(df)

setnames(df, c("variable", "value"),
         c("mo_source_column", "mo_source_value"))
df <- df[!is.na(mo_source_value)]

df$mo_date <- paste0(extract_numeric(df$mo_source_column), "0101")
df$mo_code <- NA #Luigi: can later change to NA
df$mo_record_vocabulary <- NA #Luigi: can later change to NA
df$mo_source_table <- "BOSTEDSFYLKE"

df$mo_unit <- NA #Luigi: can later change to NA

unique(df$mo_source_value)


# df$mo_meaning[df$mo_source_value==46] <- "Vestland"
# df$mo_meaning[df$mo_source_value==30] <- "Viken"
# df$mo_meaning[df$mo_source_value==42] <- "Agder"
# df$mo_meaning[df$mo_source_value==34] <- "Innlandet"
# df$mo_meaning[df$mo_source_value==50] <- "Troendelag"
# df$mo_meaning[df$mo_source_value==38] <- "Vestfold og Telemark"
# df$mo_meaning[df$mo_source_value==3] <- "Oslo"
# df$mo_meaning[df$mo_source_value==18] <- "Nordland"
# df$mo_meaning[df$mo_source_value==15] <- "Moere og Romsdal"
# df$mo_meaning[df$mo_source_value==11] <- "Rogaland"
# df$mo_meaning[df$mo_source_value==54] <- "Troms og Finnmark"
# sum(df$mo_meaning=="")
# sum(is.na(df$mo_meaning))
# df <- df[!is.na(df$mo_meaning)]

df$mo_meaning <- "region"

df$mo_origin <- "SSB"

str(df)

df$mo_source_column <- as.character(df$mo_source_column)
df$visit_occurrence_id <- NA

df <- df[,.(person_id,
            mo_date,
            mo_code,
            mo_record_vocabulary,
            mo_source_table,
            mo_source_column,
            mo_source_value,
            mo_unit,
            mo_meaning,
            mo_origin,
            visit_occurrence_id)]
df <- unique(df)
str(df)
fwrite(df,"N:\\durable\\vac4eu\\CDMInstances\\vac4eu\\MEDICAL_OBSERVATIONS_SSB_BOSTEDSFYLKE_2020_2022.csv")
rm(df)
gc()
