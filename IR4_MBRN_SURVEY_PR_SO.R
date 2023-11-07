TSD_install_path <- "N:/durable/vac4eu/R_v4.2_packages_linux"
library(data.table)
library(haven)
library(stringr)
library(reshape2)

library(tidyverse)
# library(haven)
ref_date <- fread("N:/durable/vac4eu/ref_date_2023.csv")
# ref_date <- as.Date("1902-05-16")
df <- read_sav("N:/durable/VAC4EU datasets/Delivery Feb-May 2023/MBRN/p222359_mfr.sav")
# df <- data.table(df)
# fwrite(df, "N:/durable/VAC4EU datasets/Delivery Feb-May 2023/MBRN/p222359_mfr.csv")
head(df)
df <- data.table(df)
sum(is.na(df$ID_MOR)) # 0
sum(is.na(df$ID_BARN)) # 0
sum(is.na(df$FDATO_DIFFERANSEDAGER_MOR)) # 4804
sum(is.na(df$FDATO_DIFFERANSEDAGER_BARN)) # 13871
# 752 475



# Note: this is a fix if R loads data with an additional column
# Change incorrect_col_nums as necessary
# Comment away as necessary 
# incorrect_col_nums = 80
# if (ncol(df) == incorrect_col_nums){
#   correct_names <- names(df)[2:ncol(df)]
#   df[[ncol(df)]] <- NULL
#   setnames(df, correct_names)
# }
# head(df)

# 752 475
# generate survey_id
df <- df %>% 
  distinct() %>% 
  group_by(ID_MOR, FDATO_DIFFERANSEDAGER_MOR) %>% 
  mutate(survey_id = paste0("MBRN_", cur_group_id()))
df <- data.table(df)
str(df)
# choose variables that are needed
names(df)
"PARITET_5" %in% names(df)[grepl("_5", names(df))] # just to check if we get PARITET_5. false

df <- df[,.(ID_MOR, ID_BARN, FDATO_DIFFERANSEDAGER_MOR, 
            FDATO_DIFFERANSEDAGER_BARN,survey_id,
            ID_BARN, SVLEN_DG, PREEKL, DIABETES_MELLITUS,
            BLODN_F13, BLODN_13_28, BLODN_E28, BLODNING_O500,
            APGAR5, ZSCORE_BW_GA, VEKT, DODKAT, KMI_FOER,
            MOR_ROYKTE_FOER_SVSK, KSNITT, KSNITT_PLANLAGT,
            PARITET, # PARITET_5
            DODFODTE_5, PLACENTA_PREVIA, MISD, NEVRALRORSDEFEKTER,
            SPINAB, ENCEPH, ANENCEPH, LEPPE_LEPPEGANESPALTE, 
            GANESPALTE, HJERTE_MISD, GASTROS, OMPHALO, KLUMPFOT,
            KROMOSOMFEIL, DOWNS,
            BARNETS_HELSE)]

str(df)
df <- unique(df)
df <- data.table(df)
# define PARITET_5 ourselves and remove PRARIET
df$PARITET_5 <- ""
df[PARITET==0]$PARITET_5 <- "0"
df[PARITET==1]$PARITET_5 <- "1"
df[PARITET==2]$PARITET_5 <- "2"
df[PARITET==3]$PARITET_5 <- "3"
df[PARITET>=4]$PARITET_5 <- "4"
table(df$PARITET_5)

df <- df[,PARITET:=NULL]

# PI want us to impute 0 for all NA outcomes
df[is.na(df)] <- 0

# PREDUCE a TABLE 1 FOR ELENA
# OUTCOMES <- df[, c("ID_MOR", "ID_BARN", "BARNETS_HELSE", "survey_id", "FDATO_DIFFERANSEDAGER"):=NULL]
# OUTCOMES[is.na(OUTCOMES)] <- 0
# str(OUTCOMES)
# for (i in colnames(OUTCOMES)){
#   if (grepl("ZSCORE|SVLEN|VEKT|KMI", i)) OUTCOMES[[i]] <- as.numeric(OUTCOMES[[i]])
#   else OUTCOMES[[i]] <- as.factor(OUTCOMES[[i]])
# }
# head(OUTCOMES)
# 
# library(arules)
# OUTCOMES$GENT_QUANTILE <- discretize(OUTCOMES$SVLEN_DG, 
#                                      method = "cluster", breaks=4)
# 
# library(table1)
# table1(~.|GENT_QUANTILE, data = OUTCOMES)

############################
         
str(df)
df$ID_MOR <- as.integer(df$ID_MOR)
setnames(ref_date, "lnr", "ID_MOR")
df1 <- merge(df, ref_date, by = "ID_MOR", all.x = TRUE)
str(df1)
df1$so_date <- df1$FDATO_DIFFERANSEDAGER_MOR + df1$ref_date
summary(df1$so_date)
# FDATO_MOR
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "2009-12-17" "2013-02-12" "2016-04-15" "2016-05-06" "2019-07-19" "2023-01-14"       "4812" 
# FDATO_BARN
# #         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "2010-01-15" "2013-02-15" "2016-04-15" "2016-05-07" "2019-07-15" "2022-12-15"      "13905" 
df1 <- df1[!is.na(so_date)]
df1 <- df1[!is.na(FDATO_DIFFERANSEDAGER_BARN)]
df1$survey_date <- df1$FDATO_DIFFERANSEDAGER_MOR + df1$ref_date
df <- df1
rm(df1)
gc()
str(df)
df <- unique(df)
############################
# SURVEY_ID
############################
si <- df[,.(ID_MOR,
            ID_BARN,
            survey_id,
            survey_date)]

id_var <- c("survey_id", "survey_date")
si <- melt(si, id.vars = id_var)
si <- data.table(si)
si$survey_meaning <- ifelse(si$variable == "ID_MOR",
                            "birth_registry_mother",
                            "birth_registry_child")
si$survey_origin <- "MBRN"
si <- si[, variable:=NULL]
setnames(si, "value", "person_id")
str(si)
si <- si[!is.na(survey_date)]
si <- si[,.(person_id,
         survey_id,
         survey_date,
         survey_meaning,
         survey_origin)]

si$survey_date <- gsub("-", "", si$survey_date)
str(si)
si <- unique(si)
fwrite(si,"SURVEY_ID.csv")
rm(si)
gc()

############################
# PERSON_RELATIONSHIPS
############################

pr <- df[,.(ID_MOR,
            ID_BARN)]
setnames(pr, "ID_BARN", "person_id")
pr$meaning_of_relationship <- "birth_mother"
pr$origin_of_relationship <- "MBRN"
pr$method_of_linkage <- "deterministic"
setnames(pr, "ID_MOR", "related_id")
str(pr)
pr <- pr[,.(person_id,
            related_id,
            meaning_of_relationship,
            origin_of_relationship,
            method_of_linkage)]
pr <- unique(pr)
fwrite(pr, "PERSON_RELATIONSHIPS.csv")
rm(pr)
gc()
############################
# SURVEY_OBSERVATIONS
############################
str(df)
df <- df[,c("ref_date", 
            "survey_date", 
            "FDATO_DIFFERANSEDAGER_MOR",
            "FDATO_DIFFERANSEDAGER_BARN",):=NULL]

df$so_date <- gsub("-", "", df$so_date)

df <- df %>% 
  separate_rows(BARNETS_HELSE, sep = ";")

sum(is.na(df$BARNETS_HELSE))
sum(df$BARNETS_HELSE=="")

df <- data.table(df)
df[BARNETS_HELSE==""]$BARNETS_HELSE <- "-_-_-_-"
df$BARNETS_HELSE <- matrix(unlist(str_split(df$BARNETS_HELSE, "_")),
                                    ncol=4, byrow = TRUE)[,2]



str(df)

so <- df


setnames(so, "ID_MOR", "person_id")

so <- melt(so, id.vars = c("person_id", "so_date", "survey_id"))
head(so)


setnames(so, c("variable", "value"), 
         c("so_source_column", "so_source_value"))
so$so_source_column <- as.character(so$so_source_column)
so <- data.table(so)
so <- so[!is.na(so_source_value) & !so_source_value=="" & !so_source_value=="-"]

so$so_source_table <- "MBRN"
so$so_unit <- ""
so[so_source_column=="SVLEN_DG"]$so_unit <- "days"
so[so_source_column=="VEKT"]$so_unit <- "gram"
so[so_source_column=="KMI_FOER"]$so_unit <- "kg/m2"

str(so)
so[so_unit==""]$so_unit <- NA
so$so_meaning <- "birth_registry_mother"
so$so_origin <- "MBRN"

str(so)
so <- unique(so)
so <- so[,.(person_id,
            so_date,
            so_source_table,
            so_source_column,
            so_source_value,
            so_unit,
            so_meaning,
            so_origin,
            survey_id)]
fwrite(so, "N:/durable/vac4eu/CDMInstances/vac4eu/SURVEY_OBSERVATIONS.csv")
rm(df,so)
gc()
