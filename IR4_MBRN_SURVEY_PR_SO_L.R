TSD_install_path <- "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux"
library(data.table)
library(tzdb, lib.loc = TSD_install_path)
library(readr, lib.loc = TSD_install_path)
library(haven, lib.loc = TSD_install_path)
library(stringr, lib.loc = TSD_install_path)
library(reshape2, lib.loc = TSD_install_path)

library(backports, lib.loc = TSD_install_path)

library(withr, lib.loc = TSD_install_path)
library(crayon, lib.loc = TSD_install_path)
library(dplyr, lib.loc = TSD_install_path)
library(tidyr, lib.loc = TSD_install_path)
# library(haven)
ref_date <- fread("/ess/p1921/data/durable/vac4eu/ref_date_2023.csv")
# ref_date <- as.Date("1902-05-16")
df <- read_sav("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/MBRN/p222359_mfr.sav")
df <- data.table(df)
# fwrite(df, "/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/MBRN/p222359_mfr.csv")
head(df)
# the number of rows without mothers' id
sum(is.na(df$ID_MOR)) # 0
sum(df$ID_MOR == "") # 4804
# the number of rows without children's id
sum(is.na(df$ID_BARN)) # 0
sum(df$ID_BARN == "") # 13871
# the number of rows without mothers' delivery date
sum(is.na(df$FDATO_DIFFERANSEDAGER_MOR)) # 4804
# the number of rows without children's delivery date
sum(is.na(df$FDATO_DIFFERANSEDAGER_BARN)) # 13871
# the number of rows without children id BUT with mother's delivery date
# these rows are the candidates for imputing ID for children
nrow(df[ID_BARN == "" & !is.na(FDATO_DIFFERANSEDAGER_MOR)]) # 10804

# we shall exclude those rows without mothers' delivery date
df <- df[!is.na(df$FDATO_DIFFERANSEDAGER_MOR)]
# 752 475
###################################################
###################################################
# check the gender for the children with (later) imputed IDs
table(df[ID_BARN == ""]$KJONN)
# 0    1    2    3    9 
# 86 4275 3501  655 2287 
# 1=M, 2=F, 3=Unsure, 9=Unknown, 0=Unspecified
###################################################
###################################################
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
# assign an unique survey_id to
# each mother ID and each of her delivery date,
df <- df %>% 
  distinct() %>% # remove duplicated rows
  group_by(ID_MOR, FDATO_DIFFERANSEDAGER_MOR) %>% 
  mutate(survey_id = paste0("MBRN_", cur_group_id()))
df <- data.table(df)
str(df)
# choose variables that are needed
names(df)
"PARITET_5" %in% names(df)[grepl("_5", names(df))] # just to check if we get PARITET_5. false

df <- df[,.(ID_MOR, ID_BARN, FDATO_DIFFERANSEDAGER_MOR, 
            FDATO_DIFFERANSEDAGER_BARN,survey_id,
            SVLEN_DG, PREEKL, DIABETES_MELLITUS, HYPERTENSJON_ALENE, PLACENTA_PREVIA,
            BLODN_F13, BLODN_13_28, BLODN_E28, BLODNING_O500,
            APGAR5, ZSCORE_BW_GA, VEKT, DODKAT, KMI_FOER,
            MOR_ROYKTE_FOER_SVSK, KSNITT, KSNITT_PLANLAGT,
            PARITET, # PARITET_5
            DODFODTE_5,  MISD, NEVRALRORSDEFEKTER,
            SPINAB, ENCEPH, ANENCEPH, LEPPE_LEPPEGANESPALTE, 
            GANESPALTE, HJERTE_MISD, GASTROS, OMPHALO, KLUMPFOT,
            KROMOSOMFEIL, DOWNS,
            BARNETS_HELSE,
            KJONN)]

str(df)
df <- unique(df)
# define PARITET_5 ourselves and remove PARITET
df$PARITET_5 <- ""
df[PARITET==0]$PARITET_5 <- "0"
df[PARITET==1]$PARITET_5 <- "1"
df[PARITET==2]$PARITET_5 <- "2"
df[PARITET==3]$PARITET_5 <- "3"
df[PARITET>=4]$PARITET_5 <- "4"
table(df$PARITET_5)

# remove PARITET
df <- df[,PARITET:=NULL]
df <- data.table(df)
# PI want us to impute 0 for all NA outcomes
## check the if the missing is NA or O
sum(is.na(df[,.(PREEKL, DIABETES_MELLITUS, HYPERTENSJON_ALENE, PLACENTA_PREVIA,
            BLODN_F13, BLODN_13_28, BLODN_E28, BLODNING_O500,
            APGAR5, ZSCORE_BW_GA, VEKT, DODKAT, KMI_FOER,
            MOR_ROYKTE_FOER_SVSK, KSNITT, KSNITT_PLANLAGT,
            PARITET_5,
            DODFODTE_5, MISD, NEVRALRORSDEFEKTER,
            SPINAB, ENCEPH, ANENCEPH, LEPPE_LEPPEGANESPALTE, 
            GANESPALTE, HJERTE_MISD, GASTROS, OMPHALO, KLUMPFOT,
            KROMOSOMFEIL, DOWNS)])) # 16458377
sum(df[,.(PREEKL, DIABETES_MELLITUS, HYPERTENSJON_ALENE, PLACENTA_PREVIA,
       BLODN_F13, BLODN_13_28, BLODN_E28, BLODNING_O500,
       APGAR5, ZSCORE_BW_GA, VEKT, DODKAT, KMI_FOER,
       MOR_ROYKTE_FOER_SVSK, KSNITT, KSNITT_PLANLAGT,
       PARITET_5,
       DODFODTE_5,  MISD, NEVRALRORSDEFEKTER,
       SPINAB, ENCEPH, ANENCEPH, LEPPE_LEPPEGANESPALTE, 
       GANESPALTE, HJERTE_MISD, GASTROS, OMPHALO, KLUMPFOT,
       KROMOSOMFEIL, DOWNS)]=="") # NA

## Now assign 0 to NA

## mothers' outcomes
df$BLODN_F13 <- ifelse(is.na(df$BLODN_F13), 0, df$BLODN_F13)
df$BLODN_13_28 <- ifelse(is.na(df$BLODN_13_28), 0, df$BLODN_13_28)
df$BLODN_E28 <- ifelse(is.na(df$BLODN_E28), 0, df$BLODN_E28)

df$HYPERTENSJON_ALENE <- ifelse(is.na(df$HYPERTENSJON_ALENE), 0, df$HYPERTENSJON_ALENE)
df$PLACENTA_PREVIA <- ifelse(is.na(df$PLACENTA_PREVIA), 0, df$PLACENTA_PREVIA)

df$PREEKL <- ifelse(is.na(df$PREEKL), 0, df$PREEKL)
df$BLODNING_O500 <- ifelse(is.na(df$BLODNING_O500), 0, df$BLODNING_O500)

df$DIABETES_MELLITUS <- ifelse(is.na(df$DIABETES_MELLITUS), 0, df$DIABETES_MELLITUS)
## children's outcomes
df$MISD <- ifelse(is.na(df$MISD), 0, df$MISD)
df$DOWNS <- ifelse(is.na(df$DOWNS), 0, df$DOWNS)
df$NEVRALRORSDEFEKTER <- ifelse(is.na(df$NEVRALRORSDEFEKTER), 0, df$NEVRALRORSDEFEKTER)
df$SPINAB <- ifelse(is.na(df$SPINAB), 0, df$SPINAB)
df$ENCEPH <- ifelse(is.na(df$ENCEPH), 0, df$ENCEPH)
df$ANENCEPH <- ifelse(is.na(df$ANENCEPH), 0, df$ANENCEPH)
df$LEPPE_LEPPEGANESPALTE <- ifelse(is.na(df$LEPPE_LEPPEGANESPALTE), 0, df$LEPPE_LEPPEGANESPALTE)
df$GANESPALTE <- ifelse(is.na(df$GANESPALTE), 0, df$GANESPALTE)
df$HJERTE_MISD <- ifelse(is.na(df$HJERTE_MISD), 0, df$HJERTE_MISD)
df$GASTROS <- ifelse(is.na(df$GASTROS), 0, df$GASTROS)
df$OMPHALO <- ifelse(is.na(df$OMPHALO), 0, df$OMPHALO)
df$KLUMPFOT <- ifelse(is.na(df$KLUMPFOT), 0, df$KLUMPFOT)
df$KROMOSOMFEIL <- ifelse(is.na(df$KROMOSOMFEIL), 0, df$KROMOSOMFEIL)


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
df1$fdato_mor <- df1$FDATO_DIFFERANSEDAGER_MOR + df1$ref_date
summary(df1$fdato_mor)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "2009-12-17" "2013-02-12" "2016-04-15" "2016-05-06" "2019-07-19" "2023-01-14"       "8" 

# remove rows without mothers' delivery dates
df1 <- df1[!is.na(fdato_mor)]
summary(df1$fdato_mor)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2009-12-17" "2013-02-12" "2016-04-15" "2016-05-06" "2019-07-19" "2023-01-14" 
df1 <- df1[, ref_date:=NULL]

head(ref_date)
setnames(ref_date, "ID_MOR", "ID_BARN")
df1$ID_BARN <- as.integer(df1$ID_BARN)
df1 <- merge(df1, ref_date, by = "ID_BARN", all.x = TRUE)
df1$fdato_barn <- df1$FDATO_DIFFERANSEDAGER_BARN + df1$ref_date
summary(df1$fdato_barn)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "2010-01-15" "2013-02-15" "2016-04-15" "2016-05-07" "2019-07-15" "2022-12-15"      "10823" 
sum(is.na(df1$ID_BARN))
# NA: 10789
10822-10789 # 33

## children without own birth date but with ID, 
nrow(df1[is.na(fdato_barn) & !is.na(ID_BARN)]) # 33 
## children without own birth day without ID
nrow(df1[is.na(fdato_barn) & is.na(ID_BARN)]) # 10788
# 34 children has ID but without fdato_barn
# 10788 chidren has neither ID nor fdato_barn
# shall check if the 34 has birth date in PERSONS
persons <- fread("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/PERSONS_FASTE_OPPL.csv", colClasses = "character")
p <- persons
p$birth_date <- as.Date(paste0(p$year_of_birth, "-", 
                               p$month_of_birth, "-", 
                               p$day_of_birth))
str(p)
sum(is.na(p$birth_date)) # 0
p <- p[,.(person_id, birth_date)]
setnames(p, "person_id", "ID_BARN")
p$ID_BARN <- as.integer(p$ID_BARN)
df1 <- merge(df1, p, by = "ID_BARN", all.x = TRUE)
str(df1)
summary(df1[is.na(fdato_barn) & !is.na(ID_BARN)]$birth_date)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2010-03-14" "2012-11-23" "2013-11-29" "2014-04-16" "2015-05-09" "2020-05-13" 
# these children's id are not captured by PERSONS
children_id_NA_fdato <- unique(df1[is.na(fdato_barn) & !is.na(ID_BARN)]$ID_BARN)
length(setdiff(children_id_NA_fdato, unique(p$ID_BARN))) # 34 correct
# these chidrens' ID_BARN need to be added to PERSONS and OP table

# assign fdato_mor to birth date and fdato_barn
mbrn_children_id_to_add <- 
  df1[is.na(fdato_barn) & !is.na(ID_BARN)]
mbrn_children_id_to_add$birth_date <- 
  mbrn_children_id_to_add$fdato_mor
mbrn_children_id_to_add$fdato_barn <- 
  mbrn_children_id_to_add$fdato_mor

mbrn_children_impute_id_to_add <- 
  df1[is.na(fdato_barn) & is.na(ID_BARN)]
mbrn_children_impute_id_to_add$birth_date <- 
  mbrn_children_impute_id_to_add$fdato_mor
mbrn_children_impute_id_to_add$fdato_barn <- 
  mbrn_children_impute_id_to_add$fdato_mor 
## impute ID for children without ID_BARN
mbrn_children_impute_id_to_add$ID_BARN <- 
  5900000 + as.integer(rownames(mbrn_children_impute_id_to_add))

mbrn_rest <- 
  df1[!is.na(fdato_barn)]
## if fdato_barn isn't the same as birth_date, 
## replace fdato_barn with birth_date
mbrn_rest[!mbrn_rest$fdato_barn == mbrn_rest$birth_date]$fdato_barn <- 
  mbrn_rest[!mbrn_rest$fdato_barn == mbrn_rest$birth_date]$birth_date
summary(mbrn_rest$birth_date)
str(mbrn_rest)
mbrn_rest$birth_date <- as.IDate(mbrn_rest$birth_date)
str(mbrn_rest)

persons_mbrn <- mbrn_children_impute_id_to_add[,.(ID_BARN,
                                                  birth_date,
                                                  KJONN)]
library(stringr)
persons_mbrn$KJONN <- as.character(persons_mbrn$KJONN)
persons_mbrn$KJONN <- str_replace_all(persons_mbrn$KJONN, 
                                      c("1"="M", 
                                  "2"="F", 
                                  "3"="O", 
                                  "9"="U", 
                                  "0"="U"))
setnames(persons_mbrn, c("ID_BARN", "KJONN"), c("person_id", "sex_at_instance_creation"))

str(persons_mbrn)
p <- persons_mbrn
p$day_of_birth <- NA
p$month_of_birth <- substr(as.character(p$birth_date), 6,7)
p$year_of_birth <- substr(as.character(p$birth_date), 1,4)
str(p)
p$day_of_death <- p$day_of_birth
p$month_of_death <- p$month_of_birth
p$year_of_death <- p$year_of_birth
p$race <- NA
p$quality <- "reliable"
p$country_of_birth <- "Norway"

p <- p[,.(person_id,
          day_of_birth,
          month_of_birth,
          year_of_birth,
          day_of_death,
          month_of_death,
          year_of_death,
          sex_at_instance_creation,
          race,
          country_of_birth,
          quality)]

fwrite(p, "PERSONS_MBRN.csv")

prs <- fread("PERSON_RELATIONSHIPS.csv")
summary(prs$related_id)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   27 1440372 2889787 2900691 4354260 5814479    1737 
to_prs <- rbind(mbrn_children_id_to_add,
                mbrn_children_impute_id_to_add,
                mbrn_rest
                )[,.(ID_MOR, ID_BARN)]
setnames(to_prs, c("ID_MOR", "ID_BARN"), c("related_id", "person_id"))
to_prs <- to_prs[,.(person_id,
                    related_id)]
to_prs$meaning_of_relationship <- "birth_mother"
to_prs$origin_of_relationship <- "MBRN"
to_prs$method_of_linkage <- "deterministic"
str(to_prs)
to_prs$person_id <- as.character(to_prs$person_id)
to_prs$related_id <- as.character(to_prs$related_id)
to_prs <- to_prs[,.(person_id,
                    related_id,
                    meaning_of_relationship,
                    origin_of_relationship,
                    method_of_linkage)]
to_prs <- unique(to_prs)
sum(is.na(to_prs$related_id)) # 0
sum(to_prs$related_id=="") # 0
sum(is.na(to_prs$person_id)) # 0
sum(to_prs$person_id == "")
summary(as.integer(to_prs$related_id)) # mothers id
summary(as.integer(to_prs$person_id)) # children id
# add to PRs
fwrite(to_prs, "PERSON_RELATIONSHIPS.csv")



# add to OP
to_persons_mbrn$op_start_date <- to_persons_mbrn$birth_date
to_persons_mbrn$op_end_date <- to_persons_mbrn$birth_date
setnames(to_persons_mbrn, "ID_BARN", "person_id")
to_op <- to_persons_mbrn[,.(person_id,
                            op_start_date,
                            op_end_date)]
to_op$op_start_date <- gsub("-", "", to_op$op_start_date)
to_op$op_end_date <- gsub("-", "", to_op$op_end_date)
to_op$op_meaning <- "child_from_birth_registry_not_in_population_registry"
to_op$op_origin <- "MBRN"
str(to_op)
to_op$person_id <- as.character(to_op$person_id)

fwrite(to_op, "OBSERVATION_PERIODS_MBRN.csv")
summary(as.integer(to_op$person_id))
op_orig <- fread("OBSERVATION_PERIODS_SSB.csv")


df1 <- rbind(mbrn_children_id_to_add,
             mbrn_children_impute_id_to_add,
             mbrn_rest)


gc()

df <- df1
############################
# SURVEY_ID
############################
si <- df[,.(ID_MOR,
            ID_BARN,
            survey_id,
            fdato_mor)]
setnames(si, "fdato_mor", "survey_date")

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
nrow(si[person_id == ""]) #0
sum(is.na(si$person_id)) # 0
si$survey_date <- gsub("-", "", si$survey_date)
str(si)
si <- unique(si)
fwrite(si,"/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/SURVEY_ID.csv")
rm(si)
gc()

############################
# PERSON_RELATIONSHIPS
############################

# pr <- df[,.(ID_MOR,
#             ID_BARN)]
# setnames(pr, "ID_BARN", "person_id")
# pr$meaning_of_relationship <- "birth_mother"
# pr$origin_of_relationship <- "MBRN"
# pr$method_of_linkage <- "deterministic"
# setnames(pr, "ID_MOR", "related_id")
# str(pr)
# pr <- pr[,.(person_id,
#             related_id,
#             meaning_of_relationship,
#             origin_of_relationship,
#             method_of_linkage)]
# pr <- unique(pr)
# fwrite(pr, "PERSON_RELATIONSHIPS.csv")
# rm(pr)
# gc()
############################
# SURVEY_OBSERVATIONS
############################
str(df)

so_mor <- df[,.(ID_MOR,
                fdato_mor,
                SVLEN_DG,
                PREEKL,
                HYPERTENSJON_ALENE,
                DIABETES_MELLITUS,
                BLODN_F13,
                BLODN_13_28,
                BLODN_E28,
                BLODNING_O500,
                KMI_FOER,
                MOR_ROYKTE_FOER_SVSK,
                KSNITT,
                KSNITT_PLANLAGT,
                PARITET_5,
                DODFODTE_5,
                PLACENTA_PREVIA,
                survey_id)]


setnames(so_mor, "ID_MOR", "person_id")
so_mor$FDATO_DIFFERANSEDAGER_MOR <- gsub("-", "", so_mor$fdato_mor)
so_mor <- melt(so_mor, id.vars = c("person_id", "fdato_mor", "survey_id"))
head(so_mor)


setnames(so_mor, c("variable", "value", "fdato_mor"), 
         c("so_source_column", "so_source_value", "so_date"))
so_mor$so_date <- gsub("-", "", so_mor$so_date)
so_mor$so_source_column <- as.character(so_mor$so_source_column)
so_mor <- data.table(so_mor)
so_mor <- so_mor[!is.na(so_source_value) & !so_source_value=="" & !so_source_value=="-"]

so_mor$so_source_table <- "MBRN"
so_mor$so_unit <- ""
so_mor[so_source_column=="SVLEN_DG"]$so_unit <- "days"

so_mor[so_source_column=="KMI_FOER"]$so_unit <- "kg/m2"

str(so_mor)
so_mor[so_unit==""]$so_unit <- NA
so_mor$so_meaning <- "birth_registry_mother"
so_mor$so_origin <- "MBRN"

str(so_mor)
so_mor <- unique(so_mor)
so_mor <- so_mor[,.(person_id,
            so_date,
            so_source_table,
            so_source_column,
            so_source_value,
            so_unit,
            so_meaning,
            so_origin,
            survey_id)]
fwrite(so_mor, "/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/SURVEY_OBSERVATIONS_MOTHER.csv")
rm(so_mor)
gc()

df <- df[,.(ID_BARN,
            fdato_mor, # as so_date 2023.10.02
            SVLEN_DG,
            APGAR5,
            ZSCORE_BW_GA,
            VEKT,
            DODKAT,
            MISD,
            NEVRALRORSDEFEKTER,
            SPINAB,
            ENCEPH,
            ANENCEPH,
            LEPPE_LEPPEGANESPALTE,
            GANESPALTE,
            HJERTE_MISD,
            GASTROS,
            OMPHALO,
            KLUMPFOT,
            KROMOSOMFEIL,
            DOWNS,
            BARNETS_HELSE,
            survey_id)]

setnames(df, c("ID_BARN", "fdato_mor"), c("person_id", "so_date"))
df$so_date <- gsub("-", "", df$so_date)
gc()
df <- df %>% 
  separate_rows(BARNETS_HELSE, sep = ";")

sum(is.na(df$BARNETS_HELSE))
sum(df$BARNETS_HELSE=="") # 747648

df <- data.table(df)
df[BARNETS_HELSE==""]$BARNETS_HELSE <- "-_-_-_-"
df$BARNETS_HELSE <- matrix(unlist(str_split(df$BARNETS_HELSE, "_")),
                                    ncol=4, byrow = TRUE)[,2]



str(df)

so <- df
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


str(so)
so[so_unit==""]$so_unit <- NA
so$so_meaning <- "birth_registry_child"
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
so <- unique(so)
fwrite(so, "SURVEY_OBSERVATIONS_CHILDREN.csv")
rm(df,so)
gc()



summary(df11$so_date)
# add these BARN to PERSONS and OP
summary(df11$ID_BARN)
so_added <- so[person_id %in% unique(df11$ID_BARN)]
head(so_added)
table(so_added[so_source_column == "DODKAT"]$so_source_value)

str(df11)
table(df11[year(so_date)> 2019]$DODKAT)
