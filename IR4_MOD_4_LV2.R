TSD_install_path <- "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux/" 
library(data.table)
library(dplyr, lib.loc = TSD_install_path)
library(purrr, lib.loc = TSD_install_path)

########################################
## 100k_new 2.3, 2.4
## 2.4 is false flag
## 2.3 skip checking
#########################################
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
df <- fread("EVENTS_Tilstandskoder_SOM.csv")
out <- df[meaning_of_event == ""]

########################################
## 100k_new 2.3, 2.4
## 2.4 is false flag
## 2.3 skip checking
#########################################
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
p <- fread("PERSONS_FASTE_OPPL.csv")
pid <- unique(p$person_id)
p1 <- fread("PERSONS_MBRN.csv")
p1id <- unique(p1$person_id)
pid <- union(pid, p1id)

so <- fread("SURVEY_OBSERVATIONS_MOTHER.csv")
motherid <- unique(so$person_id)
length(setdiff(motherid, pid))

so <- fread("SURVEY_OBSERVATIONS_CHILDREN.csv")
childrenid <- unique(so$person_id)
length(setdiff(childrenid, pid))

sid <- fread("SURVEY_ID.csv")
sid <- unique(sid$person_id)
length(setdiff(sid, pid))

# false flag


########################################
## 100k_new 2.3, 2.6
## outside of op, after visit_end_date
#########################################
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_100k_new/")
vo <- list.files(pattern = "VISIT_OCCURRENCE_Akt")
for ( i in seq_along(vo) ){
  df0 <- fread(vo[i])
  df0 <- df0[meaning_of_visit == "outpatient_contact"]
  if ( i == 1 ) df <- df0
  else df <- rbind(df, df0)
}

ev <- list.files(pattern = "EVENTS_Til")
for ( i in seq_along(ev) ){
  tb0 <- fread(ev[i])
  tb0 <- tb0[grepl("outpatient_contact", meaning_of_event)]
  if ( i == 1 ) tb <- tb0
  else tb <- rbind(tb, tb0)
}

events <- merge(df, tb, by = "visit_occurrence_id")
head(events)
nrow(events[start_date_record > visit_end_date]) # 15
wrong <- events[start_date_record > visit_end_date]
table(wrong$origin_of_event)
# Tilstandskoder_AVT 
# 15 
table(wrong$origin_of_visit)
# Akt_AVT 
# 15
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
df <- fread("VISIT_OCCURRENCE_Akt_AVT.csv")
nrow(df[visit_start_date > visit_end_date]) # 1654
wrong <- df[visit_start_date > visit_end_date]
length(unique(wrong$visit_occurrence_id)) # 1654
tb <- fread("EVENTS_Tilstandskoder_AVT.csv")
length(unique(tb[start_date_record > end_date_record]$visit_occurrence_id)) # 427

# check raw
avt <- fread("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/NPR/22_8877_Aktivitetsfil_AVT.csv")
som <- fread("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/NPR/22_8877_Aktivitetsfil_SOM.csv")
tsb <- fread("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/NPR/22_8877_Aktivitetsfil_TSB.csv")
nrow(avt[differansedager_InnDato > differansedager_UtDato]) # 3558
nrow(som[differansedager_InnDato > differansedager_UtDato]) # 0
nrow(tsb[differansedager_InnDato > differansedager_UtDato]) # 6

# remove visit_end_date and end_date_record
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
df <- fread("VISIT_OCCURRENCE_Akt_AVT.csv")
df$visit_end_date <- NA
fwrite(df, "VISIT_OCCURRENCE_Akt_AVT.csv")

df <- fread("VISIT_OCCURRENCE_Akt_TSB.csv")
df$visit_end_date <- NA
fwrite(df, "VISIT_OCCURRENCE_Akt_TSB.csv")

df <- fread("EVENTS_Tilstandskoder_AVT.csv")
df$end_date_record <- NA
fwrite(df, "EVENTS_Tilstandskoder_AVT.csv")

df <- fread("EVENTS_Tilstandskoder_TSB.csv")
df$end_date_record <- NA
fwrite(df, "EVENTS_Tilstandskoder_TSB.csv")

meta <- fread("METADATA.csv")

##################################
## FULL 2.3
## outside of op
###################################
## EVENTS_DAR, 2052
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
op <- fread("OBSERVATION_PERIODS.csv")

summary(op$op_start_date)
summary(op$op_end_date)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 19090915 19650715 19901215 19882641 20091012 20230113 
run.seq <- function(x) as.numeric(ave(paste(x), x, FUN = seq_along))


df <- fread("EVENTS_DAR.csv")
df <- df[,.(person_id, start_date_record)]

# create a function to check if a date is within a range of dates
is_within_range <- function(x, ranges) {
  purrr::map_lgl(x, ~any(ranges$op_start_date <= .x & .x <= ranges$op_end_date))
}
library(data.table)

op_dt <- as.data.table(op)
df_dt <- as.data.table(df)

op_dt[, c("op_start_date", "op_end_date") := lapply(.SD, as.Date), .SDcols=2:3]
df_dt[, "start_date_record" := as.Date(start_date_record)]

# subset df using non-equi join condition
result <- df_dt[op_dt, on = .(person_id, start_date_record < op_start_date, start_date_record > op_end_date), nomatch = 0L, mult = 'last', .(person_id, start_date_record, op_start_date, op_end_date)]

op <- op[,.(person_id, op_start_date, op_end_date)]
# join the two data frames together by person_id
merged <- merge(op, df, by = "person_id")

# check if each date in the merged data frame is within a date range for that person
merged$within_range <- is_within_range(merged$start_date_record, merged[,2:3])

# exclude dates that are within one of the date ranges
result <- merged[!(merged$within_range & duplicated(merged$person_id)), c("person_id", "start_date_record")]

result
# person_id start_date_record op_start_date op_end_date     op_meaning op_origin
#   2013666          20211210      19820515    20170612 legal_resident       SSB
#   2013666          20211210      20210130    20211209 legal_resident       SSB
#   4914136          20200902      19410815    20120424 legal_resident       SSB
#   4914136          20200902      20191103    20200901 legal_resident       SSB
run.seq <- function(x) as.numeric(ave(paste(x), x, FUN = seq_along))
## EVENTS_KUHR, 948679
files <- list.files(pattern = "EVENTS_KUHR")
for (i in seq_along(files)){
  df <- fread(files[i])
  df <- df[,.(person_id, start_date_record)]
  L <- list(df, op)
  L2 <- lapply(L, function(x) cbind(x, run.seq = run.seq(x$person_id)))
  out <- Reduce(function(...) merge(..., all = TRUE), L2)[-2]
  out <- out[!is.na(start_date_record)]

  
  outside_op <- out[start_date_record > op_end_date |
                        start_date_record < op_start_date] #
  
  outside_op <- out[start_date_record > op_end_date |
                        start_date_record < op_start_date] #
  if (i == 1) outside_op_all <- outside_op
  else outside_op_all <- rbind(outside_op, outside_op_all)
}


  
outside_op[person_id %in% outside_op_dup$person_id]
# person_id start_date_record op_start_date op_end_date     op_meaning op_origin
#       44          20220204      20220223          NA legal_resident       SSB  born in 2000
#       44          20220207      20220223          NA legal_resident       SSB
#     1062          20220121      20220126          NA legal_resident       SSB  born in 2004
#     1062          20220117      20220126          NA legal_resident       SSB
#     1062          20220116      20220126          NA legal_resident       SSB
# ---                                                                               
#   5813616          20221102      19941215    20160306 legal_resident       SSB in the 2nd period 
#   5814213          20211220      19981215    20180816 legal_resident       SSB in the 2nd period
#   5814213          20211220      19981215    20180816 legal_resident       SSB in the 2nd period
#   5814317          20220717      20221128          NA legal_resident       SSB in the 1st period
#   5814317          20220717      20221128          NA legal_resident       SSB in the 1st period

##################################
## FULL 2.3, 2.4, 2.6
## outside op dates. more than on op
## remove ids not in PERSONS, tolerable %
## after visit end date, tolerable %
###################################
## 2.3, EVENTS, primary_care 216372
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
op <- fread("OBSERVATION_PERIODS.csv")
summary(op$op_start_date)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 19090915 19650715 19901215 19882457 20091015 20230113 
summary(op$op_end_date)
kuhr <- fread(list.files(pattern = "ENTS_K"))

kop <- merge(kuhr, op, by = "person_id", all.x = TRUE)
b4 <- kop[meaning_of_event=="primary_care" & 
           (start_date_record < op_start_date)] # 3653

aft <- kop[meaning_of_event=="primary_care" & 
             (start_date_record > op_end_date)] # 212719

nrow(b4)+nrow(aft) # 216372 bingo

## remove IDs
id <- unique(fread("PERSONS.csv")$person_id)

tb <- fread(list.files(pattern = "SSB_SIV")) 
old <- nrow(tb) # 16180169
tb_id <- unique(tb$person_id)
de_id <- unique(setdiff(tb_id, id))
tb <- tb[!person_id %in% de_id] 
nrow(tb) # 16179955
fwrite(tb, list.files(pattern = "SSB_SIV"))

tb <- fread(list.files(pattern = "BOSTED")) 
old <- nrow(tb) # 16180677
tb_id <- unique(tb$person_id)
de_id <- unique(setdiff(tb_id, id))
tb <- tb[!person_id %in% de_id] 
nrow(tb) # 16180461
fwrite(tb, list.files(pattern = "BOSTED"))


tb <- fread(list.files(pattern = "_ID")) 
old <- nrow(tb) 
old # 1477140(new)   1473734
tb_id <- unique(tb$person_id)
de_id <- unique(setdiff(tb_id, id))
tb <- tb[!person_id %in% de_id] 
nrow(tb) # 1475395(new)  1473692
fwrite(tb, list.files(pattern = "_ID"))

tb <- fread(list.files(pattern = "SURVEY_OBSERVATIONS")) 
old <- nrow(tb) 
old # 25540022(new)    25480875
tb_id <- unique(tb$person_id)
de_id <- unique(setdiff(tb_id, id))
tb <- tb[!person_id %in% de_id] 
nrow(tb) # 25479403(new)    25480598
fwrite(tb, list.files(pattern = "SURVEY_OBSERVATIONS"))

files <- list.files(pattern = "EVENTS_NPR")
for (name in files){
  tb <- fread(name) 
  old <- nrow(tb) 
  old # 
  tb_id <- unique(tb$person_id)
  de_id <- unique(setdiff(tb_id, id))
  tb <- tb[!person_id %in% de_id] 
  new <- nrow(tb) # 
  print(paste(old-new, "number of rows were removed from ", name, "."))
  fwrite(tb, name)
}
# for the new data
# "264 number of rows were removed from  EVENTS_NPR_AVT.csv ."
# "524 number of rows were removed from  EVENTS_NPR_SOM.csv ."
# "193 number of rows were removed from  EVENTS_NPR_TSB.csv ."

files <- list.files(pattern = "VISIT")
for (name in files){
  tb <- fread(name) 
  old <- nrow(tb) 
  old # 
  tb_id <- unique(tb$person_id)
  de_id <- unique(setdiff(tb_id, id))
  tb <- tb[!person_id %in% de_id] 
  new <- nrow(tb) # 
  print(paste(old-new, "number of rows were removed from ", name, "."))
  fwrite(tb, name)
}
# for the new data
## KURH is still fine with 13 years data
## no rows removed from NPR either.


  files <- list.files(pattern = "MEDICINES")
  for (name in files){
    tb <- fread(name) 
    old <- nrow(tb) 
    old # 
    tb_id <- unique(tb$person_id)
    de_id <- unique(setdiff(tb_id, id))
    tb <- tb[!person_id %in% de_id] 
    nrow(tb) # 
    fwrite(tb, name) 
  
}



##################################
# checking 2.5
## Q: total = 0???
## A: 0 because no visit_occurrence_id
###################################
setwd("N:/durable/vac4eu/CDMInstances/vac4eu_10k/")
mos <- list.files(pattern = "CAL_OB")
i=2
df <- str(fread(mos[i]))

df <- str(fread(list.files(pattern = "ENTS_D")))

df <- str(fread(list.files(pattern = "VACC")))

##################################
# checking 2.3
# date value outside op dates
## need to assgin 20230114 to op_end_date
###################################
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_10k/")
op <- fread(list.files(pattern = "VATION_PE"))
ins <- fread("INSTANCE.csv")
# put 
op[is.na(op_end_date)]$op_end_date <- max(ins$up_to_when_data_complete)
fwrite(op, list.files(pattern = "VATION_PE"))


setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
op <- fread(list.files(pattern = "VATION_PE"))
ins <- fread("INSTANCE.csv")
# put 
op[is.na(op_end_date)]$op_end_date <- max(ins$up_to_when_data_complete)
fwrite(op, list.files(pattern = "VATION_PE"))

##################################
# checking 2.1 
# date value before birth
# gross_income 376 
# due to imputed datas
###################################
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
files <- list.files(pattern = "INNT")

df <- fread(files, colClasses = "character")
df <- df[,.(person_id, mo_date)]
df$person_id <- as.integer(df$person_id)
df$mo_date <- as.Date(df$mo_date, "%Y%m%d", "%Y-%m-%d")
p <- fread("PERSONS.csv", colClasses = "character")
unique(p$month_of_birth)
p$month_of_birth <- ifelse(nchar(p$month_of_birth)==3, 
                           substr(p$month_of_birth, 2,3), 
                           p$month_of_birth)
p<- p[,.(year_of_birth, month_of_birth, day_of_birth, person_id)]
p$birth_date <- as.Date(paste0(p$year_of_birth, "-", p$month_of_birth, "-", p$day_of_birth))
p <- p[,.(person_id, birth_date)]
p$person_id <- as.integer(p$person_id)
df <- merge(df, p, by = "person_id", all.x = TRUE)
df$diff <- as.integer(df$mo_date - df$birth_date)

nrow(df[is.na(birth_date)]) # 296
length(setdiff(unique(df$person_id), unique(p$person_id))) # 77

df <- fread(files, colClasses = "character")
str(df)
be4 <- nrow(df) # 21628736
df <- df[!as.integer(person_id) %in% NA_birth_id] 
aft <- nrow(df)
be4- aft # 296 GOOD
str(df)
fwrite(df, files)

df <- df[,.(person_id, mo_date)]
df$person_id <- as.integer(df$person_id)
df$mo_date <- as.Date(df$mo_date, "%Y%m%d", "%Y-%m-%d")

df <- merge(df, p, by = "person_id", all.x = TRUE)
df$diff <- as.integer(df$mo_date - df$birth_date)
sum(df[!is.na(diff)]$diff<0) # 219134

be4_birth <- df[!is.na(diff) & diff<0]
str(be4_birth)
table(be4_birth$diff)
# -411  -349  -348  -319  -318  -288  -287  -258  -257  -227  -226  -196  -195  -166  -165  -135  -134  -105  -104   -74   -73   -45   -14 
#    2  7272  7672  7618  8380  8758  9234  8826  9604  9554 10468  9856 10452  9614 10030  9432  9916  9104  9430  8982  9760 17204 17966 

#############
#############

setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_10k/")
files <- list.files(pattern = "INNT")

df <- fread(files, colClasses = "character")
df <- df[,.(person_id, mo_date)]
df$person_id <- as.integer(df$person_id)
df$mo_date <- as.Date(df$mo_date, "%Y%m%d", "%Y-%m-%d")
p <- fread("PERSONS.csv", colClasses = "character")
unique(p$month_of_birth)
p$month_of_birth <- ifelse(nchar(p$month_of_birth)==3, 
                           substr(p$month_of_birth, 2,3), 
                           p$month_of_birth)
p<- p[,.(year_of_birth, month_of_birth, day_of_birth, person_id)]
p$birth_date <- as.Date(paste0(p$year_of_birth, "-", p$month_of_birth, "-", p$day_of_birth))
p <- p[,.(person_id, birth_date)]
p$person_id <- as.integer(p$person_id)
df <- merge(df, p, by = "person_id", all.x = TRUE)
df$diff <- as.integer(df$mo_date - df$birth_date)

sum(df$diff<0) # 376 


##################################
# remove ref_date from EVENTS_DAR
# and reorder perhaps
###################################
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu_10k/")
files <- list.files(pattern = ".csv")
df <- fread(files[grepl("_DAR", files)])
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
fwrite(df, files[grepl("_DAR", files)])

setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
files <- list.files(pattern = ".csv")
df <- fread(files[grepl("_DAR", files)])
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
fwrite(df, files[grepl("_DAR", files)])
summary(df$start_date_record)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 20181216 20191005 20200706 20200788 20210410 20220114 
library(haven, lib.loc = TSD_install_path)


library(tzdb, lib.loc = TSD_install_path)
library(readr, lib.loc = TSD_install_path)
raw <- read_sav("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/Cause of death registry/DAARdata_220919_inkl2022.sav")
head(raw)
summary(raw$DAAR)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2017    2019    2020    2020    2021    2022 