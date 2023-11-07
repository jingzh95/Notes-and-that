# SSB
# /ess/p1921/data
# /ess/p1921/data
TSD_install_path = ifelse(toupper(Sys.info()["sysname"]) == "WINDOWS",
                          "N:/durable/vac4eu/R_v4.1_packages/",
                          "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux/")
setwd("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/SSB/")
origin <- "SSB"
cdm <- "/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/"
cdm_table <- "OBSERVATION_PERIODS.csv"
library(data.table)
library(openxlsx, lib.loc = TSD_install_path)
files <- list.files(pattern = ".csv")
tablename <- files[which((files %like% "FASTE") == TRUE)]
df <- fread(tablename)
tablename <- files[which((files %like% "UTVANDRING") == TRUE)]
df_mig <- fread(tablename)
df <- merge(df, df_mig,  all.x = TRUE)
rm(df_mig)
df <- df[,.(lnr,
            foedsels_aar_mnd,
            KJOENN,
            fodeland,
            invkat,
            foedselsdato_refdato,
            doedsdato_refdato,
            forstdato_refdato,
            fralanddato_refdato,
            tillanddato_refdato)]
gc()
head(df)

df$birth_date <- paste0(df$foedsels_aar_mnd,"15")
df$birth_date <- as.Date(df$birth_date, "%Y%m%d")

df$ref_date <- df$birth_date - df$foedselsdato_refdato

# fwrite(df[,.(lnr, ref_date)], "/ess/p1921/data/durable/vac4eu/ref_date_2023.csv")


df$tillanddato_refdato <- df$ref_date + df$tillanddato_refdato

df$doedsdato_refdato <- df$ref_date + df$doedsdato_refdato


df$fralanddato_refdato <- df$ref_date + df$fralanddato_refdato
df$forstdato_refdato <- df$ref_date + df$forstdato_refdato

setnames(df, c("tillanddato_refdato",
               "fralanddato_refdato",
               "forstdato_refdato",
               "doedsdato_refdato"),
         c("last_emi_date",
           "last_immi_date",
           "first_reg_date",
           "death_date"))

### step 0 ###
### remove death_date and last_emi before 1995
### remove individuals NA in birth_date 
### remove aux vars 
df <- df[year(death_date)>=1995 | is.na(death_date)]
# df <- df[year(last_emi_date)>=1995 | is.na(last_emi_date)]
df <- df[!is.na(birth_date),]                                         # 5814128

df <- df[,c("foedselsdato_refdato",
            "foedsels_aar_mnd",
            "ref_date"):=NULL]

### step 1 ###
### assign very small num to NA for candidates of op_start_date
### assign very large num to NA for candidates of op_end_date
##############################

df[is.na(first_reg_date)]$first_reg_date <- as.Date("1885-01-01")
nrow(df[first_reg_date=="1885-01-01"])                                # 24
df[is.na(last_immi_date)]$last_immi_date <- as.Date("1885-01-01")
nrow(df[last_immi_date=="1885-01-01"])                                # 4525589
df[is.na(death_date)]$death_date <- as.Date("2024-12-31")
nrow(df[death_date=="2024-12-31"])                                    # 5617943
df[is.na(last_emi_date)]$last_emi_date <- as.Date("2024-12-31")
nrow(df[last_emi_date=="2024-12-31"])                                 # 5650936


### remove birth_date == death_date
### remove first_reg_date == death_date
### remove last_immi_date == death_date
### remove last_emi_date == death_date
### remove last_emi_date == last_immi_date
### remove last_emi_date == first_reg_date

# df <- df[!death_date == birth_date]                                   # 5813160
# df <- df[!death_date == first_reg_date]                               # 5813160
# df <- df[!death_date == last_immi_date]                               # 5813160
# df <- df[!last_emi_date == last_immi_date]                            # 5813160
# df <- df[!last_emi_date == first_reg_date]                            # 5813160
# df <- df[!last_emi_date == birth_date]                                # 5813160


### step 2 ###
### assign the maximum of
### birth/first reg./last immi
### to op_start_date
##############################
df$op_start_date <- pmax(df$last_immi_date, 
                           df$first_reg_date, 
                           df$birth_date)
summary(df$op_start_date)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1909-09-15" "1965-07-15" "1990-12-15" "1988-10-02" "2009-10-15" "2023-01-13" 

### step 3 ###
### assign the minimum of
### death/last emi date
### to op_end_date
##############################
df$op_end_date <- pmin(df$death_date,
                      df$last_emi_date)

summary(df$op_end_date)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1964-09-16" "2024-12-31" "2024-12-31" "2024-09-15" "2024-12-31" "2024-12-31" 
df <- df[op_end_date > as.Date("2009-12-15")] # the minimum of "INSTANCE$since_..." is "2009-12-16"

### step 4 ###
### check ind. with start>end
##############################
nrow(df[op_start_date > op_end_date]) # 27930
pot_two_period <- df[op_start_date > op_end_date]

summary(as.numeric(pot_two_period$last_immi_date) - 
          as.numeric(pot_two_period$last_emi_date))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2     487     987    1267    1814    4724  
immi_after_emi <- df[last_immi_date >= last_emi_date] # 31 642
setdiff(as.integer(immi_after_emi$lnr), pot_two_period$lnr) # 0
setdiff(pot_two_period$lnr, as.integer(immi_after_emi$lnr)) # 0

  df1 <- df[op_end_date>=op_start_date]
  df2 <- df[op_end_date<op_start_date]
  nrow(df2[op_end_date == op_start_date]) # 0

     df21 <- df2 
     df22 <- df2

     df21$op_start_date <- df21$birth_date
     df21$op_end_date <- df21$last_emi_date
     summary(as.numeric(df21$op_end_date - df21$op_start_date))
     # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     #   1    7386    9858   10390   13557   33506  
     df22$op_start_date <- df22$last_immi_date
     df22$op_end_date<- df22$death_date
     summary(as.numeric(df22$op_end_date - df22$op_start_date))
     # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     #    1    1050    1411    1409    1772    4974
     
    
  df2 <- rbind(df21, df22)
  df <- rbind(df1, df2)

  
  df[op_end_date == "2024-12-31"]$op_end_date <- NA
  sum(is.na(df$op_end_date)) # 5483086
  sum(df$op_start_date == "1881-01-01") # 0
  df <- unique(df)
  setnames(df, "lnr", "person_id")
  
  op <- df[,.(person_id,
              op_start_date,
              op_end_date)]
  op$op_meaning <- "legal_resident"
  op$op_origin <- origin
  str(op)
  op$op_start_date <- gsub("-", "", op$op_start_date)
  op$op_end_date <- 
    gsub("-", "", op$op_end_date)
  summary(as.numeric(op[!is.na(op_end_date)]$op_end_date))
  #      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  # 20091217 20190412 20200729 20199924 20211025 20230114
  nrow(op) # 5846732
  op <- unique(op)
  nrow(op) # 5837470
  nrow(op[duplicated(person_id)]) # 27935
  # dup_op <- df[duplicated(person_id)] 
# remove op_end_date before 2010
  ins <- fread(paste0(cdm, "INSTANCE.csv"), colClasses = "character")
  end_date <- min(ins$since_when_data_complete)
  end_date
  op <- op[as.numeric(op_end_date) >= as.numeric(end_date) | is.na(op_end_date)]
  summary(as.numeric(op[!is.na(op_end_date)]$op_end_date))
df <- fread("OBSERVATION_PERIODS.csv")
library(data.table)
nrow(df[duplicated(person_id)])
dup_op <- df[duplicated(person_id)]
summary(dup_op[!is.na(op_end_date)]$op_end_date-dup_op[!is.na(op_end_date)]$op_start_date)
sum(is.na(dup_op$op_end_date))
nrow(df[op_end_date < op_start_date])

setwd(cdm)
fwrite(op, cdm_table)

# METADATA and INSTANCE
table <- gsub(".csv", "", cdm_table)
meta <- fread("METADATA.csv")
ins <- fread("INSTANCE.csv")
meta_chunk <- meta[tablename==table]
ins_chunk <- ins[source_table_name == origin]

rm(op)
gc()
## PERSONS ##
# get the country code
country_code <- openxlsx::read.xlsx("/ess/p1921/data/durable/vac4eu/fodelandkoder_ssb.xlsx")
country_code <- data.table(country_code)


persons <- df[,.(lnr, foedsels_aar_mnd, foedselsdato_refdato,
                 doedsdato_refdato, KJOENN, fodeland,
                 invkat)]

setnames(persons, c("lnr","KJOENN", "invkat", "fodeland"),
         c("person_id","sex_at_instance_creation","race", "code"))
persons <- data.table(persons, key = "code")
persons_nocountry <- persons[is.na(code),] # NA for all variables

persons <- persons[country_code]
persons <- persons[!is.na(persons$foedsels_aar_mnd),]

# length(unique(persons$name)) # 228 countries

# get the ref_date for all
persons$birth_date <- paste0(persons$foedsels_aar_mnd,"15")
str(persons)
persons$birth_date <- as.Date(persons$birth_date, "%Y%m%d")
persons$ref_date <- persons$birth_date - persons$foedselsdato_refdato
 ref_date_2023 <- persons[,.(person_id, ref_date)]
 fwrite(ref_date_2023, "/ess/p1921/data/durable/vac4eu/ref_date_2023.csv")
persons$death_date <- persons$ref_date + persons$doedsdato_refdato

persons$day_of_birth <- "15"
persons$month_of_birth <- substr(as.character(persons$foedsels_aar_mnd),5,6)
persons$year_of_birth <- substr(as.character(persons$foedsels_aar_mnd),1,4)

persons$month_of_birth <- substr(as.character(persons$foedsels_aar_mnd),5,6)
persons$year_of_birth <- substr(as.character(persons$foedsels_aar_mnd),1,4)

persons$year_of_death <- as.character(year(persons$death_date))
persons$month_of_death <- as.character(month(persons$death_date))
persons$day_of_death <- substr(as.character(persons$death_date), 9,10)

unique(persons$day_of_death) # check if "03" or "3" e.g

persons$sex_at_instance_creation <- ifelse(persons$sex_at_instance_creation == 1,
                                           "M","F")
persons$quality <- "reliable"
persons$person_id <- as.character(persons$person_id)
persons$country_of_birth <- persons$name
persons_ordered <- persons[,.(person_id,
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

for ( i in seq_along(persons_ordered)){
  print(names(persons_ordered)[i])
  print(sum(is.na(persons_ordered[[i]])))
} # 5617939 alive

for ( i in seq_along(persons_ordered)){
  print(names(persons_ordered)[i])
  print(sum(persons_ordered[[i]]==""))
} 


setwd(cdm)
fwrite(persons_ordered, "PERSONS.csv")
rm(df,op,persons_nocountry,persons_ordered,temp,country_code,first_immi_date,iv,iv_date)
