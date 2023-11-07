# CHECK THE DATASETS OF 2022

ref_date <- as.Date("1902-05-16")
library(data.table)
df <- fread("N:\\durable\\VAC4EU datasets\\Delivery May-Aug 2022\\FHI\\MBRN\\MFRdata_P222359_KOBLET_08062022.csv")
# 690512

# df1 <- fread("N:\\durable\\VAC4EU datasets\\Delivery May-Aug 2022\\FHI\\MBRN\\MFRdata_P222359_KOBLET_08062022_shfited.csv")
# Note: this is a fix if R loads data with an additional column
# Change incorrect_col_nums as necessary
# Comment away as necessary 
incorrect_col_nums = 80
if (ncol(df) == incorrect_col_nums){
  correct_names <- names(df)[2:ncol(df)]
  df[[ncol(df)]] <- NULL
  setnames(df, correct_names)
}
# 
# [1] "LNR_MOR"                "LNR_BARN"               "LNR_FAR"                "DODKAT"                
# [5] "SVLEN_ART_DG"           "SPABORT_23_5"           "SVLEN_UL_DG"            "VEKT"                  
# [9] "ABRUPTIOP"              "PARITET_5"              "CPAP"                   "MOR_ROYKTE_FOER_SVSK"  
# [13] "MORS_ALDER"             "MOR_ROYKTE_1_TRIMESTER" "KSNITT"                 "APGAR5"                
# [17] "DODFODTE_5"             "MORS_VEKT_FOER"         "NEVRALRORSDEFEKTER"     "MFMND"                 
# [21] "FARS_ALDER"             "LEVENDEFODTE_5"         "MORS_HOYDE"             "PREEKL"                
# [25] "HJERTE_MISD"            "LENGDE"                 "KROMOSOMFEIL"           "SMENSD_KODE"           
# [29] "DOWNS"                  "ART"                    "DIFF_MFDATO"            "SPABORT_12_5"          
# [33] "SIVST"                  "ZSCORE_BW_GA"           "SVLEN_DG"               "ASTMA"                 
# [37] "MOR_ROYKTE_OPPL"        "KSNITT_TIDLIGERE_MFR"   "DIFF_FDATO"             "PREEKLTIDL"            
# [41] "FMND"                   "BLODN_F13"              "TERMINMETODE"           "PLUREK"                
# [45] "BLODN_E28"              "KJONN"                  "LEPPE_LEPPEGANESPALTE"  "KLUMPFOT"              
# [49] "SETE_ISSE"              "BLODNING_O500"          "PLACENTA_PREVIA"        "HYPERTENSJON_KRONISK"  
# [53] "SVLEN_SM_DG"            "KSNITT_PLANLAGT"        "KSNITT_TIDLIGERE"       "EPILEPSI"              
# [57] "BARNETS_HELSE"          "YRKE_KODE"              "DIABETES_MELLITUS"      "MOR_FAAR"              
# [61] "PRENAT_DIAGNOSTIKK_UTF" "SPINAB"                 "ENCEPH"                 "FAAR"                  
# [65] "OMPHALO"                "BOFYLKE"                "KMI_FOER"               "HYPERTENSJON_ALENE"    
# [69] "APGAR10"                "MISD"                   "PRENAT_PAT_KODER"       "GASTROS"               
# [73] "ANENCEPH"               "EKLAMPSI"               "GANESPALTE"             "HELLP"                 
# [77] "BLODN_13_28"            "PLURAL"                 "DIFF_SMENSD"           
# unique(df$KSNITT)
# unique(df$KSNITT_PLANLAGT)
df <- df[,.(LNR_MOR,LNR_BARN,DIFF_FDATO,DODKAT)]
head(df)
sum(is.na(df$LNR_BARN)) # 10770
sum(is.na(df$LNR_MOR)) # 0
sum(df$LNR_BARN=="") # NA
df <- df[!is.na(LNR_BARN)]
# 0 = Live born, still alive
# 1= Live born, diedwithin 24 hours
# 2 = Live born, died 2.-6. day
# 3 = Live born, died 7.-27.day
# 4 = Live born, died 28.day–1 year of age
# 5 = Live born, died 1-2 years of age
# 6 = Live born, died> 2 years of age
# 7 = Stillborn, died before delivery
# 8 = Stillborn, died during delivery
# 9 = Stillborn, unknown time of death
# 10 = Abortion requiring approval (§2.3c)
# 11 = Live born, unknown follow-up status
# 12 = Live born, emigrated
# 13 = Live born, registered birth certificate
table(df$DODKAT)

dodkat <- df[DODKAT>0 & DODKAT<12,] # 123
nrow(dodkat[(DODKAT>0 & DODKAT<12) & !is.na(LNR_BARN),])
table(dodkat$DODKAT)
# 1  2  3  4  5  6  7 
# 2  2  1 28 16 72  2 
dodkat$delivery_date <- ref_date + dodkat$DIFF_FDATO
summary(dodkat$delivery_date)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2010-03-18" "2014-01-14" "2017-12-28" "2017-02-10" "2019-12-08" "2021-12-22" 
nrow(dodkat[delivery_date >= as.Date("2017-01-01")])
# get death date
ssb <- fread("N:\\durable\\VAC4EU datasets\\Delivery May-Aug 2022\\SSB data\\W22_0605_FASTE_OPPL.csv")
head(ssb)
setnames(dodkat, "LNR_BARN", "lnr")
ssb <- ssb[,.(lnr, doeds_dato_delta)]
dodkat <- merge(dodkat, ssb, by = "lnr", all.x = TRUE)
sum(is.na(dodkat$doeds_dato_delta)) # 31

library(haven)
dar <- death <- read_sav("N:/durable/VAC4EU datasets/Delivery May-Aug 2022/FHI/DAR/DAARdata_220919.sav")
class(dar)
str(dar)
dar <- data.table(dar)
dar <- dar[,.(LOPENR, DIFF_DAGER_DODSDATO, 
              DIAGNOSE_UNDERLIGGENDE_K, TYPE_DIAGNOSE_KODEVERK_K,
              ALDER_AAR)]
setnames(dar, "LOPENR", "lnr")
dodkat <- merge(dodkat, dar, by = "lnr", all.x = TRUE)
sum(is.na(dodkat$DIFF_DAGER_DODSDATO)) # 30

dodkat$lag_dar_ssb <- dodkat$doeds_dato_delta - dodkat$DIFF_DAGER_DODSDATO
summary(dodkat$lag_dar_ssb)
# no lag time
dodkat$lag_death_foed <- dodkat$doeds_dato_delta - dodkat$DIFF_FDATO
summary(dodkat$lag_death_foed)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0   375.8  1063.5  1497.5  2658.0  3978.0      31 

# 0 = Live born, still alive
# 1= Live born, diedwithin 24 hours
# 2 = Live born, died 2.-6. day
# 3 = Live born, died 7.-27.day
# 4 = Live born, died 28.day–1 year of age
# 5 = Live born, died 1-2 years of age
# 6 = Live born, died> 2 years of age
# 7 = Stillborn, died before delivery
# 8 = Stillborn, died during delivery
# 9 = Stillborn, unknown time of death
# 10 = Abortion requiring approval (§2.3c)
# 11 = Live born, unknown follow-up status
# 12 = Live born, emigrated
# 13 = Live born, registered birth certificate
dodkat$dar_dodkatdodkat <- ""
dodkat[lag_death_foed<=1]$dar_dodkatdodkat <- "1"
dodkat[lag_death_foed>1 & lag_death_foed<7 ]$dar_dodkatdodkat <- "2"
dodkat[lag_death_foed>6 & lag_death_foed<28 ]$dar_dodkatdodkat <-"3"
dodkat[lag_death_foed>27 & lag_death_foed<366 ]$dar_dodkatdodkat <- "4"
dodkat[lag_death_foed>365 & lag_death_foed<731 ]$dar_dodkatdodkat <- "5"
dodkat[lag_death_foed>730 ]$dar_dodkatdodkat <- "6"
counts <- data.table(table(dodkat$dar_dodkatdodkat))


table(dodkat[dar_dodkatdodkat==""]$DODKAT)
#  4  5  6  7 
# 10  2 17  2 
 
summary(dodkat[dar_dodkatdodkat==""& DODKAT==4]$delivery_date)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2021-05-23" "2021-06-14" "2021-09-17" "2021-09-01" "2021-10-23" "2021-12-22"
summary(dodkat[dar_dodkatdodkat==""& DODKAT==4]$lag_death_foed)
# NA
summary(dodkat[dar_dodkatdodkat==""& DODKAT==5]$delivery_date)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-12-25" "2021-01-16" "2021-02-08" "2021-02-08" "2021-03-03" "2021-03-26"
summary(dodkat[dar_dodkatdodkat==""& DODKAT==6]$delivery_date)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2011-02-18" "2013-01-06" "2015-12-24" "2015-08-10" "2018-02-15" "2019-06-02"  
summary(dodkat[dar_dodkatdodkat==""& DODKAT==7]$delivery_date)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2014-05-19" "2014-11-07" "2015-04-29" "2015-04-29" "2015-10-18" "2016-04-08" 
