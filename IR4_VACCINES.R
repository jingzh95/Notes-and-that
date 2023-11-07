library(data.table)
library(vroom) # load strange SYSVAK data
ref_date_2023 <- fread("N:/durable/vac4eu/ref_date_2023.csv")
kob <- fread("N:\\durable\\VAC4EU datasets\\Delivery Feb-May 2023\\National registry/Files/")

# Note: something wrong with formatting of SYSVAK data.
# both base-R (read.csv, read.table) and data.table (fread) 
# have issues loading it.
# Solution: use "vroom" package (tidyverse)

# can set number of lines to read with `n_max`
df <- vroom(file = "N:\\durable\\VAC4EU datasets\\Delivery Feb-May 2023\\SYSVAK\\svkoppdr_p220069_koblet_2023.csv")
df <- setDT(df) # optional: convert to "data.table", beneficial (faster) if using data.table functions

nrow(df)
# 48,254,610
df <- unique(df) # 31,555,251
nrow(df)
head(df)
unique(df$PREPARATBESKRIVELSE)

df <- df[,.(KOBLINGSNOEKKEL, 
            REGISTRERINGSDATO_DIFF, 
            VAKSINASJONSDATO_DIFF, 
            VAKSINEKODE,
            PREPARATBESKRIVELSE)]
gc()


setnames(df, c("KOBLINGSNOEKKEL", "REGISTRERINGSDATO_DIFF", 
               "VAKSINASJONSDATO_DIFF",
               "VAKSINEKODE", "PREPARATBESKRIVELSE"),
         c("person_id","vx_record_date",
           "vx_admin_date",
           "vaccode","vx_brand"))
length(unique(df$person_id)) # 5348449
# ftm, use koblingskoekkel as person_id.

setnames(ref_date_2023, "lnr", "person_id")
df <- merge(df, ref_date_2023, by = "person_id", all.x = TRUE) # left_join
df <- unique(df) # 31,554,595
str(df)
df$vx_record_date <- as.numeric(gsub(";", "", df$vx_record_date))
df$vx_record_date <- gsub("-","", as.character(df$ref_date + df$vx_record_date))
df$vx_admin_date <- gsub("-","", as.character(df$ref_date + df$vx_admin_date))
sum(duplicated(df)==TRUE) # 340
dup <- subset(df, duplicated(df)) # no any dates
length(unique(dup$person_id))  # 103
df <- df[!is.na(vx_admin_date),] # 31,553,896
unique(df$vx_brand)
# head(df)
unique(df$vaccode)
# [1]  "MMR52" "HEP01" "POL03"  "FLU02" "VAR01" "KOM09" "PNE13" "POL02" "HPV02" "HEP20" "MK08" 
# [14] "MK04"  "DTP51" "FLU01" "PNE01" "HPV01"   "KOM06" "DTP52" "MK09"  "POL04" "POL01"
# [27]  "MK03"  "HIB01" "PNE02" "MK10"  "MK01"  "MK06"  "MK07"  "KOM04" "ROT02" "ROT01" "PNE52" "FLU03"
# [40] "AII02" "" "VAR03" "SIN03" "XCV03" "XXS03" "VAR02"  "" "" "XXC03" "XCO03"

# import the vaksinkoder sheet this time

atc <- openxlsx::read.xlsx("N:/durable/vac4eu/scripts/vaksinekoder_atc.xlsx")
atc <- unique(atc)
atc[atc=="37622"] <- "JAN03"
head(atc)
# vx_atc
df <- merge(df, atc, by = "vaccode", all.x = TRUE)
unique(df$vaccode[df$atc_code=="J07BX03" & df$vx_brand=="Ikke oppgitt"])


preparat <- openxlsx::read.xlsx("N:/durable/vac4eu/scripts/vaksinekoder.xlsx")
preparat[preparat=="37622"] <- "JAN03"
preparat <- setDT(preparat)
preparat <- unique(preparat, by = "vaccode")
df <- merge(df, preparat, by = "vaccode", all.x = TRUE) # left_join
head(df)


df[vx_brand=="Ikke oppgitt"]$vx_brand <- "unknown"
setnames(df, "atc_code", "vx_atc")

# vx_atc
# df$vx_atc[grepl("ASZ03|BNT03|CBA01|CBA45|CSH03|CUR03|JAN03|
#                 MOD03|NUV03|SBA01|SBA45|SIN03|
#                 VAL03|VID03|XCO03|XCV03|XXC03|XXS03", df$vaccode)==TRUE] <- "J07BX03"
# df$vx_atc[grepl("FLU01", df$vaccode)==TRUE] <- "J07BB01"
# df$vx_atc[grepl("AII02|FLU02", df$vaccode)==TRUE] <- "J07BB02"
# df$vx_atc[grepl("FLU03", df$vaccode)==TRUE] <- "J07BB03"
# df$vx_atc[grepl("PNE01", df$vaccode)==TRUE] <- "J07AL01"
# df$vx_atc[grepl("PNE02|PNE13", df$vaccode)==TRUE] <- "J07AL02"
# df$vx_atc[grepl("PNE52", df$vaccode)==TRUE] <- "J07AL52"
# df$vx_atc[grepl("POL01", df$vaccode)==TRUE] <- "J07BF01"
# df$vx_atc[grepl("POL02", df$vaccode)==TRUE] <- "J07BF02"
# df$vx_atc[grepl("POL03", df$vaccode)==TRUE] <- "J07BF03"
# df$vx_atc[grepl("POL04", df$vaccode)==TRUE] <- "J07BF04"
# df$vx_atc[grepl("MMR52", df$vaccode)==TRUE] <- "J07BD52"
# df$vx_atc[grepl("HIB01", df$vaccode)==TRUE] <- "J07AG01"
# df$vx_atc[grepl("KOM04", df$vaccode)==TRUE] <- "J07CA04"
# df$vx_atc[grepl("KOM06", df$vaccode)==TRUE] <- "J07CA06"
# df$vx_atc[grepl("KOM09", df$vaccode)==TRUE] <- "J07CA09"
# df$vx_atc[grepl("HEP01", df$vaccode)==TRUE] <- "J07BC01"
# df$vx_atc[grepl("HEP20", df$vaccode)==TRUE] <- "J07BC20"
# df$vx_atc[grepl("VAR01", df$vaccode)==TRUE] <- "J07BK01"
# df$vx_atc[grepl("VAR02", df$vaccode)==TRUE] <- "J07BK02"
# df$vx_atc[grepl("VAR03", df$vaccode)==TRUE] <- "J07BK03"
# df$vx_atc[grepl("MK01", df$vaccode)==TRUE] <- "J07AH01"
# df$vx_atc[grepl("MK03", df$vaccode)==TRUE] <- "J07AH03"
# df$vx_atc[grepl("MK04", df$vaccode)==TRUE] <- "J07AH04"
# df$vx_atc[grepl("MK06", df$vaccode)==TRUE] <- "J07AH06"
# df$vx_atc[grepl("MK07", df$vaccode)==TRUE] <- "J07AH07"
# df$vx_atc[grepl("MK08", df$vaccode)==TRUE] <- "J07AH08"
# df$vx_atc[grepl("MK09", df$vaccode)==TRUE] <- "J07AH09"
# df$vx_atc[grepl("MK10", df$vaccode)==TRUE] <- "J07AH10"
# df$vx_atc[grepl("HPV01", df$vaccode)==TRUE] <- "J07BM01"
# df$vx_atc[grepl("HPV02", df$vaccode)==TRUE] <- "J07BM02"
# df$vx_atc[grepl("HPV09", df$vaccode)==TRUE] <- "J07BM03"
# df$vx_atc[grepl("DTP51", df$vaccode)==TRUE] <- "J07AJ51"
# df$vx_atc[grepl("DTP52", df$vaccode)==TRUE] <- "J07AJ52"
# df$vx_atc[grepl("ROT01", df$vaccode)==TRUE] <- "J07BH01"
# df$vx_atc[grepl("ROT02", df$vaccode)==TRUE] <- "J07BH02"
# 
head(df)
unique(df$vx_text[df$vx_atc=="J07BX03"])
# vx_type

df$vx_type[grepl("Covid-19", df$vx_text)==TRUE] <- "covid19_original"
df$vx_type[grepl("BA.1", df$vx_text)==TRUE] <- "covid19_bivalent_BA1"
df$vx_type[grepl("BA.4-5", df$vx_text)==TRUE] <- "covid19_bivalent_BA4to5"

df$vx_type[grepl("AII02|FLU02", df$vaccode)==TRUE] <- "iv_i"
df$vx_type[grepl("FLU01", df$vaccode)==TRUE] <- "influenza"
df$vx_type[grepl("FLU03", df$vaccode)==TRUE] <- "iv_att_l_n"
df$vx_type[grepl("PNE01", df$vaccode)==TRUE] <- "pcv"
df$vx_type[grepl("PNE02", df$vaccode)==TRUE] <- "pcv_c"
df$vx_type[grepl("PNE13", df$vaccode)==TRUE] <- "pcv13"
df$vx_type[grepl("PNE52", df$vaccode)==TRUE] <- "pcv_10"

df$vx_type[grepl("DTP51", df$vaccode)==TRUE] <- "dptet"
df$vx_type[grepl("DTP52", df$vaccode)==TRUE] <- "daptet_ads"

df$vx_type[grepl("POL01", df$vaccode)==TRUE] <- "polio"
df$vx_type[grepl("POL03", df$vaccode)==TRUE] <- "ipv"
df$vx_type[grepl("POL02|POL04", df$vaccode)==TRUE] <- "opv_l"

df$vx_type[grepl("MMR52", df$vaccode)==TRUE] <- "mmr_l"

df$vx_type[grepl("HIB01", df$vaccode)==TRUE] <- "hib_c"

df$vx_type[grepl("KOM04", df$vaccode)==TRUE] <- "act-hib-polio"
df$vx_type[grepl("KOM06", df$vaccode)==TRUE] <- "dhibapipvtet_c"
df$vx_type[grepl("KOM09", df$vaccode)==TRUE] <- "dhibapipvtet"

df$vx_type[grepl("HEP01", df$vaccode)==TRUE] <- "hepb"
df$vx_type[grepl("HEP20", df$vaccode)==TRUE] <- "hepahepb"

df$vx_type[grepl("VAR01", df$vaccode)==TRUE] <- "varicella zoster"

df$vx_type[grepl("VAR02", df$vaccode)==TRUE] <- "vz_l"
df$vx_type[grepl("VAR03", df$vaccode)==TRUE] <- "vz_r"

df$vx_type[grepl("HPV01", df$vaccode)==TRUE] <- "hpv4"
df$vx_type[grepl("HPV02", df$vaccode)==TRUE] <- "hpv2"
df$vx_type[grepl("HPV09", df$vaccode)==TRUE] <- "hpv9"

df$vx_type[grepl("MK01|MK03|MK06|MK09|MK10", df$vaccode)==TRUE] <- "meningitis"
df$vx_type[grepl("MK04", df$vaccode)==TRUE] <- "men4_pls"
df$vx_type[grepl("MK07", df$vaccode)==TRUE] <- "menc_c"
df$vx_type[grepl("MK08", df$vaccode)==TRUE] <- "men4_c"
df$vx_type[grepl("ROT01", df$vaccode)==TRUE] <- "rv"
df$vx_type[grepl("ROT02", df$vaccode)==TRUE] <- "rv5"

sum(is.na(df$vx_type))

unique(df[grepl("covid19", vx_type)]$vx_manufacturer)
# [1] "Comirnaty (BioNTech og_x000D_ Pfizer)"                          "Spikevax (Moderna)"                                            
# [3] "Comirnaty Original/Omicron BA.1 (BioNTech og Pfizer)"           "Vaxzevria (AstraZeneca)"                                       
# [5] "Comirnaty Original/Omicron BA.4-5_x000D_\n(BioNTech og Pfizer)" "Janssen - Cilag"                                               
# [7] "Spikevax Bivalent Original/Omicron BA.1_x000D_\n(Moderna)"      "Spikevax Bivalent Original/Omicron BA.4-5_x000D_\n(Moderna)"   
# [9] "CoronaVac"                                                      "Convidecia"                                                    
# [11] "Sinopharm BIBP"                                                 "Covishield"                                                    
# [13] "Nuvaxovid"                                                      "CureVac"                                                       
# [15] "Covaxin"                                                        "Covovax"     

df[grepl("Pfizer", vx_manufacturer)]$vx_manufacturer <- "pfizer"
df[grepl("Moderna", vx_manufacturer)]$vx_manufacturer <- "moderna"
df[grepl("AstraZeneca", vx_manufacturer)]$vx_manufacturer <- "astrazeneca"
df[grepl("Janssen", vx_manufacturer)]$vx_manufacturer <- "janssen"
df[grepl("CoronaVac", vx_manufacturer)]$vx_manufacturer <- "coronavac"
df[grepl("Convidecia", vx_manufacturer)]$vx_manufacturer <- "convidecia"
df[grepl("Sinopharm", vx_manufacturer)]$vx_manufacturer <- "sinopharm"
df[grepl("Covishield", vx_manufacturer)]$vx_manufacturer <- "covishield"
df[grepl("Nuvaxovid", vx_manufacturer)]$vx_manufacturer <- "nuvaxovid"
df[grepl("CureVac", vx_manufacturer)]$vx_manufacturer <- "cureVac"
df[grepl("Covaxin", vx_manufacturer)]$vx_manufacturer <- "covaxin"
df[grepl("Covovax", vx_manufacturer)]$vx_manufacturer <- "covovax"


df <- df[,"vx_brand":=NULL]
df <- df[,"vaccode":=NULL]
df <- df[,"ref_date":=NULL]
df$vx_text <- ""
df$medicinal_product_id <- ""
df$origin_of_vx_record <- "SYSVAK"
df$meaning_of_vx_record <- "vaccination_record"
df$vx_dose <- NA

df$vx_lot_num <- NA

df$visit_occurrence_id <- NA

df <- df[,.(person_id,
            vx_record_date,
            vx_admin_date,
            vx_atc,
            vx_type,
            vx_text,
            medicinal_product_id,
            origin_of_vx_record,
            meaning_of_vx_record,
            vx_dose,
            vx_manufacturer,
            vx_lot_num,
            visit_occurrence_id)]
df <- unique(df)
fwrite(df, "N:\\durable\\vac4eu\\CDMInstances\\vac4eu\\VACCINES.csv", row.names = FALSE)


vx_counts <- data.table(table(df[grepl("covid19", vx_type)]$vx_manufacturer))
write.csv2(vx_counts, "N:/durable/vac4eu/vx_counts.csv", row.names = FALSE)

rm(df)
gc()
