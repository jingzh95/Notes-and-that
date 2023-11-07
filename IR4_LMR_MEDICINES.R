TSD_install_path <- "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux"
library(data.table)
library(stringr, lib.loc  = TSD_install_path)


ref_date <- fread("/ess/p1921/data/durable/vac4eu/ref_date_2023.csv")

setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu/")
raw_path <- "/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/"
origin <- "LMR"
format <- ".csv"
table_path <- paste0(raw_path, origin, "/")
files <- list.files(path = table_path, pattern = format)
files
# [1] "Delfil2010_4_65_uttrekksfil_2023-07-27.csv" "Delfil2010_4_66_uttrekksfil_2023-07-27.csv" "Delfil2011_4_65_uttrekksfil_2023-07-27.csv"
# [4] "Delfil2011_4_66_uttrekksfil_2023-07-27.csv" "Delfil2012_4_65_uttrekksfil_2023-07-27.csv" "Delfil2012_4_66_uttrekksfil_2023-07-27.csv"
# [7] "Delfil2013_4_65_uttrekksfil_2023-07-27.csv" "Delfil2013_4_66_uttrekksfil_2023-07-27.csv" "Delfil2014_4_65_uttrekksfil_2023-07-27.csv"
# [10] "Delfil2014_4_66_uttrekksfil_2023-07-27.csv" "Delfil2015_4_65_uttrekksfil_2023-07-27.csv" "Delfil2015_4_66_uttrekksfil_2023-07-27.csv"
# [13] "Delfil2016_4_65_uttrekksfil_2023-07-27.csv" "Delfil2016_4_66_uttrekksfil_2023-07-27.csv" "Delfil2017_4_65_uttrekksfil_2023-07-27.csv"
# [16] "Delfil2017_4_66_uttrekksfil_2023-07-27.csv" "Delfil2018_4_65_uttrekksfil_2023-07-27.csv" "Delfil2018_4_66_uttrekksfil_2023-07-27.csv"
# [19] "Delfil2019_4_65_uttrekksfil_2023-07-27.csv" "Delfil2019_4_66_uttrekksfil_2023-07-27.csv" "Delfil2020_4_65_uttrekksfil_2023-07-27.csv"
# [22] "Delfil2020_4_66_uttrekksfil_2023-07-27.csv" "Delfil2021_4_65_uttrekksfil_2023-07-27.csv" "Delfil2021_4_66_uttrekksfil_2023-07-27.csv"
# [25] "Delfil2022_4_65_uttrekksfil_2023-07-27.csv" "Delfil2022_4_66_uttrekksfil_2023-07-27.csv"
setnames(ref_date, "lnr", "koblingsnoekkel", skip_absent = T)  ## T
for(i in seq_along(files)[seq_along(files) %% 2 == 1]){
  df1 <- fread(paste0(table_path, files[i]))         
  df1 <- df1[,.(koblingsnoekkel,
                Vare_Varenavn,
                Legemiddel_ATCkode_Niva5,
                Utlevering_Dato_Differansedager,
                Utlevering_AntallPakninger,
                Utlevering_Lopenummer)]

  gc()
  df1 <- merge(df1, ref_date, by = "koblingsnoekkel", all.x = TRUE)
  df1$Utlevering_Dato_Differansedager <- df1$ref_date + df1$Utlevering_Dato_Differansedager
  summary(df1$Utlevering_Dato_Differansedager)
  df1 <- df1[!is.na(Utlevering_Dato_Differansedager)]
  setnames(df1, c("koblingsnoekkel",
                  "Vare_Varenavn",
                  "Legemiddel_ATCkode_Niva5",
                  "Utlevering_Dato_Differansedager",
                  "Utlevering_AntallPakninger",
                  "Utlevering_Lopenummer"),
           c("person_id",
             "medicinal_product_id",
             "medicinal_product_atc_code",
             "date_dispensing",
             "disp_number_medicinal_product",
             "visit_occurrence_id"))
  head(df1)
  df1$disp_number_medicinal_product <- gsub(",", "", df1$disp_number_medicinal_product)   ## remove ","
    df1$disp_number_medicinal_product <- as.numeric(df1$disp_number_medicinal_product, digits=3)  ## added by Anteneh
  df1$date_dispensing <- gsub("-", "", df1$date_dispensing)
  head(df1)
  df1$date_prescription <- NA
  df1$presc_quantity_per_day <- NA
  df1$presc_quantity_unit <- NA
  df1$presc_duration_days <- NA
  df1$product_lot_number <- NA
  df1$indication_code <- NA
  df1$indication_code_vocabulary <- NA
  df1$meaning_of_drug_record<-"dispensing_in_community_pharmacy"
  df1$origin_of_drug_record<-"LMR"
  df1$prescriber_speciality <- NA
  df1$prescriber_speciality_vocabulary <- NA
  
  df1 <- df1[,.(person_id,
                medicinal_product_id,
                medicinal_product_atc_code,
                date_dispensing,
                date_prescription,
                disp_number_medicinal_product,
                presc_quantity_per_day,
                presc_quantity_unit,
                presc_duration_days,
                product_lot_number,
                indication_code,
                indication_code_vocabulary,
                meaning_of_drug_record,
                origin_of_drug_record,
                prescriber_speciality,
                prescriber_speciality_vocabulary,
                visit_occurrence_id)]
  str(df1)
  library(stringr)
  out_file_suffix <- paste0(substr(str_split_1(files[i], "_")[1], 7, 10), "_5")
  library(data.table)
  fwrite(df1, paste0("MEDICINES_", out_file_suffix, format))
  
  df2 <- fread(paste0(table_path, files[i+1]))
  df2 <- df2[,.(koblingsnoekkel,
                # Vare_Varenavn,
                Legemiddel_ATCkode_Niva4,
                Utlevering_Dato_Differansedager,
                Utlevering_AntallPakninger,
                Utlevering_Lopenummer)]
  gc()
  df2 <- merge(df2, ref_date, by = "koblingsnoekkel", all.x = TRUE)
  df2$Utlevering_Dato_Differansedager <- df2$ref_date + df2$Utlevering_Dato_Differansedager
  summary(df2$Utlevering_Dato_Differansedager)
  df2 <- df2[!is.na(Utlevering_Dato_Differansedager)]
  setnames(df2, c("koblingsnoekkel",
                  # "Vare_Varenavn",
                  "Legemiddel_ATCkode_Niva4",
                  "Utlevering_Dato_Differansedager",
                  "Utlevering_AntallPakninger",
                  "Utlevering_Lopenummer"),
           c("person_id",
             # "medicinal_product_id",
             "medicinal_product_atc_code",
             "date_dispensing",
             "disp_number_medicinal_product",
             "visit_occurrence_id"))
  head(df2)
  df2$medicinal_product_id <- NA
  df2$disp_number_medicinal_product <- gsub(",", ".", df2$disp_number_medicinal_product)          ## added by AD "."
    df2$disp_number_medicinal_product <- as.numeric(df2$disp_number_medicinal_product, digits=3)  ## added by Anteneh
  df2$date_dispensing <- gsub("-", "", df2$date_dispensing)
  head(df2)
  df2$date_prescription <- NA
  df2$presc_quantity_per_day <- NA
  df2$presc_quantity_unit <- NA
  df2$presc_duration_days <- NA
  df2$product_lot_number <- NA
  df2$indication_code <- NA
  df2$indication_code_vocabulary <- NA
  df2$meaning_of_drug_record<-"dispensing_in_community_pharmacy"
  df2$origin_of_drug_record<-"LMR"
  df2$prescriber_speciality <- NA
  df2$prescriber_speciality_vocabulary <- NA
  
  df2 <- df2[,.(person_id,
                medicinal_product_id,
                medicinal_product_atc_code,
                date_dispensing,
                date_prescription,
                disp_number_medicinal_product,
                presc_quantity_per_day,
                presc_quantity_unit,
                presc_duration_days,
                product_lot_number,
                indication_code,
                indication_code_vocabulary,
                meaning_of_drug_record,
                origin_of_drug_record,
                prescriber_speciality,
                prescriber_speciality_vocabulary,
                visit_occurrence_id)]
  str(df2)
  out_file_suffix <- paste0(substr(str_split_1(files[i+1], "_")[1], 7, 10), "_4")
  fwrite(df2, paste0("MEDICINES_", out_file_suffix, format))
  gc()

} 
