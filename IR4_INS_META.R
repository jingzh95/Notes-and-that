TSD_install_path <- 
  ifelse(toupper(Sys.info())["sysname"]=="LINUX",
         "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux/",
         "N:/durable/vac4eu/R_v4.1_packages/")
bla <- 
  ifelse(toupper(Sys.info())["sysname"]=="LINUX",
         "/ess/p1921/data",
         "N:")
library(data.table)
library(stringr, lib.loc = TSD_install_path)
setwd(paste0(bla, "/durable/vac4eu/CDMInstances/vac4eu/"))
##########################################################3
##########################################################3
##########################################################3
##########################################################3
##########################################################3
##########################################################3
if (as.Date(substr(file.info("INSTANCE.csv")$atime, 1, 10)) == 
            as.Date("2023-07-09")){
  print("The INSTANCE table is updated for NPR, MBRN and KUHR for 2023 delivery.")
}

# for complete KUHR
ins <- fread("INSTANCE.csv")
df <- fread(list.files(pattern = "RENCE_KUHR_2010"))
since <- min(df$visit_start_date)
ins[source_table_name=="KUHR"]$since_when_data_complete <- since
df <- fread(list.files(pattern = "RENCE_KUHR_2022"))
up_to <- max(df$visit_start_date)
ins[source_table_name=="KUHR"]$up_to_when_data_complete <- up_to
ins[source_table_name=="KUHR"]
fwrite(ins, "INSTANCE.csv")

# setwd("N:/durable/vac4eu/CDMInstances/vac4eu/")

ins <- fread("INSTANCE.csv")
ins <- ins[!source_table_name == "LMR"]
unique(ins$source_table_name)
# add LMR chunk
# add LMR chunk
# add LMR chunk
# add LMR chunk
tb <- "LMR"
head(ins)

chunk <- data.table(source_table_name = tb,
                    source_column_name = c("koblingsnoekkel",
                                           "Vare_Varenavn",
                                           "Legemiddel_ATCkode_Niva5",
                                           "Legemiddel_ATCkode_Niva4",
                                           "Utlevering_Dato_Differansedager",
                                           "Utlevering_AntallPakninger",
                                           "Utlevering_Lopenummer"),
                    included_in_instance = "yes",
                    date_when_data_last_updated = 20230505,
                    since_when_data_complete = "20091216",
                    up_to_when_data_complete = "20230114",
                    restriction_in_values = "no",
                    list_of_values = NA,
                    restriction_condition = NA)

ins <- rbind(ins, chunk)


ins[grepl("FDATO", source_column_name)]$source_column_name <- "FDATO_DIFFERANSEDAGER_MOR"
fwrite(ins, "INSTANCE.csv")
# end adding LMR chunk
# end adding LMR chunk
# end adding LMR chunk
# end adding LMR chunk
ins$source_table_name <- 
  gsub("[\"]", "", ins$source_table_name)
ins$restriction_condition <- NA

str(ins)
meta <- fread("METADATA.csv")

# change LMR to meta
# change LMR to meta
# change LMR to meta
# change LMR to meta
# change LMR to meta
meta[type_of_metadata == "presence_of_table" &
     tablename=="MEDICINES"]$values <- "Yes"

meta[type_of_metadata == "presence_of_column" &
       tablename == "MEDICINES" &
       columnname == "medicinal_product_id"]$values <- "Yes"

meta[type_of_metadata == "presence_of_column" &
       tablename == "MEDICINES" &
       columnname == "date_dispensing"]$values <- "Yes"

meta[type_of_metadata == "presence_of_column" &
       tablename == "MEDICINES" &
       columnname == "visit_occurrence_id"]$values <- "Yes"

meta[type_of_metadata == "list_of_values" &
       tablename == "INSTANCE" &
       columnname == "source_table_name"]$values <- "MBRN KUHR NPR MSIS SYSVAK SSB DAR LMR"

chunk <- data.table(type_of_metadata = "list_of_values",
                    tablename = "MEDICINES",
                    columnname = "meaning_of_drug_record",
                    other = NA,
                    values = "dispensing_in_community_pharmacy")
meta[type_of_metadata == "list_of_values" &
       tablename == "SURVEY_OBSERVATIONS" &
       columnname == "so_source_column"]$values <- "FDATO_DIFFERANSEDAGER_MOR SVLEN_DG PREEKL DIABETES_MELLITUS BLODN_F13 BLODN_13_28 BLODN_E28 BLODNING_O500 APGAR5 ZSCORE_BW_GA VEKT DODKAT KMI_FOER MOR_ROYKTE_FOER_SVSK KSNITT KSNITT_PLANLAGT DODFODTE_5 PLACENTA_PREVIA MISD NEVRALRORSDEFEKTER SPINAB ENCEPH ANENCEPH LEPPE_LEPPEGANESPALTE GANESPALTE HJERTE_MISD GASTROS OMPHALO KLUMPFOT KROMOSOMFEIL DOWNS BARNETS_HELSE PARITET_5"
  

meta <- rbind(meta, chunk)
fwrite(meta, "METADATA.csv")
# end chaning LMR to meta
# end chaning LMR to meta
# end chaning LMR to meta
# end chaning LMR to meta




source <- fread("CDM_SOURCE.csv")
str(source)
source$recommended_end_date <- 20221216
fwrite(source, "CDM_SOURCE.csv")
fwrite(source, "../vac4eu_10k/CDM_SOURCE.csv")

tbs_ins <- unique(ins$source_table_name)
tbs_meta <- meta[columnname=="source_table_name"]$values
tbs_meta <- str_split(tbs_meta, " ")[[1]]

to_remove <- setdiff(tbs_ins, tbs_meta)
to_remove
to_add <- setdiff(tbs_meta, tbs_ins)
to_add

ins <- ins[!source_table_name==to_remove,]
str(ins)

# 

  

tb="KUHR"

for (tb in tbs_meta){
  if (tb == "NPR"){
    # cdms  <- list.files(pattern = "RENCE_NPR")
    # for (i in seq_along(seq_along(cdm))){
    #   cdm <- fread(cdms[i]) # TSB SOM
    #   head(cdm)
    #   max0 <- max(cdm$visit_start_date, na.rm = T) # 20230114  20230114 20230114
    #   min0 <- min(cdm$visit_start_date, na.rm = T) # 20070531  20000526 (864 s.t)  10107?
    #   max1 <- max(cdm$visit_end_date, na.rm = T) # 20230114  20230114 94091224?
    #   min1 <- min(cdm$visit_end_date, na.rm = T) # 20091217 20091216 10107?
    #   if (i == 1){
    #     max_start_date <- max0
    #     min_start_date <- min0
    #     max_end_date <- max1
    #     min_end_date <- min1
    #   }else{
    #     max_start_date <- max(max_start_date, max0)
    #     min_start_date <- min(min_start_date, min0)
    #     max_end_date <- max(max_end_date, max1)
    #     min_end_date <- min(min_end_date, min1)
    # 
    #   }
    #   rm(df)
    #   gc()
    # }
    
    npr <- data.table(source_table_name = "NPR",
                      source_column_name = c("lnr",
                                             "inndato_diff",
                                             "utdato_diff",
                                             "koderverdi",
                                             "kodenavn",
                                             "kobl_nokkel",
                                             # "sluttdato_diff",
                                             "omsorgsnivaa"),
                      included_in_instance = "yes",
                      date_when_data_last_updated = 20230505,
                      since_when_data_complete = "20091216",
                      up_to_when_data_complete = "20230114",
                      restriction_in_values = "no",
                      list_of_values = NA,
                      restriction_condition = NA)
    # df[source_column_name=="inndato_diff"]$since_when_date_complete <- min_start_date 
    # df[source_column_name=="utdato_diff"]$since_when_date_complete <- min_end_date    
    # df[source_column_name=="inndato_diff"]$up_to_when_date_complete <- max_start_date
    # df[source_column_name=="utdato_diff"]$up_to_when_date_complete <- max_end_date
    ins <- ins[!source_table_name=="NPR"]
    ins <- rbind(ins, npr)
    
    ins <- unique(ins)
    fwrite(ins, "INSTANCE.csv")
  }else if (tb == "KUHR"){
    cdms  <- list.files(pattern = "RENCE_KUHR")
    
      cdm <- fread(cdms) 
      head(cdm)
      max0 <- max(cdm$visit_start_date, na.rm = T) # 20230114  
      min0 <- min(cdm$visit_start_date, na.rm = T) # 20211216
      
      print(cdms)
      print(max0)
      print(min0)
      
      kuhr <- ins[source_table_name == "KUHR"]
      kuhr$since_when_data_complete <- min0
      kuhr$up_to_when_data_complete <- max0
      
      ins <- ins[!source_table_name=="KUHR"]
      ins <- rbind(ins, kuhr)
      rm(cdm)
      gc()
      
      ins <- unique(ins)
      fwrite(ins, "INSTANCE.csv")
    

  }else if (tb == "MBRN"){
    cdm <- fread("SURVEY_OBSERVATIONS.csv")
    str(cdm)
    
    vars <- union(unique(cdm$so_source_column), "ID_MOR")
    vars <- union(vars, "FDATO_DIFFERANSEDAGER_BARN")
    since <- min(cdm$so_date, na.rm = T)
    up_to <- max(cdm$so_date, na.rm = T)
    
    mbrn <- data.table(source_table_name = "MBRN",
                       source_column_name = vars,
                       included_in_instance = "yes",
                       date_when_data_last_updated = 20230505,
                       since_when_data_complete = since,
                       up_to_when_data_complete = up_to,
                       restriction_in_values = "no",
                       list_of_values = NA,
                       restriction_condition = NA)
    
    ins <- ins[!source_table_name=="MBRN"]
    ins <- rbind(ins, mbrn)
    
    ins <- unique(ins)
    fwrite(ins, "INSTANCE.csv")
  }else if (tb == "MSIS"){
    cdm <- fread(list.files(pattern = "COVID"))
    since <- min(cdm$mo_date) # 20200212
    up_to <- max(cdm$mo_date) # 20230114
    msis <- ins[source_table_name == "MSIS"]
    msis$since_when_data_complete <- since
    msis$up_to_when_data_complete <- up_to
    
    ins <- ins[!source_table_name=="MSIS"]
    ins <- rbind(ins, msis)
    
    rm(cdm)
    gc()
    
    ins <- unique(ins)
    fwrite(ins, "INSTANCE.csv")
    
  }else if (tb == "SYSVAK"){
    cdm <- fread(list.files(pattern = "VACCINES"))
    since <- min(cdm$vx_admin_date) # 20091216
    up_to <- max(cdm$vx_admin_date) # 20230114
    sysvak <- ins[source_table_name == "SYSVAK"]
    sysvak$since_when_data_complete <- since
    sysvak$up_to_when_data_complete <- up_to
    
    ins <- ins[!source_table_name=="SYSVAK"]
    ins <- rbind(ins, sysvak)
    
    ins <- unique(ins)
    fwrite(ins, "INSTANCE.csv")
    
  }else if (tb == "DAR"){
    cdm <- fread(list.files(pattern = "DAR"))
    since <- min(cdm$start_date_record ) # 20181216
    up_to <- max(cdm$start_date_record ) # 20220114
    
    dar <- data.table(source_table_name = "DAR",
                       source_column_name = c("LOPENR", 
                                              "DIFF_DAGER_DOD", 
                                              "DIAGNOSE_UNDERLIGGENDE_K",
                                              "TYPE_DIAGNOSE_KODEVERK_K"),
                       included_in_instance = "yes",
                       date_when_data_last_updated = 20230505,
                       since_when_data_complete = since,
                       up_to_when_data_complete = up_to,
                       restriction_in_values = "no",
                       list_of_values = NA,
                       restriction_condition = NA)
    
    ins <- ins[!source_table_name=="DAR"]
    ins <- rbind(ins, dar)
    
    ins <- unique(ins)
    fwrite(ins, "INSTANCE.csv")
    
  }# else if (tb == "INNT"){
  #   cdm <- fread(list.files(pattern = "INNT"))
  #   since <- min(cdm$start_date_record ) # 20181216
  #   up_to <- max(cdm$start_date_record ) # 20220114
  #   dar <- ins[source_table_name == "DAR"]
  #   dar$since_when_data_complete <- since
  #   dar$up_to_when_data_complete <- up_to
  #   
  #   ins <- ins[!source_table_name=="DAR"]
  #   ins <- rbind(ins, dar)
  #   ins <- unique(ins)
  #   fwrite(ins, "INSTANCE.csv")
  # }
}

ins <- list.files(pattern = "INSTANCE")
ins <- fread(ins)
ins <- ins[!source_column_name=="sivilstand"]

tb <- "SSB"
sivilstand <- list.files(pattern = "SIVIL")
df <- fread(sivilstand)
summary(df[mo_source_column=="sivilstand_2020"]$mo_date)
unique(df$mo_source_column)
chunk <- data.table(source_table_name = tb,
                    source_column_name = c("sivilstand_2020",
                                           "sivilstand_2021",
                                           "sivilstand_2022"),
                    included_in_instance = "yes",
                    date_when_data_last_updated = 20230505,
                    since_when_data_complete = c("20200101",
                                                 "20210101",
                                                 "20220101"),
                    up_to_when_data_complete = c("20200101",
                                                 "20210101",
                                                 "20220101"),
                    restriction_in_values = "no",
                    list_of_values = NA,
                    restriction_condition = NA)
