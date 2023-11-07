ref_date <- fread("N:/durable/vac4eu/ref_date_2023.csv")


library(data.table)
df <- fread("N:\\durable\\VAC4EU datasets\\Delivery Feb-May 2023\\MSIS\\PDB3198_MSIS-data_2023-03.csv")
# 1 444 633
# names(df)
head(df) 
# str(df)

# subset observations that are covid positive
# need to double check if the number changed in the new data
df <- df[DiagnoseNr==713,]

varsnames <- c("koblingsnoekkel", "PrøvedatoDiffDager", "DiagnoseNr")
new_varsnames <- c("person_id", "mo_date", "mo_code")
setnames(df, varsnames, new_varsnames)

df <- df[,.(person_id, mo_date, mo_code)]


sum(is.na(df$mo_source_value))/nrow(df)


df <- merge(df, ref_date, by = "person_id", all.x = TRUE)
df$mo_date <- df$mo_date + df$ref_date
summary(df$mo_date)
#      Min.      1st Qu.       Median         Mean      3rd Qu. 
# "2020-02-12" "2021-12-24" "2022-01-27" "2021-12-26" "2022-02-17" 
#        Max.         NA's 
# "2023-01-14"         "27" 
sum(is.na(df$mo_date)) # 27
df <- df[!is.na(mo_date),] # removed no test dates
df$mo_date <- gsub("-", "", as.character(df$mo_date))
df$person_id <- as.character(df$person_id)





df$mo_code <- as.character(df$mo_code)
df$mo_source_value <- NA
df$mo_source_value <- df$mo_code
df$mo_record_vocabulary <- "NO_lab_coding_system"
df$mo_source_table <- "MSIS"
df$mo_source_column <- "DiagnoseNr"
df$mo_unit <- NA #Luigi: can later change to NA
df$mo_meaning <- "covid19_positive_test"
df$mo_origin <- "MSIS"

df$visit_occurrence_id <- NA
# suggested to remove by Roel

# order?
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
fwrite(df, "N:\\durable\\vac4eu\\CDMInstances\\vac4eu\\MEDICAL_OBSERVATIONS_MSIS_COVID_POS.csv")
rm(df)
gc()
