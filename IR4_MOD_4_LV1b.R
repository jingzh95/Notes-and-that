TSD_install_path <- "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux/" 
library(data.table)
library(dplyr, lib.loc = TSD_install_path)
library(purrr, lib.loc = TSD_install_path)

########################################
## MEDICINES
########################################
setwd("/ess/p1921/data/durable/vac4eu/CDMInstances/vac4eu")
files <- list.files(pattern = "MEDICINES")
for (i in seq_along(files)){
  df <- fread(files[i])
  print(files[i])
  print(df$medicinal_produc_id)
}

########################################
## VACCINES
########################################
df <- fread("VACCINES.csv")
t <- unique(df$vx_type)
t <- unique(df$vx_manufacturer)

meta <- fread("METADATA.csv")
v <- meta[type_of_metadata == "list_of_values" &
     tablename == "VACCINES" & 
       columnname == "vx_type"]$values

setdiff(unique(v),t)


df <- fread("SURVEY_OBSERVATIONS_CHILDREN.csv")
df[so_source_column=="birth_registry_child"]$person_id
unique(df[so_source_column == "BARNETS_HELSE"]$so_source_value)



