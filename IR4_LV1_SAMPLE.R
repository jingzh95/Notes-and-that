

setwd("N:/durable/vac4eu/CDMInstances/vac4eu_10k/")
setwd("N:/durable/vac4eu/CDMInstances/vac4eu/")



# METADATA CDM_SOURCE
# include PROCEDURES
setwd("N:/durable/vac4eu/CDMInstances/vac4eu_10k/")
meta <- fread("METADATA.csv")

source <- fread("CDM_SOURCE.csv")
str(source)
source$recommended_end_date <- 20221216
fwrite(source, "CDM_SOURCE.csv")
fwrite(source, "../vac4eu_10k/CDM_SOURCE.csv")