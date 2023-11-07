setwd("/ess/p1921/data/durable/vac4eu/Data characterisation/Level1b_1.4/g_output")
files <- list.files(pattern = ".csv")
files
library(data.table)
df <- fread(files[2])
unique(df$mo_meaning)
unique(df$mo_code)

df <- fread(files[3])
head(df)
summary(df$disp_number_medicinal_product)

sum(df$disp_number_medicinal_product<0) # 37

df <- fread(files[4])
head(df)

df <- fread(files[5])
head(df)

df <- fread(files[6])
head(df)
length(unique(df$country_of_birth)) # 228

df <- fread(files[7])
head(df) # n_maked

df <- fread(files[8])
head(df)

df <- fread(files[9])
head(df)
unique(df$so_meaning)
unique(df$so_origin)

df <- fread(files[10])
head(df)
unique(df$vx_type)

df <- fread(files[11])
head(df)
unique(df$meaning_of_visit)
table(df$meaning_of_visit)

