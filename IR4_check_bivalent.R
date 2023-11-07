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
df <- setDT(df) #

# CBA01|CBA45|SBA01|SBA45
biv <- df[VAKSINEKODE=="CBA01"|
            VAKSINEKODE=="CBA45"|
            VAKSINEKODE=="SBA01"|
            VAKSINEKODE=="SBA45"]  # 382 807
length(unique(biv$KOBLINGSNOEKKEL)) # 382 360


