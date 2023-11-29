########################################
rm(list=ls(all.names=TRUE))
gc()
setwd("/ess/p1921/data/durable/Saimazeb/for random forest")
# save_to <- "/ess/p1921/data/durable/Jing/rf"
files <- list.files(pattern = ".dta")
files
# "working file  5-11.dta" "working file 12-15.dta" "working file 16-17.dta"


age_group <- c("5-11", "12-15", "16-17") #
# if you want 5-11, set i = 1, else i = 2

TSD_install_path <- "/ess/p1921/data/durable/vac4eu/R_v4.3_packages_linux"

library(tzdb, lib.loc = TSD_install_path)
library(readr, lib.loc = TSD_install_path)
library(haven, lib.loc = TSD_install_path)

library(data.table)
library(vroom, lib.loc = TSD_install_path)
library(openxlsx, lib.loc = TSD_install_path)
library(dplyr, lib.loc = TSD_install_path)
library(stringr)

library(farver, lib.loc = TSD_install_path)
library(ggplot2, lib.loc = TSD_install_path)
library(table1, lib.loc = TSD_install_path)

library(randomForest, lib.loc = TSD_install_path)


######################
### LOAD DATA 5-11 ###
######################
for ( i in seq_along(age_group)){
  df <- read_dta(files[i])
  df <- data.table(df)
  names(df)
  
  ###########################################
  ### CLEAN & CHANGE VARIABLE NAMES 5-11 ###
  ###########################################
  df1 <- df
  ##  remove variables that are not needed in TB1
  
  if (i == 1){
    df1 <- df1[,c("lnr", "y_m_birth", "county",
                  "lopenr_mor", "lopenr_far",
                  "Household_incom","far_wlonn","mor_wlonn" ):=NULL]
  }else{
    df1 <- df1[,c("lnr", "y_m_birth", "county",
                  "lopenr_mor", "lopenr_far",
                  "household_income","far_incom","mor_incom" ):=NULL]
  }
  
  
  ## check the names of the variables that are kept
  names(df1) 
  
  ## rename variables according to TB1
  if (i == 1){
    setnames(df1, 
             c("gender", "Immigration_status", "preterm", # old names "county", 
               "Chil_vac_status", "Prev_covid_infe",      
               "Country_birth", "Household_income_level",
               "Vaccination_status","mor_covid_vs",            
               "Mother_education_level", "far_covid_vs",          
               "Father_Education_level" ,"asthma" ,"riskfactors",        
               "Mor_Prof","Far_Prof"), 
             c("Sex",  "Immigration background", "Preterm birth",   ## CHANGE LABEL HERE "County of residence",
               "Childhood Vaccination status", "Previous COVID-19 infection",             ## CHANGE LABEL HERE
               "Country of birth","Household income",                                     ## CHANGE LABEL HERE  
               "Vaccination_status","Mother's COVID-19 vaccination status",                ## CHANGE LABEL HERE
               "Mother's education", "Father's COVID-19 vaccination status",                ## CHANGE LABEL HERE
               "Father's education" ,"Diagnosed with asthma" ,"Risk of COVID-19 infection",    ## CHANGE LABEL HERE    
               "Mother's profession","Father's profession"))
  }else{
    setnames(df1, 
             c("gender", "Immigration_status", # old names "county", 
               "Prev_covid_inf",      
               "country_birth", "Household_income_level",
               "Vaccination_status","mor_covid_vs",            
               "Mother_education_level", "far_covid_vs",          
               "Father_Education_level" ,"asthma" ,"riskfactors",        
               "Mor_Prof","Far_Prof"), 
             c("Sex",  "Immigration background",    ## CHANGE LABEL HERE "County of residence",
               "Previous COVID-19 infection",             ## CHANGE LABEL HERE
               "Country of birth","Household income",                                     ## CHANGE LABEL HERE  
               "Vaccination_status","Mother's COVID-19 vaccination status",                ## CHANGE LABEL HERE
               "Mother's education", "Father's COVID-19 vaccination status",                ## CHANGE LABEL HERE
               "Father's education" ,"Diagnosed with asthma" ,"Risk of COVID-19 infection",    ## CHANGE LABEL HERE    
               "Mother's profession","Father's profession"))
  }
  
  
  ## check the updated names
  names(df1)
  
  #################################################
  ### LABEL THE VARIABLES ACCORDING TO TB1 5-11 ###
  #################################################
  
  
  
  # change format
  ## CHANGE LABEL HERE inside "labels"
  df1$Sex = factor(df1$Sex, labels = c("Male", "Female"))
  # df1$`County of residence`  <- factor(df1$`County of residence`, 
  # labels = c("Oslo", "Rogaland", "Møre og Romsdal",
  # "Nordland", "Viken", "Innlandet",
  # "Vestfold and Telemark", "Agder",
  # "Vestland", "Trondelag", "Troms and Finnmark"))
  df1$`Immigration background` = factor(df1$`Immigration background`,
                                        labels = c("Norwegian-born to two Norwegian-born parents",
                                                   "Immigrants",
                                                   "Norwegian-born to immigrant parents",
                                                   "Foreign-born with one Norwegian-born parent",
                                                   "Norwegian-born with one foreign-born parent",
                                                   "Foreign-born with two Norwegian-born parents"))
  # df1$`Preterm birth` = factor(df1$`Preterm birth`,
  #                              c("Extremely preterm",
  #                                "Very preterm",
  #                                "Late Preterm",
  #                                "Full term"))
  # for some reason I have to use string replace
  if (i == 1){
    df1$`Preterm birth` <- str_replace_all(as.character(df1$`Preterm birth`),
                                           c("0"="Extremely preterm",
                                             "1"="Very preterm",
                                             "2"="Late Preterm",
                                             "4"="Full term"))
    
    df1$`Preterm birth` <- as.factor(df1$`Preterm birth`)
    
    df1$`Childhood Vaccination status` <- str_replace_all(as.character(df1$`Childhood Vaccination status`),
                                                          c("0"="Not Vaccinated",
                                                            "1"="Partially Vaccinated",
                                                            "2"="Fully Vaccinated"))
    df1$`Childhood Vaccination status` = as.factor(df1$`Childhood Vaccination status`)
  } 
  
  
  
  
  
  
  df1$`Previous COVID-19 infection`= factor(df1$`Previous COVID-19 infection`, 
                                            labels = c("Not infected", "Infected"))
  df1$`Risk of COVID-19 infection` = factor(df1$`Risk of COVID-19 infection`, 
                                            labels = c("No risk", "Medium risk", "High risk"))
  
  df1$`Diagnosed with asthma` = factor(df1$`Diagnosed with asthma`, 
                                       labels = c("No", "Yes"))
  
  
  df1$`Country of birth` = factor(df1$`Diagnosed with asthma`, 
                                  labels = c("Scandinavia", "Out of Scandinavia"))
  
  
  df1$`Household income` = factor(df1$`Household income`, 
                                  labels = c("Low income", 
                                             "Medium income",
                                             "High income"))
  
  if (i == 1| i == 3){
    df1$`Mother's COVID-19 vaccination status` = factor(df1$`Mother's COVID-19 vaccination status`, 
                                                        labels =  c("Not Vaccinated",
                                                                    "Vaccinated"))
  }else{
    df1$`Mother's COVID-19 vaccination status` <- ifelse(!is.na(df1$`Mother's COVID-19 vaccination status`), "Vaccinated", "Not Vaccinated")
    df1$`Mother's COVID-19 vaccination status` <- as.factor(df1$`Mother's COVID-19 vaccination status`)
  }#####???? need to check with Saima
  
  
  df1$`Mother's profession` = factor(df1$`Mother's profession`,
                                     labels = c("Military occupation and undeclared",
                                                "Managers/Sales/Office occupation",
                                                "Academic professions",
                                                "Farmers/Transport/cleaners"))
  
  df1$`Mother's education` = factor(df1$`Mother's education`, 
                                    labels = c("Low education", 
                                               "Middle education",
                                               "High education"))
  
  
  df1$`Father's COVID-19 vaccination status` = factor(df1$`Father's COVID-19 vaccination status`, 
                                                      labels =  c("Not Vaccinated",
                                                                  "Vaccinated"))
  
  
  
  
  df1$`Father's profession` = factor(df1$`Father's profession`,
                                     labels = c("Military occupation and undeclared",
                                                "Managers/Sales/Office occupation",
                                                "Academic professions",
                                                "Other")) ## CHANGE LABEL HERE
  
  df1$`Father's education` = factor(df1$`Father's education`, 
                                    labels = c("Low education", 
                                               "Middle education",
                                               "High education"))
  
  
  df1$Vaccination_status = factor(df1$Vaccination_status, 
                                  labels =  c("Not Vaccinated",
                                              "Vaccinated"))
  
  #########################
  ## PERC OF MISSINGNESS ##
  ## & REMOVE MISSINGNESS #
  #########################
  
  # output <- data.table(col = vector("character", ncol(df1)),
  #                      missingness = vector("numeric", ncol(df1)))
  # for (j in seq_along(df1)){
  #   output[j,1] <- names(df1)[j]
  #   output[j,2] <- round(sum(is.na(df1[[j]]))/nrow(df1), 2)
  # }
  # output
  
  #" PRODUCE TB1 with missingness counting
  if (i == 1){
    table1(~Sex+`Immigration background`+ # `County of residence`+
             `Preterm birth`+`Childhood Vaccination status`+`Previous COVID-19 infection`+
             `Risk of COVID-19 infection` + `Diagnosed with asthma` +
             `Country of birth`+`Household income`+
             `Mother's COVID-19 vaccination status`+`Mother's profession`+`Mother's education`+
             `Father's COVID-19 vaccination status`+`Father's profession`+`Father's education`|Vaccination_status, data=df1,
    )
  }else{
    table1(~Sex+`Immigration background`+ # `County of residence`+
             # `Preterm birth`+`Childhood Vaccination status`+
             `Previous COVID-19 infection`+
             `Risk of COVID-19 infection` + `Diagnosed with asthma` +
             `Country of birth`+`Household income`+
             `Mother's COVID-19 vaccination status`+`Mother's profession`+`Mother's education`+
             `Father's COVID-19 vaccination status`+`Father's profession`+`Father's education`|Vaccination_status, data=df1,
    )
  }
  
  
  
  # remove rows with missing value
  data <- na.omit(df1)
  data <- data.frame(data)
  # create a rf model
  set.seed (20231115)
  rf_model_1 <- randomForest(Vaccination_status~., data = data)
  # summary
  print(rf_model_1)
  
  
  # plot variable importance
  # ## CHANGE LABEL HERE inside "main"
  bmp(paste0("vx_uptake_features_excl_county_", age_group[i], ".bmp"),
      width = 960, height = 720, units = "px",
  )
  varImpPlot(rf_model_1,
             main = paste0("IMPORTANT FEATURES OF COVID-19 VACCINATION UPTAKE IN CHILDREN BETWEEN ",
                           age_group[i])
  )
  dev.off()
  
  ###############################
  ## manually plot importance ##
  ###############################
  ## extract the importance ranking
  imp <- data.frame(rf_model_1$importance)
  ## replace the "." with space
  row.names(imp) <- gsub("\\.", " ",  row.names(imp))
  ## add "'" 
  row.names(imp) <- str_remove_all(row.names(imp), c("Mother s" = "Mother's",
                                                     "Father s" = "Father's",
                                                     "COVID 19" = "COVID-19"))
  ## use row names as Features
  imp$Features <- row.names(imp)
  rownames(imp) <- NULL
  
  imp <- imp[order(imp$MeanDecreaseGini, decreasing = T),]
  imp$age_group <- paste("Age", age_group[i])
  
  if (i == 1) imp_all <- imp
  else imp_all <- rbind(imp, imp_all)
  
  ## plot and save
  # bmp(paste0("better_vx_uptake_features__excl_county_", age_group[i], ".bmp"),
  #     width = 960, height = 720, units = "px",
  # )
  # 
  # ggplot(imp, aes(x = MeanDecreaseGini, y = reorder(Features, +MeanDecreaseGini))) +
  #   geom_bar(stat="identity", fill="steelblue", alpha=.6, width=.4) +
  #   ggtitle(paste0("IMPORTANT FEATURES OF COVID-19 VACCINATION UPTAKE IN CHILDREN BETWEEN ", ## CHANGE LABEL HERE
  #                  age_group[i])) +
  #   xlab("Importance of Features") +
  #   ylab("Features")
  # 
  # dev.off()
  
  sink(file=paste0("Importance_score.csv"))
  print(imp_all)
  sink(file=NULL)
  
  # make predictions using the model 
  # we can try using the imputed data
  predictions <- predict(rf_model_1, data)
  
  # evaluate the model's performance, e.g., using confusion matrix or other metrics
  confusion_matrix <- table(data$Vaccination_status, predictions)
  
  # print the confusion matrix
  sink(file=paste0("confusion_matrix_", age_group[i], ".txt"))
  print(confusion_matrix)
  sink(file=NULL)
  
  
  ########################################
  # no imputation needed after discussion
  ########################################
  
}

imp_all$age_group <- as.factor(imp_all$age_group)
bmp(paste0("better_vx_uptake_factors__excl_county_by_age", ".bmp"),
    width = 1280, height = 960, units = "px",
)

ggplot(imp_all, aes(x = MeanDecreaseGini, 
                    y = reorder(Features, +MeanDecreaseGini), 
                    # group = age_group, fill = age_group
                    )) +
  geom_bar(position = "dodge", stat="identity", 
           fill = "steelblue",
           alpha=.6, width=.4) +
  facet_wrap(~age_group, 
             scales="free_x", 
             ncol=3,  
             # labeller= variable_labeller
             ) +
  ggtitle(paste0("IMPORTANT FACTORS OF COVID-19 VACCINATION UPTAKE IN CHILDREN BETWEEN BY AGE GROUP")) +
  xlab("Importance of Factors") +
  ylab("Mean Decrease Gini") +
  theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,
                                  face="bold")) 

dev.off()


## VX UPTAKE BY CALENDAR TIME
setwd("/ess/p1921/data/durable/VAC4EU datasets/Delivery Feb-May 2023/SYSVAK/")
files <- list.files(pattern = ".csv")
files
df <- vroom(files)
df <- setDT(df)
head(df)
df <- df[,.(KOBLINGSNOEKKEL,
            ALDERIDAGERVEDKONSULTASJON,
            VAKSINEKODE,
            VAKSINASJONSDATO_DIFF,
            PREPARATBESKRIVELSE)]
gc()

setnames(df, c("KOBLINGSNOEKKEL", 
               "ALDERIDAGERVEDKONSULTASJON",
               "VAKSINASJONSDATO_DIFF",
               "VAKSINEKODE", 
               "PREPARATBESKRIVELSE"),
         c("person_id",
           "age",
           "vx_admin_date",
           "vaccode",
           "vx_brand"))

preparat <- openxlsx::read.xlsx("/ess/p1921/data/durable/vac4eu/scripts/vaksinekoder.xlsx")
preparat[preparat=="37622"] <- "JAN03"
preparat <- setDT(preparat)
preparat <- unique(preparat, by = "vaccode")
vx <- merge(df, preparat, by = "vaccode", all.x = TRUE) 
table(vx$vx_manufacturer)

vx <- vx[atc_code == "J07BX03"]
gc()

vx[vx_brand=="Ikke oppgitt"]$vx_brand <- "unknown"

vx$age <- round(vx$age/365)
vx <- vx[age< 18 & age > 4] # 421028
vx$age_group <- ""
vx[age<=11 & age >= 5]$age_group <- "5-11"
vx[age<=15 & age >= 12]$age_group <- "12-15"
vx[age<=17 & age >=16]$age_group <- "16-17"
length(unique(vx[age_group == "5-11"]$person_id)) # 8313 vs. 9645
length(unique(vx[age_group == "12-15"]$person_id)) # 186600 vs. 197699
length(unique(vx[age_group == "16-17"]$person_id)) # 120493 vs. 105538

nrow(vx[age_group == "5-11"]) # 9645
nrow(vx[age_group == "12-15"]) # 211016
nrow(vx[age_group == "16-17"]) # 200367


vx_info <- data.table(table(vx$vx_brand, vx$vx_manufacturer))
vx_info <- vx_info[!N==0]
tb <- vx_info
rownb <- vector("integer", nrow(tb))
for (i in 1:nrow(tb)){
  if (tb[i][["V2"]] == tb[i+1][["V2"]] ){
    tb[i][["N"]] <- tb[i][["N"]] + tb[i+1][["N"]]
    rownb[i+1] <- i+1
    print(rownb)
  }else i = i+1
}

rownb <- rownb[!rownb == 0]
tb <- tb[!rownames(tb) %in% rownb]
tb$V1 <- ifelse(tb$V1 == "unknown", tb$V2, tb$V1)


write.csv2(tb, "/ess/p1921/data/durable/Saimazeb/for random forest/vx_brand_info.csv",
           row.names = F)


vx <- merge(vx, ref_date_2023, by = "person_id", all.x = T)
vx$vx_admin_date <- vx$vx_admin_date + vx$ref_date
sum(is.na(vx$vx_admin_date))
summary(vx$vx_admin_date)
vx <- vx[year(vx_admin_date)>2019]
vx <- vx[!is.na(vx$vx_admin_date)]

vx <- vx %>%
  mutate(year_month = format(vx_admin_date, "%Y-%m"))

uptake_cal <- vx %>% 
  group_by(age_group, year_month) %>%
  summarise(monthly_uptakes = n())

fwrite(uptake_cal, "monthly_uptakes.csv")

ggplot(uptake_cal, aes(x = year_month, y = monthly_uptakes, fill = age_group)) +
  geom_bar(position = "dodge", stat="identity", 
           fill = "steelblue",
           alpha=.6, width=.4) +
  facet_wrap(~age_group, 
             scales="free", 
             ncol=1,  
             # labeller= variable_labeller
  )  +
  labs(title = "Monthly Uptakes by Age Group", x = "Month", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
