# TSD_install_path <- ifelse("N:\\durable\\vac4eu\\R_v4.1_packages\\") #Luigi: lagt til for TSD

# Note: directory is called v4.1, but packages seem to load fine on 
# current R version on windows v 4.2 (maybe it was always v4.2)
TSD_install_path <- "N:\\durable\\vac4eu\\R_v4.1_packages\\"

options(download.file.method="libcurl") # 

install.packages("rlist", repos = "https://cran.tsd.usit.no", lib=TSD_install_path) # Jing: needed for lv1b
# install.packages("colorRamps", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)

#not in list, but from script... but from CreateSpells_v_10.R (in L3 functions subfolder)
# install.packages("RcppAlgos", repos = "https://cran.tsd.usit.no", lib=TSD_install_path) 


## For level 1B checks

# has dependencies "gsubfn" and "proto"
install.packages("sqldf", repos = "https://cran.tsd.usit.no", lib=TSD_install_path) 

## For pregnancy algorithm
install.packages("ggthemes", repos = "https://cran.tsd.usit.no", lib=TSD_install_path) 
install.packages("DT", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)

## For D3 scripts..
install.packages("english", repos = "https://cran.tsd.usit.no", lib=TSD_install_path) 
install.packages("peakRAM", repos = "https://cran.tsd.usit.no", lib=TSD_install_path) 
#in to_run_t4.R...##
install.packages("stddiff", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("geeM", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("tableone", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
 # step_41 IR4
install.packages("labeling", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("ggtext", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
# has dependency "this.path" (but don't need to load)
install.packages("logr", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
##



# Moderna scripts
install.packages("janitor", repos = "https://cran.tsd.usit.no", lib=TSD_install_path) # has dependency snakecase
install.packages("fastDummies", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
#install.packages("EnvStats", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("epitools", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("dplyr", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)

install.packages("sf", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("epiR", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)

install.packages("yamal", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("jquerylib", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("highr", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("labeling", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("farver", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)

# natmyo
install.packages("epitools", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("doFuture", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)

install.packages("rngtools", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
install.packages("doRNG", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)

install.packages("fuzzyjoin", repos = "https://cran.tsd.usit.no", lib=TSD_install_path)
# Pfizer Myocarditis (main objective)

