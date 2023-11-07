
# Note: directory is called "R_v4.2_packages_linux" but packages can be loaded
# in current version (4.3.1) as of 2023-09-27 (maybe it was always v4.3)
TSD_install_path <- "/ess/p1921/data/durable/vac4eu/R_v4.2_packages_linux/"#Luigi: lagt til for TSD

options(download.file.method="libcurl") # 
install.packages("rstudioapi", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("labeling", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)

# for moderna
install.packages("backports", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("tzdb", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("withr", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("rematch", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("crayon", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("broom", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("vroom", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("readxl", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("brew", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("cli", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("rlang", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("roxygen2", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("docstring", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("textshaping", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("gdtools", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("gdtools", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("plotly", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)


# install.packages("harfbuzz", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
# install.packages("fribidi", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("ragg", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("sf", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("flextable", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("officer", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("epiR", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("conflicted", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("ragg", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("/ess/p1921/data/durable/vac4eu/tibble_3.2.1.tar.gz", repos = NULL, type = "source", lib = TSD_install_path)
# install.packages("/ess/p1921/data/durable/vac4eu/tidyverse_2.0.0.tar.gz", repos = NULL, type = "source", lib = TSD_install_path)
install.packages("tidyverse", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)

install.packages("foreach", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("future", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("doFuture", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("doRNG", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
install.packages("fuzzyjoin", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)

# test
install.packages("future.callr", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)


# for pfizer
install.packages("geepack", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)

# for lc
install.packages("Publish", repos = "https://cran.tsd.usit.no", lib = TSD_install_path)
