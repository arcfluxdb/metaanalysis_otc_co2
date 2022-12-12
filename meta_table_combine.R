# Unit_years for all variables

# 20/12/2021

# Jan Dietrich

### merge meta data together

# this script uses both flux and plot metadata to build a big metadata table

rm(list = ls())


##### libraries ####
library(readxl)
library(tidyverse)
library(stringr)
library(writexl)
library(dplyr)
library(janitor)
library(tibble)
library(lubridate)
library(readr)
library(effsize)
library(plyr)

library(svMisc)



flux_data <- read_csv("data_tables/flux/flux_data.csv")

colnames(flux_data)

flux_data <- distinct(flux_data[,c("site_id_automatic","flux_year_automatic")])

flux_meta <- read_csv("data_tables/flux/flux_meta.csv")
colnames(flux_meta)

flux_meta$x1 <- NULL



unit_year <- merge(flux_data,
                   flux_meta,
                   by = c("site_id_automatic","flux_year_automatic"))

plot_meta <- read_csv("data_tables/plot/plot_meta.csv", 
                      col_types = cols(X1 = col_skip(), treatment = col_skip(), 
                                       plot_id = col_skip(), stand_plot_id = col_skip()))

plot_meta <- distinct(plot_meta)


colnames(plot_meta)

meta_data <- merge(unit_year,
                   plot_meta,
                   by = c(c("site_id_automatic","flux_year_automatic")),
                   all = T)


write.csv(meta_data, paste(getwd(),
                            "/data_tables/summary/unit_year_meta.csv",
                            sep = ""
))
