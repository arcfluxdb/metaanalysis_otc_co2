# ITEX flux meta-analysis 

# 05/01/2021 
#

# CO2 files

# script to convert excelsheets into R-dataframes

##### set up envir ----
rm(list =ls())

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

##### import all files from folder ----
# get folder with all files

# set dir where files are stored

# enter your directory here where the files are stored
files <- dir("C:/Users/Jan/Desktop/arctic_tundra/data/final_data",
             pattern = ".xlsx")

# quite check to solve possible single issues
file <- files[12]

# swe_5_2017_test
file <- files[str_detect(files, "swe_6_2015")]



# loop to import all
for (file in files){
  
  # create path to files
  path <- suppressMessages(paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data/",
                                 file,
                                 sep=""))
  
  ###  import flux data FLUX DATA
  flux_data <- read_excel(path, 
                          sheet = "3_FLUX_DATA",
                          skip = 1)
  
  
  # get units 
  unitco2 = ifelse(flux_data$CO2[3] == "other", flux_data$CO2[4], flux_data$CO2[3])
  unitch4 = ifelse(flux_data$CH4[3] == "other", flux_data$CH4[4], flux_data$CH4[3])
  
  
  # add additonal data columns if not in data set
  # -> has to be done to get same dataframe size for all datasets
  if (str_detect(file, "sva")){
    names(flux_data)[names(flux_data) == "additional_data"] <- "temp_inside_chamber_add"
  }
  if (str_detect(file, "fin_1")){
    names(flux_data)[names(flux_data) == "additional_data"] <- "temp_inside_chamber_add"
  }
  if (str_detect(file, "nor_11")){
    names(flux_data)[names(flux_data) == "additional_data"] <- "temp_inside_chamber_add"
  } 
  if (str_detect(file, "nor_12")){
    names(flux_data)[names(flux_data) == "additional_data"] <- "temp_inside_chamber_add"
  }
  
  colnames(flux_data) <- tolower(colnames(flux_data))
  if ("additional_data" %in% tolower(colnames(flux_data))){
    names(flux_data)[names(flux_data) == "additional_data"] <-  paste(substr(flux_data[[3,c("additional_data")]],
                                                                             3,
                                                                             nchar(flux_data[[3,c("additional_data")]])),
                                                                      str_replace_all(substr(ifelse(is.na(flux_data[[6,c("additional_data")]]),
                                                                                                    "",
                                                                                                    flux_data[[6,c("additional_data")]]),
                                                                             1,
                                                                             2),
                                                                             "[^[:alnum:]]", " "),
                                                                      ifelse(is.na(flux_data[[6,c("additional_data")]]),
                                                                             "_add",
                                                                             "cm_add"),
                                                                      sep="_")
  }
  if (("additional_data_1" %in% colnames(flux_data))){

    names(flux_data)[names(flux_data) == "additional_data_1"] <- paste(substr(flux_data[[3,c("additional_data_1")]],
                                                                              3,
                                                                              nchar(flux_data[[3,c("additional_data_1")]])),
                                                                       str_replace_all(substr(ifelse(is.na(flux_data[[6,c("additional_data_1")]]),
                                                                                              "",
                                                                                              flux_data[[6,c("additional_data_1")]]),
                                                                                              1,
                                                                                              2),
                                                                                       "[^[:alnum:]]", " "),
                                                                       ifelse(is.na(flux_data[[6,c("additional_data_1")]]),
                                                                              "_add",
                                                                              "cm_add"),
                                                                       sep="_")
  }
  if (("additional_data_2" %in% colnames(flux_data))){
    names(flux_data)[names(flux_data) == "additional_data_2"] <-  paste(substr(flux_data[[3,c("additional_data_2")]],
                                                                               3,
                                                                               nchar(flux_data[[3,c("additional_data_2")]])),
                                                                        str_replace_all(substr(ifelse(is.na(flux_data[[6,c("additional_data_2")]]),
                                                                                                      "",
                                                                                                      flux_data[[6,c("additional_data_2")]]),
                                                                                               1,
                                                                                               2),
                                                                                        "[^[:alnum:]]", " "),
                                                                        ifelse(is.na(flux_data[[6,c("additional_data_2")]]),
                                                                               "_add",
                                                                               "cm_add"),
                                                                        sep="_")
  }
  if (("additional_data_3" %in% colnames(flux_data))){

    names(flux_data)[names(flux_data) == "additional_data_3"] <-  paste(substr(flux_data[[3,c("additional_data_3")]],
                                                                               3,
                                                                               nchar(flux_data[[3,c("additional_data_3")]])),
                                                                        str_replace_all(substr(ifelse(is.na(flux_data[[6,c("additional_data_3")]]),
                                                                                                      "",
                                                                                                      flux_data[[6,c("additional_data_3")]]),
                                                                                               1,
                                                                                               2),
                                                                                        "[^[:alnum:]]", " "),
                                                                        ifelse(is.na(flux_data[[6,c("additional_data_3")]]),
                                                                               "_add",
                                                                               "cm_add"),
                                                                        sep="_")
  }
  
  # drop empty rows and clean_columnnames
  flux_data <- flux_data %>% 
    drop_na(`site_id [automatic]`) %>% 
    clean_names()
  
  
  # get fluxes values as numeric
  # TODO! clean up!
  flux_data$co2 <- as.numeric(flux_data$co2)
  flux_data$co2[flux_data$co2 == "NA" ] <- "NA"
  flux_data$co2[is.na(flux_data$co2)] <- "NA"
  flux_data$co2[flux_data$co2 == "NA" ] <- NA
  flux_data$co2 <- as.numeric(flux_data$co2)
  
  # skip if only ch4 is measured
  if (is.na(mean(as.numeric(flux_data$co2), na.rm = T))){
    rm(flux_data)
    next
  }
  
  
  
  # delete empty columns
  drop_columns <- str_extract(colnames(flux_data),"x..$")[!is.na(str_extract(colnames(flux_data),"x..$"))]
  drop_columns <- append(drop_columns,c("var"))
  flux_data <- flux_data[ , -which(names(flux_data) %in% drop_columns)]
  

  
  
  
  # rename column with r measurement
  colnames(flux_data) <- gsub("2_14","_co2", colnames(flux_data))
  colnames(flux_data) <- gsub("2_16","_ch4", colnames(flux_data))
  
  
  # add column with orginal submitted co2-flux unit
  if (length(unitco2) != 0){
    flux_data <- add_column(flux_data,
                            co2_unit = rep(unitco2,
                                           length(flux_data$co2)),
                            .after = "co2")  
  } else {
    flux_data <- add_column(flux_data,
                            co2_unit = rep(NA,
                                           length(flux_data$co2)),
                            .after = "co2")
  }
  if (length(unitch4) != 0){
    flux_data <- add_column(flux_data,
                            ch4_unit = rep(unitch4,
                                           length(flux_data$ch4)),
                            .after = "ch4") 
  } else {
    flux_data <- add_column(flux_data,
                            ch4_unit = rep(NA,
                                           length(flux_data$ch4)),
                            .after = "ch4")
  }
  
  
  # check R? values
  # 0.8 is used as threshold all others are excluded
  
  if (!is.nan(mean(as.numeric(flux_data$r_co2),na.rm=T))){
    if (mean(as.numeric(flux_data$r_co2),na.rm=T)<1 ){
      flux_data <- flux_data[flux_data$r_co2 > 0.80,]
    } else {
      flux_data <- flux_data[flux_data$r_co2 > 80,]
    }
  }
  
  #### co2 unit conversion

  if (length(unitco2) != 0 && is.na(unitco2)){
    if (unitco2 == "µmol CO2 m-2 s-1"){
      co2_gC <- as.numeric(flux_data$co2) * 1e-6 * 12 * (60*60*24)
    } else if (unitco2 == "g CO2 m-2 h-1"){
      co2_gC <- (as.numeric(flux_data$co2) / 44) * 12 *24
    }  else if (unitco2 == "g co2 m-2 hr-1"){
      co2_gC <- (as.numeric(flux_data$co2) / 44) * 12 *24
    } else if (unitco2 == "mg CO2 m-2 hr-1"){
      co2_gC <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 *24
    } else if (unitco2 == "mg CO2 m-2 day-1"){
      co2_gC <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12
    } else if (unitco2 == "µmol CO2 m-2 h-1"){
      co2_gC <- as.numeric(flux_data$co2) * 1e-6 * 12 * 24
    } else if (unitco2 == "mmol CO2 m-2 s-1"){
      co2_gC <- as.numeric(flux_data$co2) * 1e-3 * 12 * (60*60*24)
    } else if (unitco2 == "g CO2 m-2 day-1"){
      co2_gC <- (as.numeric(flux_data$co2) / 44) *12
    } else if (unitco2 == "mg CO2 m-2 h-1"){
      co2_gC <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 *24
    } else if (unitco2 == "g CO2-C m-2 half-hour-1"){
      print("hello")
      co2_gC <- as.numeric(flux_data$co2) * 24 * 2
    } else if (unitco2 == "mg CO2-C m-2 h-1"){
      co2_gC <- as.numeric(flux_data$co2) * 1e-3 * 24
    } else if (unitco2 == "gCO2 m-2 hr-1"){
      co2_gC <- (as.numeric(flux_data$co2) / 44) * 12 * 24
    } else if (unitco2 == "mg C-CO2 /m2 / day"){
      co2_gC <- as.numeric(flux_data$co2) * 1e-3
    } else if (unitco2 == "mg C-CO2 / m2 day"){
      co2_gC <- as.numeric(flux_data$co2) * 1e-3
    } else if (unitco2 == "mgCO2m-2h-1"){
      co2_gC <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 * 24
    } else if (unitco2 == "mgC.m2.hr"){
      co2_gC <- as.numeric(flux_data$co2)* 1e-3 * 24
    } else {
      
      # double check for problematic set
      # TODO! clean up!
      if (file == "gre_6_2010.xlsx" ){
        
        co2_gC <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 * 24
        
      }
    }
    
    flux_data <- add_column(flux_data,
                            co2_gC = co2_gC,
                            .after = "co2")
  } else {
    flux_data <- add_column(flux_data,
                            co2_gC = rep(NA,
                                         length(flux_data$co2)),
                            .after = "co2")
  }
  
  
  
  # convert units for site with two different ones.
  if (file == "can_5_2019.xlsx"){
    
    # switch conversion based on flux_ID
    flux_data <- flux_data %>% 
      mutate(co2_gC = ifelse(flux_id_automatic == "CAN_5_CO2GS_2019",
                             co2 * 1e-6 * 12 * (60*60*24),
                             co2_gC))
    
  }

  ### PLOT METADATA
  plot_data <- read_excel(path, 
                          sheet = "4_PLOT_METADATA",
                          skip = 2)
  
  plot_data <- plot_data %>% 
    clean_names()
  
  
  
  plot_data[plot_data == "NA"] <- NA
  
  
  
  switchcols <- colnames(plot_data)[!is.na(str_extract(colnames(plot_data), "_1"))]
  
  for(column in switchcols){
    names(plot_data)[names(plot_data) == column] <- gsub("_1",
                                                         ifelse(is.na(plot_data[1,column]),
                                                                "_no_info",
                                                                plot_data[1,column]),
                                                         names(plot_data)[names(plot_data) == column])     
  }
  switchcols <- colnames(plot_data)[!is.na(str_extract(colnames(plot_data), "_2"))]
  
  for(column in switchcols){
    names(plot_data)[names(plot_data) == column] <- gsub("_2",
                                                         ifelse(is.na(plot_data[1,column]),
                                                                "_no_info",
                                                                plot_data[1,column]),
                                                         names(plot_data)[names(plot_data) == column])     
  }
  
  rm(switchcols)
  # drop empty rows
  plot_data <- plot_data %>% 
    drop_na(`treatment`) %>% 
    clean_names()
  
  
  # delete columns which would be double after merge
  drop_columns <- c("var", "stand_plot_id")
  plot_data <- plot_data[ , -which(names(plot_data) %in% drop_columns)]
  
  
  # single case problemsove for empty n_coord
  if (!("n_coord" %in% colnames(plot_data))){
    plot_data <- add_column(plot_data,
                            n_coord = rep(NA, length(plot_data$plot_id)),
                            .before = "e_coord")
  }
  
  
  ##### add microbial data
  if (!("bacterial_bm_dna" %in% colnames(plot_data))){
    plot_data <- add_column(plot_data,
                            bacterial_bm_dna = rep(NA,
                                                   length(plot_data$plot_id)),
                            .after = "total_vascular_gram_forb_shrub")
  } 
  if (!("fungal_bm_dna" %in% colnames(plot_data))){
    plot_data <- add_column(plot_data,
                            fungal_bm_dna = rep(NA,
                                                length(plot_data$plot_id)),
                            .after = "bacterial_bm_dna")
  } 
  if (!("bacterial_bm_weight" %in% colnames(plot_data))){
    plot_data <- add_column(plot_data,
                            bacterial_bm_weight = rep(NA,
                                                      length(plot_data$plot_id)),
                            .after = "fungal_bm_dna")
  } 
  if (!("fungal_bm_weight" %in% colnames(plot_data))){
    plot_data <- add_column(plot_data,
                            fungal_bm_weight = rep(NA,
                                                   length(plot_data$plot_id)),
                            .after = "bacterial_bm_weight")
  } 
  if (!("f_b_ratio" %in% colnames(plot_data))){
    plot_data <- add_column(plot_data,
                            f_b_ratio = rep(NA,
                                            length(plot_data$plot_id)),
                            .after = "fungal_bm_weight")
  } 
  
  
  # clean up columnnames 

  colnames(plot_data) <- gsub("soil_moist","soil_moist_plot",colnames(plot_data))
  colnames(plot_data) <- gsub("thaw_depth","thaw_depth_plot",colnames(plot_data))
  
  # clean treatments 
  plot_data$treatment <- gsub("CTR", "CTL",plot_data$treatment)
  
  
  #### merge flux data and plot data
  
  
  # check for special cases
  if (file== "nor_11_2020.xlsx"){
    final <- merge(x = flux_data,
                   y = plot_data,
                   by.x = "treatment_if_other",
                   by.y = "plot_id",
                   all = TRUE)
    colnames(final)[1] <- "plot_id"
    colnames(final)[1] <- "treatment_if_other"
    
    final$plot_id <- final$treatment_if_other
  
    
    
  } else if (file == "can_5_2019.xlsx"){
    flux_data$help <- substr(flux_data$plot_id,2,2)
    plot_data$help <- substr(plot_data$plot_id,2,2)
    final <- merge(x = flux_data,
                   y = plot_data,
                   by = "help",
                   all = TRUE)
    plot_id <- final$plot_id.x
    final$plot_id.x <- NULL
    final$plot_id.y <- NULL
    final$help <- NULL
    
    final <- add_column(final,
                        plot_id = plot_id,
                        .before = "site_id_automatic")
  } else {
    
  #### regular merge for all remaining datasets
    final <- merge(x = flux_data,
                   y = plot_data,
                   by = c("plot_id", "treatment"),
                   all = TRUE)
  }
  
  
  # omit empty data points
  final <- final[!is.na(final$site_id_automatic),]
  
  
  # Co2-fluxes canot be negative
  final <- final[as.numeric(final$co2_gC) >= 0,]
  
  if(sum(is.na(final$leap_year)) > 0){
    final$leap_year <- unique(final$leap_year[!is.na(final$leap_year)])[1]
  }
  
  
  ## add climate data from sheets
  climate_data <- read_excel(path, 
                          sheet = "2_SITE_DATA",
                          skip = 1)
  final$n_coord_site <- rep(climate_data[[2,2]], length(final$site_id_automatic))
  final$e_coord_site <- rep(climate_data[[3,2]], length(final$site_id_automatic))
  final$mean_temp <- rep(climate_data[[4,2]], length(final$site_id_automatic))
  final$mean_jul_temp <- rep(climate_data[[5,2]], length(final$site_id_automatic))
  final$mean_feb_temp <- rep(climate_data[[6,2]], length(final$site_id_automatic))
  final$mean_precip <- rep(climate_data[[7,2]], length(final$site_id_automatic))
  
  
  # switch water table to numeric -> depth kann only be positive?
  final$water_table_depth <- abs(as.numeric(final$water_table_depth))
  
  
  ## switch date formats
  if (file == "swe_16_2011.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "sva_4_2019.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "sva_4_2018.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "fin_1_2013.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "nor_11_2020.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "nor_12_2020.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "sva_1_2016.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "sva_1_2017.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "sva_2_2016.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "sva_2_2017.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "sva_3_2016.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "sva_3_2017.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else if (file == "ala_2_2015.xlsx"){
    final$flux_date <- as.Date(final$flux_date, format = "%d/%m/%y")
  } else {
    final$flux_date <- as.Date(final$flux_date)
  }
  

  

  
  # get running effect size per day with +- 10 around.
  for (day in unique(final$flux_julian_day_automatic)){
    final$cohen10[final$flux_julian_day_automatic == day] <- cohen.d(final$co2_gC[final$treatment == "CTL" & 
                                                                                    abs(as.numeric(final$flux_julian_day_automatic) - as.numeric(day)) <= 10 ],
                                                                     final$co2_gC[final$treatment == "OTC" &
                                                                                    abs(as.numeric(final$flux_julian_day_automatic) - as.numeric(day)) <= 10 ],
                                                                     na.rm = T)$estimate
    
  }
  
  
  final$treatment.y <- NULL
  colnames(final) <- gsub("treatment.x","treatment",colnames(final))
  
  final$treatment[final$treatment == "Other"] <- "OTHER"
  
  
  
  final <- final %>% 
    distinct()
  
  assign(file, final)
  

  
  
  # remove all exessive frames
  rm(flux_data)
  rm(plot_data)
  rm(drop_columns)
  rm(final)
 
  rm(climate_data)
  
}


`1607071521_aus_1_2016.xlsx` <- `1607071521_aus_1_2016.xlsx` %>% 
  mutate(season_automatic = ifelse(leap_year == "Leap",
                                   ifelse(as.numeric(flux_julian_day_automatic) < 61 |
                                            as.numeric(flux_julian_day_automatic) >= 274,
                                          "GS",
                                          "NGS"),
                                   ifelse(as.numeric(flux_julian_day_automatic) >= 274 |
                                            as.numeric(flux_julian_day_automatic) < 60,
                                          "GS",
                                          "NGS")))
`1607071539_aus_1_2017.xlsx` <- `1607071539_aus_1_2017.xlsx` %>% 
  mutate(season_automatic = ifelse(leap_year == "Leap",
                                   ifelse(as.numeric(flux_julian_day_automatic) < 61 |
                                            as.numeric(flux_julian_day_automatic) >= 274,
                                          "GS",
                                          "NGS"),
                                   ifelse(as.numeric(flux_julian_day_automatic) >= 274 |
                                            as.numeric(flux_julian_day_automatic) < 60,
                                          "GS",
                                          "NGS")))
`1607071545_aus_1_2018.xlsx` <- `1607071545_aus_1_2018.xlsx` %>% 
  mutate(season_automatic = ifelse(leap_year == "Leap",
                                   ifelse(as.numeric(flux_julian_day_automatic) < 61 |
                                            as.numeric(flux_julian_day_automatic) >= 274,
                                          "GS",
                                          "NGS"),
                                   ifelse(as.numeric(flux_julian_day_automatic) >= 274 |
                                            as.numeric(flux_julian_day_automatic) < 60,
                                          "GS",
                                          "NGS")))



##### add vegetation cover to GRE_6 ----
veg_cover <- read_excel("C:/Users/Jan/Desktop/arctic_tundra/data/gre_3_veg/veg_cover.xlsx")
gre_6_list <- mget(ls()[str_sub(ls(),12,16) == "gre_6"]) 

colnames(veg_cover) <- c("plot_id",
                         "evergreen_shrubs_x",
                         "deciduous_shrubs_x",
                         "forbs_x",
                         "graminoids_x",
                         "lichens_x",
                         "mosses_x",
                         "other",
                         "sum")

veg_cover <- veg_cover[,c(1,2,3,4,5,6,7)]


for (i in seq(1:length(gre_6_list))){
  station <- gre_6_list[[i]]
  station <- station %>% 
    mutate(plot_id = ifelse(treatment == "CTL",
                            paste(plot_id,"C",sep=""),
                            ifelse(treatment == "OTC",
                                   paste(plot_id,"T",sep=""),
                                   ifelse(treatment_if_other == "ShortGrowing",
                                          paste(plot_id,"SG",sep=""),
                                          ifelse(treatment_if_other == "LongGrowing",
                                                 paste(plot_id,"LG",sep=""),
                                                 paste(plot_id,"S",sep=""))))))
  gre_6_list[[i]] <- station
}

  
for (i in seq(1:length(gre_6_list))){
  station <- gre_6_list[[i]]
  file <- names(gre_6_list[i])
  
  
  station <- merge(station,
                   veg_cover,
                   by = "plot_id",
                   all = T)
  
  station$graminoids <- station$graminoids_x
  station$graminoids_x <- NULL
  
  station$forbs <- station$forbs_x
  station$forbs_x <- NULL
  
  station$lichens <- station$lichens_x
  station$lichens_x <- NULL
  
  station$evergreen_shrubs <- station$evergreen_shrubs_x
  station$evergreen_shrubs_x <- NULL
  
  station$deciduous_shrubs <- station$deciduous_shrubs_x
  station$deciduous_shrubs_x <- NULL 
  
  station$mosses <- station$mosses_x
  station$mosses_x <- NULL
  
  station <- station %>% 
    mutate(total_vascular_gram_forb_shrub = graminoids +
                                            forbs +
                                            deciduous_shrubs + 
                                            evergreen_shrubs + 
                                            mosses +
                                            lichens)
  
  assign(file, station)
}
rm(gre_6_list)
rm(veg_cover)
rm(station)

##### combine all sets to list ----
data_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))

##### outlier dection -----


### 3*standard deviation

for (i in seq(1:length(data_list))){
  station <- data_list[[i]]
  file <- names(data_list)[i]
  
  upper <- mean(station$co2_gC, na.rm=T) + 3* sd(station$co2_gC, na.rm=T)
  lower <- mean(station$co2_gC, na.rm=T) - 3* sd(station$co2_gC, na.rm=T)
  
  station <- station %>% 
    mutate(sd3_out = (co2_gC > upper | co2_gC < lower))
  
  assign(file, station)
  rm(station)
}
data_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))

##### write merged sets to excel sheet -----
 for (i in 1:length(data_list)){
   exportpath <- paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data_combined/CO2_sites/",
                       names(data_list[i]), sep="")
   write_xlsx(data_list[[i]], exportpath)
 }

##### merge all sets to one big data set (arctic_co2) ----

arcticflux <- data.frame()

for (station in data_list){
  arcticflux <- rbind.fill(arcticflux,station)
}
rm(station)
arcticflux <- arcticflux[!is.na(arcticflux$co2_gC),]
arcticflux <- arcticflux %>% 
  distinct()


colSums(is.na(arcticflux))


emptycols <- sapply(arcticflux, function (k) all(is.na(k)))
arcticflux <- arcticflux[!emptycols]
rm(emptycols)
colnames(arcticflux)
colnames(arcticflux)[colnames(arcticflux) == "som_1_add"] <- "som_percent_add"

arcticflux <- arcticflux %>% 
  mutate(tot_shrubs = ifelse(is.na(as.numeric(deciduous_shrubs)),
                             ifelse(is.na(as.numeric(evergreen_shrubs)),
                                    0,
                                    as.numeric(evergreen_shrubs)),
                             ifelse(is.na(as.numeric(evergreen_shrubs)),
                                    as.numeric(deciduous_shrubs),
                                    as.numeric(evergreen_shrubs) +
                                      as.numeric(evergreen_shrubs)
                                          )))


arcticflux$deciduous_shrubs[arcticflux$site_id_automatic == "RUS_1"] <- NA
arcticflux$deciduous_shrubs[arcticflux$site_id_automatic == "RUS_2"] <- NA


# years <- arcticfflux %>%
#   group_by(site_id_automatic,flux_year_automatic) %>%
#   summarise(year = unique(flux_year_automatic, collapse=" "))
# 
# years$year <- 1
# for (i in c(2:length(years$year))){
#   if(years$site_id_automatic[i] == years$site_id_automatic[i-1]){
#     years$year[i] <- years$year[i-1] + 1
#   }
# }
# 
# 
# colnames(arcticflux)
# 
# arcticflux <- merge(arcticflux,
#                     years,
#                     by = c("site_id_automatic","flux_year_automatic"),
#                     all.x = T)
# 
##### write out arctic_co2 to excel ----
exportpath <- paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data_combined/combined_sites/",
                    "arctic_co2.xlsx", sep="")
write_xlsx(arcticflux, exportpath)

write.csv(arcticflux,paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data_combined/combined_sites/",
                           "arctic_co2.csv", sep=""))
#### write out file for metaanalysis----

metaflux <- arcticflux[, c(1:13)]

exportpath <- paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data_combined/combined_sites/",
                    "metaflux_co2.xlsx", sep="")
write_xlsx(metaflux, exportpath)


##### create summary file ----

co2_summary <- arcticflux %>% 
  group_by(site_id_automatic,as.numeric(flux_year_automatic)) %>% 
  dplyr::summarise(n = n(),
            plots = length(unique(plot_id)),
            treatments = paste(unique(treatment),collapse = " "),
            seasons = paste(unique(season_automatic), collapse = " "),
            CO2min = min(co2_gC, na.rm = T),
            CO2max = max(co2_gC, na.rm = T),
            CO2mean = mean(co2_gC, na.rm = T),
            cO2_R = mean(as.numeric(r_co2),na.rm = T),
            n_coord_site = unique(n_coord_site),
            e_coord_site = unique(e_coord_site),
            mean_temp = unique(mean_temp),
            mean_feb_temp = unique(mean_feb_temp),
            mean_jul_temp = unique(mean_jul_temp),
            mean_precip = unique(mean_precip),
            PAR = ifelse(!is.na(mean(as.numeric(par),na.rm = T)),  1,NA),
            AirTemp = ifelse(!is.na(mean(as.numeric(air_temp),na.rm = T)), 1,NA),
            SoilTemp = ifelse(!is.na(mean(as.numeric(soil_temp),na.rm = T)),  1,NA),
            SoilMoist = ifelse(!is.na(mean(as.numeric(soil_moist),na.rm = T)),  1,NA),
            WTdepth = ifelse(!is.na(mean(as.numeric(water_table_depth),na.rm = T)),  1,NA),
            Thaw = ifelse(!is.na(mean(as.numeric(thaw_depth),na.rm = T)),  1,NA),
            SoilMoist_plot = ifelse(!is.na(mean(as.numeric(soil_moist_plot),na.rm = T)),  1,NA),
            soil_moist_30_cm_add =ifelse(!is.na(mean(as.numeric(soil_moist_30_cm_add),na.rm = T)),  1,NA),
            
            
            soiltemp_2_cm_add =ifelse(!is.na(mean(as.numeric(soiltemp_2_cm_add),na.rm = T)),  1,NA),
            soiltemp_5_cm_add =ifelse(!is.na(mean(as.numeric(soiltemp_5_cm_add),na.rm = T)),  1,NA),
            soiltemp_10_cm_add =ifelse(!is.na(mean(as.numeric(soiltemp_10_cm_add),na.rm = T)),  1,NA),
            soiltemp_20_cm_add =ifelse(!is.na(mean(as.numeric(soiltemp_20_cm_add),na.rm = T)),  1,NA),
            soiltemp_30_cm_add =ifelse(!is.na(mean(as.numeric(soiltemp_30_cm_add),na.rm = T)),  1,NA),
            soiltemp_40_cm_add =ifelse(!is.na(mean(as.numeric(soiltemp_40_cm_add),na.rm = T)),  1,NA),
            
            som_organic =ifelse(!is.na(mean(as.numeric(som_organic),na.rm = T)),  1,NA),
            som_organic_2 =ifelse(!is.na(mean(as.numeric(som_organic_2),na.rm = T)),  1,NA),
            som_mixture = ifelse(!is.na(mean(as.numeric(som_mixture),na.rm = T)),  1,NA),
            som_mineral = ifelse(!is.na(mean(as.numeric(som_mineral),na.rm = T)), 1,NA),
            som_percent_add = ifelse(!is.na(mean(as.numeric(som_percent_add),na.rm = T)),  1,NA),
            
            soc_no_info = ifelse(!is.na(mean(as.numeric(soc_no_info),na.rm = T)),  1,NA),
            soc_organic =ifelse(!is.na(mean(as.numeric(soc_organic),na.rm = T)),  1,NA),
            soc_mineral =ifelse(!is.na(mean(as.numeric(soc_mineral),na.rm = T)),  1,NA),
            soc_mixture =ifelse(!is.na(mean(as.numeric(soc_mixture),na.rm = T)),  1,NA),
            
            son_no_info = ifelse(!is.na(mean(as.numeric(son_no_info),na.rm = T)),  1,NA),
            son_organic =ifelse(!is.na(mean(as.numeric(son_organic),na.rm = T)),  1,NA),
            soc_organic_2 =ifelse(!is.na(mean(as.numeric(soc_organic_2),na.rm = T)),  1,NA),
            son_mineral =ifelse(!is.na(mean(as.numeric(son_mineral),na.rm = T)),  1,NA),
            son_mixture =ifelse(!is.na(mean(as.numeric(son_mixture),na.rm = T)),  1,NA),
            
            c_n_no_info = ifelse(!is.na(mean(as.numeric(c_n_no_info),na.rm = T)),  1,NA),
            c_n_organic = ifelse(!is.na(mean(as.numeric(c_n_organic),na.rm = T)),  1,NA),
            c_n_organic_2 =ifelse(!is.na(mean(as.numeric(c_n_organic_2),na.rm = T)),  1,NA),
            c_n_mineral =ifelse(!is.na(mean(as.numeric(c_n_mineral),na.rm = T)),  1,NA),
          
            p_hh2o_no_info = ifelse(!is.na(mean(as.numeric(p_hh2o_no_info),na.rm = T)),  1,NA),
            p_hh2o_mixture = ifelse(!is.na(mean(as.numeric(p_hh2o_mixture),na.rm = T)),  1,NA),
            p_hh2o_organic =ifelse(!is.na(mean(as.numeric(p_hh2o_organic),na.rm = T)),  1,NA),
            p_hh2o_organic_2 =ifelse(!is.na(mean(as.numeric(p_hh2o_organic_2),na.rm = T)),  1,NA),
            p_hh2o_mineral = ifelse(!is.na(mean(as.numeric(p_hh2o_mineral),na.rm = T)),  1,NA),
            
            bulk_dens_organic =ifelse(!is.na(mean(as.numeric(bulk_dens_organic),na.rm = T)),  1,NA),
            bulk_dens_organic_2=ifelse(!is.na(mean(as.numeric(bulk_dens_organic_2),na.rm = T)),  1,NA),
            bulk_dens_mixture = ifelse(!is.na(mean(as.numeric(bulk_dens_mixture),na.rm = T)),  1,NA),
            bulk_dens_mineral = ifelse(!is.na(mean(as.numeric(bulk_dens_mineral),na.rm = T)),  1,NA),
            cm3_bulk_density_add = ifelse(!is.na(mean(as.numeric(cm3_bulk_density_add),na.rm = T)),  1,NA),
            
            temp_inside_chamber_add = ifelse(!is.na(mean(as.numeric(temp_inside_chamber_add),na.rm = T)),  1,NA),
            gravimetric_soilmoist_add = ifelse(!is.na(mean(as.numeric(gravimetric_soilmoist_add),na.rm = T)),  1,NA),
            
            org_layer_depth = ifelse(!is.na(mean(as.numeric(org_layer_depth),na.rm = T)),  1,NA),
            thaw_depth_plot = ifelse(!is.na(mean(as.numeric(thaw_depth_plot),na.rm = T)),  1,NA),
            snow_depth = ifelse(!is.na(mean(as.numeric(snow_depth),na.rm = T)),  1,NA),
            
            community_mean_plant_height = ifelse(!is.na(mean(as.numeric(community_mean_plant_height),na.rm = T)), 1,NA),
            biomass = ifelse(!is.na(mean(as.numeric(biomass),na.rm = T)), 1,NA),
            graminoids = ifelse(!is.na(mean(as.numeric(graminoids),na.rm = T)),  1,NA),
            forbs = ifelse(!is.na(mean(as.numeric(forbs),na.rm = T)),  1,NA),
            deciduous_shrubs = ifelse(!is.na(mean(as.numeric(deciduous_shrubs),na.rm = T)), 1,NA),
            evergreen_shrubs = ifelse(!is.na(mean(as.numeric(evergreen_shrubs),na.rm = T)),  1,NA),
            tot_shrubs =ifelse(!is.na(mean(as.numeric(tot_shrubs),na.rm = T)),  1,NA),
            mosses = ifelse(!is.na(mean(as.numeric(mosses),na.rm = T)),  1,NA),
            lichens = ifelse(!is.na(mean(as.numeric(lichens),na.rm = T)),  1,NA),
            total_vascular_gram_forb_shrub = ifelse(!is.na(mean(as.numeric(total_vascular_gram_forb_shrub),na.rm = T)), 1,NA),
            
            bacterial_bm_dna = ifelse(!is.na(mean(as.numeric(bacterial_bm_dna),na.rm = T)), 1,NA),
            fungal_bm_dna = ifelse(!is.na(mean(as.numeric(fungal_bm_dna),na.rm = T)), 1,NA),
            bacterial_bm_weight = ifelse(!is.na(mean(as.numeric(bacterial_bm_weight),na.rm = T)), 1,NA),
            fungal_bm_weight = ifelse(!is.na(mean(as.numeric(fungal_bm_weight),na.rm = T)), 1,NA),
            f_b_ratio = ifelse(!is.na(mean(as.numeric(f_b_ratio),na.rm = T)), 1,NA)) %>% 
  adorn_totals("row")

                  
colnames(co2_summary)[2] <- "flux_year_automatic"


co2_summary[co2_summary$site_id_automatic == "Total","CO2min"] <- mean(co2_summary$CO2min[co2_summary$site_id_automatic != "Total"])
co2_summary[co2_summary$site_id_automatic == "Total","CO2max"] <- mean(co2_summary$CO2max[co2_summary$site_id_automatic != "Total"])
co2_summary[co2_summary$site_id_automatic == "Total","CO2mean"] <- mean(co2_summary$CO2mean[co2_summary$site_id_automatic != "Total"])


##### write out summary file ----
exportpath <- paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data_combined/summary/",
                    "co2_summary.xlsx", sep="")
write_xlsx(co2_summary, exportpath)

write.csv(co2_summary,paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data_combined/summary/",
                            "co2_summary.csv", sep=""))

##### create meta summary file ----

df_meta <- read_csv("C:/Users/Jan/Desktop/arctic_tundra/data/df_metadata_surveys.csv")

co2_meta <- df_meta[df_meta$C_Loss == "ER",]

co2_meta <- co2_meta %>%
  mutate(Year = ifelse(Year > 90, Year + 1900, Year + 2000))

co2_meta$X1 <- paste(co2_meta$Site_ID, co2_meta$Year, sep = "_")
co2_summary <- add_column(co2_summary,
                          X1 = paste(co2_summary$site_id_automatic,
                                     co2_summary$flux_year_automatic,
                                     sep = "_"),
                          .before = "site_id_automatic")

concat_unique <- function(x){paste(unique(x),  collapse=' ')}
co2_meta<- co2_meta %>% 
  group_by(X1) %>% 
  summarise_all(concat_unique)

co2_meta <- co2_meta[,c(1,17:32,34:36)]#,40,41,42,44,45,47:60,77:78,91:112)]


co2 <- merge(co2_summary,
             co2_meta,
             by = "X1",
             all.x = T) 

rm(co2_meta)
rm(df_meta)


##### write meta_summary file -----

exportpath <- paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data_combined/summary/",
                    "co2_meta_data.xlsx", sep="")
write_xlsx(co2, exportpath)

write.csv(co2,paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data_combined/summary/",
                    "co2_meta_data.csv", sep=""))

##### plot Running Effectsize ----



# # set output directory here
# setwd("C:/Users/Jan/Desktop/arctic_tundra/data/")
# 
# pdf("running_effectsize_c02.pdf")
# 
# for (i in seq(1,length(data_list))){
# 
# 
#   station <- data_list[[i]]
#   name <- names(data_list)[[i]]
# 
#   # if (!("OTC" %in% unique(station$treatment_x))){
#   #   next
#   # }
#   #
#   # effectsize <- cohen.d(station$co2_gC[station$treatment=="OTC"],
#   #                       station$co2_gC[station$treatment=="CTL"],
#   #                       na.rm=T)
# 
#   print(ggplot(data=station,aes(x = flux_date, y = cohen10))+
#           geom_point()+
#           theme(strip.background = element_rect(color=NA,
#                                                 fill=NA,
#                                                 size=1.5,
#                                                 linetype="solid"))+
#           ggtitle(paste("Effectsize CO2 CTL - OTC  ",name))+
#           # annotate("text",
#           #          x = 1.5,
#           #          y = 25,
#           #          label = paste("effectsize",
#           #                        round(effectsize$estimate,
#           #                        digits=2)))+
#           labs(y="effectsize cohen co2", x = "Date", subtitle = paste("# measuring Days:", length(unique(station$flux_julian_day_automatic)),
#                                                                       " # obs: ", length(station$co2_gC))))
# }
# dev.off()


##### plot fluxes by treatment ----

pdf("fluxplots_co2.pdf")


print(ggplot(data=arcticflux,aes(x = treatment, y = co2_gC),fill=co2_gC)+
        stat_boxplot(geom ='errorbar')+
        geom_boxplot(fill='#A4A4A4', color="black")+
        theme_bw()+
        theme(strip.background = element_rect(color=NA,
                                              fill=NA,
                                              size=1.5,
                                              linetype="solid"))+
        ggtitle("CO2 at all stations")+
        scale_y_continuous(limits = c(0, 25)) +
        labs(y="CO2 [g CO2 m-2 day-1]", x = "treatment"))

for (i in seq(1,length(data_list))){
  
  
  station <- data_list[[i]]
  name <- names(data_list)[[i]]
  
  if (is.na(station$co2_gC[1])){
    next
  }
  
  # if (!("OTC" %in% unique(station$treatment_x))){
  #   next
  # }
  # 
  # effectsize <- cohen.d(station$co2_gC[station$treatment=="OTC"],
  #                       station$co2_gC[station$treatment=="CTL"],
  #                       na.rm=T)
  
  print(ggplot(data=station,aes(x = treatment, y = co2_gC),fill=co2_gC)+
          stat_boxplot(geom ='errorbar')+
          geom_boxplot(fill='#A4A4A4', color="black")+
          theme_bw()+
          theme(strip.background = element_rect(color=NA,
                                                fill=NA,
                                                size=1.5,
                                                linetype="solid"))+
          ggtitle(paste("CO2 at ",name))+
          scale_y_continuous(limits = c(0, 25)) +
          # annotate("text",
          #          x = 1.5,
          #          y = 25,
          #          label = paste("effectsize",
          #                        round(effectsize$estimate,
          #                        digits=2)))+
          labs(y="CO2 [g CO m-2 day-1]", x = "treatment"))
}

dev.off()

View(arcticflux %>% 
       group_by(site_id_automatic) %>% 
       summarise(som_organic )

