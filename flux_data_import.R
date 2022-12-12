# Arctic_flux_database Flux Data

# 29/11/2021

# Jan Dietrich

### Import of site specific data in r dataframe

# this script pulls out all SITE_DATA sheets from all excel sheets
# and merges them into on big table with SITE_ID_YEAR as a unique key

rm(list = ls())


##### libraries ####
library(readxl)
library(tidyverse)
# library(stringr)
# library(writexl)
library(plyr)
library(dplyr)
library(janitor)
# library(tibble)
# library(lubridate)
# library(readr)
# library(effsize)
library(svMisc)
library(testit)
###### get data ####
#source(paste(getwd(),"/R_scripts/functions.R", sep=""))


#getwd()
# enter your directory here where the files are stored
folder <- "C:/Users/jandi/OneDrive/Documents/arctictundra/arcticflux.database/data_tables/raw/"

files <- dir(folder,
             pattern = ".xlsx")

#file <- files[55]
file <- files[str_detect(files, "ala_4_2017")]

grep("ALA_4_2017", files)

##### help functions #####


importData <- function(folder, files){
  datasets <- list()
  for (file in files){
    
    
    # create path to files
    path <- suppressMessages(paste(folder,
                                   file,
                                   sep=""))
    
    ###  import flux data FLUX DATA
    flux_data <- suppressMessages(read_excel(path, 
                                             sheet = "3_FLUX_DATA",
                                             skip = 1))
    
    ## cleaning of column appearance ----
    # rename to clean lower case column names for easy use
    flux_data <- flux_data %>% 
      clean_names()
    colnames(flux_data) <- tolower(colnames(flux_data))
    
    
    # delete empty columns
    drop_columns <- str_extract(colnames(flux_data),"x..$")[!is.na(str_extract(colnames(flux_data),"x..$"))]
    if (length(drop_columns) > 1){
      flux_data <- flux_data[ , -which(names(flux_data) %in% drop_columns)]
    }
    
    
    # add new columns for co2 units and extra info ----
    flux_data <- add_column(flux_data,
                            co2_unit = rep(ifelse(flux_data$co2[3] == "other", flux_data$co2[4], flux_data$co2[3]),
                                           length(flux_data$co2)),
                            .after = "co2")
    
    flux_data <- add_column(flux_data,
                            co2_freq = rep(flux_data$co2[1],
                                           length(flux_data$co2)),
                            .after = "co2_unit")
    flux_data <- add_column(flux_data,
                            co2_slope = rep(flux_data$co2[2],
                                            length(flux_data$co2)),
                            .after = "co2_freq")
    
    
    # add new columns for ch4 units and extra info ----
    flux_data <- add_column(flux_data,
                            ch4_unit = rep(ifelse(flux_data$ch4[3] == "other", flux_data$ch4[4], flux_data$ch4[3]),
                                           length(flux_data$ch4)),
                            .after = "ch4")
    flux_data <- add_column(flux_data,
                            ch4_freq = rep(flux_data$ch4[1],
                                           length(flux_data$ch4)),
                            .after = "ch4_unit")
    flux_data <- add_column(flux_data,
                            ch4_slope = rep(flux_data$ch4[2],
                                            length(flux_data$ch4)),
                            .after = "ch4_freq")
    flux_data <- add_column(flux_data,
                            ch4_dir = rep(flux_data$ch4[5],
                                          length(flux_data$ch4)),
                            .after = "ch4_slope")
    
    
    # change columns for soiltemp and soilmosi ----
    
    
    if (file == "ala_1_2013.xlsx"){
      flux_data$soil_temp[6] <- 10
      flux_data$soil_moist[6] <- 10
    }
    
    flux_data <- add_column(flux_data,
                            par_unit = rep(flux_data$par[3],
                                           length(flux_data$site_id_automatic)),
                            .after = "par")
    
    flux_data <- add_column(flux_data,
                            air_temp_unit = rep(flux_data$air_temp[3],
                                                length(flux_data$site_id_automatic)),
                            .after = "air_temp")
    
    flux_data <- add_column(flux_data,
                            soil_temp_unit = rep(flux_data$soil_temp[3],
                                                 length(flux_data$site_id_automatic)),
                            .after = "soil_temp")
    
    flux_data <- add_column(flux_data,
                            soil_temp_depth = rep(flux_data$soil_temp[6],
                                                  length(flux_data$site_id_automatic)),
                            .after = "soil_temp_unit")
    
    
    flux_data <- add_column(flux_data,
                            soil_moist_unit = rep(flux_data$soil_moist[3],
                                                  length(flux_data$site_id_automatic)),
                            .after = "soil_moist")
    
    
    flux_data <- add_column(flux_data,
                            soil_moist_depth = rep(flux_data$soil_moist[6],
                                                   length(flux_data$site_id_automatic)),
                            .after = "soil_moist_unit")
    
    flux_data <- add_column(flux_data,
                            water_table_depth_unit = rep(flux_data$water_table_depth[3],
                                                         length(flux_data$site_id_automatic)),
                            .after = "water_table_depth")
    
    flux_data <- add_column(flux_data,
                            thaw_depth_unit = rep(flux_data$thaw_depth[3],
                                                  length(flux_data$site_id_automatic)),
                            .after = "thaw_depth")
    
    
    if (sum(!is.na(str_extract(colnames(flux_data), "add"))) !=0){
      
      
      add_columns <- colnames(flux_data)[!is.na(str_extract(colnames(flux_data), "add"))]
      
      
      for (i in 1:length(add_columns)){
        
        column_nr <- grep(paste("^",colnames(flux_data)[colnames(flux_data) == add_columns[i]],"$",sep=""), colnames(flux_data))
        
        colnames(flux_data)[column_nr] <- word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1)
        
        flux_data <- add_column(flux_data,
                                new_unit = rep(word(unname(c(flux_data[3,column_nr], recursive=TRUE))),
                                               length(flux_data$site_id_automatic)),
                                .after = column_nr)
        
        colnames(flux_data)[column_nr + 1 ] <- paste(word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1),"_unit",sep="")
        
        
        flux_data <- add_column(flux_data,
                                new_depth = rep(word(unname(c(flux_data[6,column_nr], recursive=TRUE))),
                                                length(flux_data$site_id_automatic)),
                                .after = column_nr+1)
        
        colnames(flux_data)[column_nr + 2 ] <- paste(word(unname(c(flux_data[3,column_nr], recursive=TRUE)),2,-1),"_depth",sep="")
        
      }
      
    }
    
    
    
    #   if (file == "swe_17_2015.xlsx" |file == "swe_18_2015.xlsx" ){
    #     colnames(flux_data)[!is.na(str_extract(colnames(flux_data), "add"))] <- word(unname(c(flux_data[3,!is.na(str_extract(colnames(flux_data), "add"))], recursive=TRUE)),2,-1)
    #   
    #     flux_data <- flux_data %>% 
    #       clean_names()
    #     
    #     
    #     word(unname(c(flux_data[3,column_nr], recursive=TRUE)))
    #     
    #     
    #     } else {
    #   
    #     colnames(flux_data)[!is.na(str_extract(colnames(flux_data), "add"))] <- paste0(word(unname(c(flux_data[3,!is.na(str_extract(colnames(flux_data), "add"))], recursive=TRUE)),2,-1),
    #                                                                                "_",
    #                                                                                as.vector(flux_data[6,!is.na(str_extract(colnames(flux_data), "add"))]) )
    #     }
    #   colnames(flux_data) <- gsub("_NA","", colnames(flux_data))
    # }
    
    flux_data <- clean_names(flux_data[!is.na(flux_data$site_id_automatic),])
    
    flux_data$var <- NULL
    
    if (sum(!is.na(str_extract(colnames(flux_data), "x23"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "x23"] <- "soil_temp_15"
      
    }
    
    
    if (sum(!is.na(str_extract(colnames(flux_data), "temp_inside_chamber_air_t_measured_inside_the_chmber_during_measurement"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "temp_inside_chamber_air_t_measured_inside_the_chmber_during_measurement"] <- "temp_inside_chamber"
      
    }
    
    if (sum(!is.na(str_extract(colnames(flux_data), "temp_inside_chamber_measured_inside_gas_chamber"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "temp_inside_chamber_measured_inside_gas_chamber"] <- "temp_inside_chamber"
      
    }
    
    if (sum(!is.na(str_extract(colnames(flux_data), "temp_inside_chamber_inside_gas_chamber_temp"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "temp_inside_chamber_inside_gas_chamber_temp"] <- "temp_inside_chamber"
      
    }
    
    if (sum(!is.na(str_extract(colnames(flux_data), "temp_inside_chamber_measured_inside_the_chamber"))) !=0){
      
      colnames(flux_data)[colnames(flux_data) == "temp_inside_chamber_measured_inside_the_chamber"] <- "temp_inside_chamber"
      
    }
    
    if (sum(!is.na(str_extract(colnames(flux_data), "air_tremp"))) !=0){
      
      flux_data$air_temp <- flux_data$air_tremp
      
      flux_data$air_temp_unit <- flux_data$air_tremp_unit 
      
      flux_data$air_tremp_depth <- NULL
      
      flux_data$air_tremp <- NULL
      
      flux_data$air_tremp_unit <- NULL
    }
    
    
    
    
    
    if(file == "gre_6_2010.xlsx"){
      flux_data$co2_unit <- "mgCO2m-2h-1"
      flux_data$co2_freq <- "1 measurement per day"
      flux_data$co2_slope <- "Linear slope"
      flux_data$soil_moist_depth <- 6
    }
    
    flux_data$co2[flux_data$co2 == "NA"] <- NA
    if (sum(!is.na(flux_data$co2)) != 0 && !is.na(unique(flux_data$co2_unit))){
      
      unitco2 <- unique(flux_data$co2_unit)
      
      
      
      if (unitco2 == "µmol CO2 m-2 s-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-6 * 12 * (60*60*24)
      } 
      
      else if (unitco2 == "g CO2 m-2 h-1"){
        flux_data$co2_gCm2d1 <- (as.numeric(flux_data$co2) / 44) * 12 *24
      } 
      
      else if (unitco2 == "g co2 m-2 hr-1"){
        flux_data$co2_gCm2d1 <- (as.numeric(flux_data$co2) / 44) * 12 *24
      }
      
      else if (unitco2 == "mg CO2 m-2 hr-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 *24
      }
      
      else if (unitco2 == "mg CO2 m-2 day-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12
      }
      
      else if (unitco2 == "µmol CO2 m-2 h-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-6 * 12 * 24
      }
      
      else if (unitco2 == "mmol CO2 m-2 s-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-3 * 12 * (60*60*24)
      }
      
      else if (unitco2 == "g CO2 m-2 day-1"){
        flux_data$co2_gCm2d1 <- (as.numeric(flux_data$co2) / 44) * 12
      }
      
      else if (unitco2 == "mg CO2 m-2 h-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 *24
      }
      
      else if (unitco2 == "g CO2-C m-2 half-hour-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 24 * 2
      }
      
      else if (unitco2 == "mg CO2m-2h-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2) * 1e-3) / 44) * 12 * 24
      }
      
      else if (unitco2 == "mg CO2-C m-2 h-1"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-3 * 24
      } 
      
      else if (unitco2 == "gCO2 m-2 hr-1"){
        flux_data$co2_gCm2d1 <- (as.numeric(flux_data$co2) / 44) * 12 * 24
      }
      
      else if (unitco2 == "mg C-CO2 /m2 / day"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-3
      }
      
      else if (unitco2 == "mg C-CO2 / m2 day"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2) * 1e-3
      }
      
      else if (unitco2 == "mgCO2m-2h-1"){
        flux_data$co2_gCm2d1 <- ((as.numeric(flux_data$co2)* 1e-3) / 44) * 12 * 24
      }
      
      else if (unitco2 == "mgC.m2.hr"){
        flux_data$co2_gCm2d1 <- as.numeric(flux_data$co2)* 1e-3 * 24
      }
      else {
        
        print("what up?!")
        stop()
      }
      range(flux_data$co2_gCm2d1)
    }
    
    
    
    # harry ----
    
    
    
    if (flux_data$site_id_automatic == "CAN_3")
      
      flux_data$ch4[flux_data$ch4 == "NA"] <- NA 
    if (sum(!is.na(flux_data$ch4)) != 0 && !is.na(unique(flux_data$ch4_unit))){
      
      if (unique(flux_data$site_id_automatic) == "CAN_3"|unique(flux_data$site_id_automatic) == "CAN_4"){
        flux_data$ch4_dir <- "Negative = Incoming to soil vs. Positive = Outgoing from soil"
      }
      
      
      if (unique(flux_data$site_id_automatic) %in%  c("NOR_13",
                                                      "NOR_14",
                                                      "NOR_15",
                                                      "NOR_16",
                                                      "NOR_2",
                                                      "NOR_3",
                                                      "NOR_4",
                                                      "NOR_5",
                                                      "SWE_12",
                                                      "SWE_13",
                                                      "SWE_14",
                                                      "SWE_15")){
        
        flux_data$ch4_dir <- "Negative = Incoming to soil vs. Positive = Outgoing from soil"
      }
      
      unitch4 <- unique(flux_data$ch4_unit)
      
      if (unique(flux_data$ch4_dir)!="Negative = Incoming to soil vs. Positive = Outgoing from soil"){
        flux_data$ch4 <- -as.numeric(flux_data$ch4)
      }
      
      if (unitch4 == "g CH4 m-2 h-1"){
        
        flux_data$ch4_mgCm2d1  <- ((as.numeric(flux_data$ch4) * 1e-6) / 18)* 1e3 * 12 * 24
        
      } else if (unitch4 == "µmol CH4 m-2 s-1"){
        flux_data$ch4_mgCm2d1 <- (as.numeric(flux_data$ch4) * 1e-6)* 1e3 * 12 * (60*60*24)
        
      } else if (unitch4 == "µmol CH4 m-2 h-1"){
        flux_data$ch4_mgCm2d1 <- (as.numeric(flux_data$ch4)* 1e-6) * 1e3  * 12 * 24
        
      } else if (unitch4 == "mg CH4 m-2 day-1"){
        flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4)) / 18) * 12
        
      } else if (unitch4 == "mg CH4 m-2 hr-1"){
        flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4)) / 18) * 12 * 24
        
      } else if (unitch4 == "mg CH4 m-2 hr1"){
        flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4)) / 18) * 12 * 24
        
      } else if (unitch4 == "mg C-CH4 /m2 / day"){
        flux_data$ch4_mgCm2d1 <- as.numeric(flux_data$ch4)
        
      } else if (unitch4 == "mg C-CH4 / m2 day"){
        flux_data$ch4_mgCm2d1 <- as.numeric(flux_data$ch4)
        
      } else if (unitch4 == "g CH4 m-2 day-1"){
        flux_data$ch4_mgCm2d1 <- (as.numeric(flux_data$ch4) *1e3 / 18) * 12
        
      } else if ( unitch4 == "mg C m-2 day-1"){
        flux_data$ch4_mgCm2d1 <- as.numeric(flux_data$ch4) 
      } 
      
    }
    
    # single case unit conversion
    if (file == "chi_4_2014.xlsx"){
      flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4) * 1e-6) / 18) * 12 * 24
    }
    
    
    if (file == "chi_4_2015.xlsx"){
      flux_data$ch4_mgCm2d1 <- ((as.numeric(flux_data$ch4) * 1e-6) / 18) * 12 * 24
    }
    
    
    
    # special transoformations ----  
    
    # missing data for ex, gathered from other datasdets
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    if (file == "NOR_14_2000.xlsx"){
      flux_data$plot_id[flux_data$treatment == "OTC"& flux_data$plot_id == "C_3"] <- "O_3"
    }
    
    
    
    flux_data$flux_year_automatic <- as.numeric(flux_data$flux_year_automatic)
    
    
    flux_data <- flux_data[!is.na(flux_data$flux_date),]
    
    
    colnames(flux_data)[grep("^soiltemp$",colnames(flux_data))] <- "soil_temp_1"
    colnames(flux_data)[grep("^soiltemp_unit$",colnames(flux_data))] <- "soil_temp_unit_1"
    colnames(flux_data)[grep("^soiltemp_depth$",colnames(flux_data))] <- "soil_temp_depth_1"
    
    
    colnames(flux_data)[grep("soiltemp",colnames(flux_data))] <- gsub("soiltemp","soil_temp",colnames(flux_data)[grep("soiltemp",colnames(flux_data))])
    
    colnames(flux_data) <- gsub("co2", "co2_raw", colnames(flux_data))
    colnames(flux_data)[colnames(flux_data)=="r2_14"] <- "co2_raw_r2"
    
    colnames(flux_data) <- gsub("ch4", "ch4_raw", colnames(flux_data))
    colnames(flux_data)[colnames(flux_data)=="r2_16"] <- "ch4_raw_r2"
    
    
    
    if ("co2_raw_gCm2d1" %in% colnames(flux_data)){
      colnames(flux_data)[colnames(flux_data)=="co2_raw_gCm2d1"] <- "co2"
      flux_data <- add_column(flux_data,
                              co2_unit = rep("g C m-2 d-1",
                                             length(flux_data$site_id_automatic)),
                              .after = "co2")
    }
    
    
    if ("ch4_raw_mgCm2d1" %in% colnames(flux_data)){ 
      colnames(flux_data)[colnames(flux_data)=="ch4_raw_mgCm2d1"] <- "ch4"
      flux_data <- add_column(flux_data,
                              ch4_unit = rep("mg C m-2 d-1",
                                             length(flux_data$site_id_automatic)),
                              .after = "ch4")
      flux_data <- add_column(flux_data,
                              ch4_dir = ifelse(!is.na(flux_data$ch4_raw_dir),
                                               "Negative = Incoming to soil vs. Positive = Outgoing from soil",
                                               NA),
                              .after = "ch4_unit")
      
      
    }
    
    
    
    
    colnames(flux_data) <- gsub("water_table_depth","water_table",colnames(flux_data))
    colnames(flux_data) <- gsub("thaw_depth","thaw",colnames(flux_data))
    
    
    
    ## switch date formats
    if (file == "swe_16_2011.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_4_2019.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_4_2018.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "fin_1_2013.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "nor_11_2020.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "nor_12_2020.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_1_2016.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_1_2017.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_2_2016.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_2_2017.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_3_2016.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "sva_3_2017.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else if (file == "ala_2_2015.xlsx"){
      flux_data$flux_date <- as.Date(flux_data$flux_date, format = "%d/%m/%y")
    } else {
      flux_data$flux_date <- as.Date(flux_data$flux_date)
    }
    
    
    
    
    ## leap year
    year <- unique(flux_data$flux_year_automatic)
    leap <- ""
    if((year %% 4) == 0){
      if((year %% 100) == 0){
        if((year %% 400) == 0){
          leap <- "Leap"
        } else {
          leap <- "NonLeap"
        }
      } else {
        leap <- "Leap"
      }
    } else {
      leap <- "NonLeap"
    }
    
    flux_data$leap_year <- leap
    
    ## season_automatic
    
    
    
    if(grepl("aus", file)){
      flux_data <- flux_data %>% 
        mutate(season_automatic = ifelse(leap_year == "Leap",
                                         ifelse(as.numeric(flux_julian_day_automatic) < 61 &
                                                  as.numeric(flux_julian_day_automatic) >= 275,
                                                "GS",
                                                "NGS"),
                                         ifelse(as.numeric(flux_julian_day_automatic) >= 274 &
                                                  as.numeric(flux_julian_day_automatic) < 60,
                                                "GS",
                                                "NGS")))
    } else {
      flux_data <- flux_data %>% 
        mutate(season_automatic = ifelse(leap_year == "Leap",
                                         ifelse(as.numeric(flux_julian_day_automatic) <= 244 &
                                                  as.numeric(flux_julian_day_automatic) >=153,
                                                "GS",
                                                "NGS"),
                                         ifelse(as.numeric(flux_julian_day_automatic) >= 152 &
                                                  as.numeric(flux_julian_day_automatic) <= 243,
                                                "GS",
                                                "NGS")))
    }
    
    ### FLUX_ID_AUTOMATIC
    
    flux_data <- flux_data %>% 
      mutate(flux_id_automatic = paste(site_id_automatic, "_", c_loss, season_automatic, "_",flux_year_automatic,sep = ""))
    
    
    
    
    assign(file, flux_data)
    datasets[[file]] <- flux_data
  }
  
  # free up space
  rm(flux_data)
  return(datasets)
}


bindData <- function(data_list){
  fluxes <- data.frame()
  for (x in 1:length(data_list)){
    
    # just a progress bar
    progress(x, length(data_list), progress.bar = T)
    Sys.sleep(0.01)
    
    
    # grab dataset X
    station <- data_list[[x]] 
    
    # merge with all coumns (creating empty one for NAs)
    fluxes <- rbind.fill(fluxes,station)
  }
  rm(station)
  return(fluxes)
}


cleanFlux <- function(fluxes){
  fluxes$soil_temp_15 <- NULL
  # remove non measurement rows
  fluxes <- fluxes[!is.na(fluxes$flux_id_automatic),]
  
  
  
  
  # standardiye treatment column
  fluxes$treatment[fluxes$treatment == "Other" |fluxes$treatment == "OTHER"] <- NA 
  
  fluxes <- fluxes %>%
    mutate(treatment = coalesce(
      treatment,
      treatment_if_other
    )) %>% 
    mutate(treatment = ifelse(treatment == "NA", "SNOWREMOVAL",treatment)) %>% 
    mutate(treatment = ifelse((site_id_automatic == "ITA_3"|site_id_automatic == "ITA_3")&
                                treatment == "OTC","OTCxSNOWREMOVAL",treatment)) %>% 
    mutate(treatment = ifelse(treatment == "OTC and Litter addition"| treatment == "OTC and litter addition",
                              "OTCxLITTERADDITION", treatment))
  
  
  return(fluxes)
  
}


splitData <- function(fluxdata, type = "data"){
  
  assert("Select either data or info",(type %in% c("data","info")))
  
  
  unit_depth <- fluxdata[,grepl(paste(c("site_id","flux_year","plot_id","unit","depth","freq","slope","dir"), collapse="|"), colnames(fluxdata))]
  unit_depth <- distinct(unit_depth)
  
  measurement_data <- fluxdata[,!grepl(paste(c("unit","depth","freq","slope","dir"), collapse="|"), colnames(fluxdata))]
  cols.num <- colnames(measurement_data)[-1:-11]
  measurement_data[cols.num] <- sapply(measurement_data[cols.num],as.numeric)
  sapply(measurement_data, class)
  
  if(type == "data"){
    return(measurement_data)
  } else if (type == "info"){
    return(unit_depth)
  } else {
    return(NULL)
  }
}


cleanInfo <- function(infodata){
  infodata <- infodata %>% 
    mutate(soil_temp_depth = gsub(" cm","",fluxinfo$soil_temp_depth)) %>% 
    mutate(soil_temp_depth_clean = ifelse(soil_temp_depth == "0-5","5",
                                    ifelse(soil_temp_depth == "0-20","20",
                                           ifelse(soil_temp_depth == "0-10","10",
                                                  ifelse(soil_temp_depth == "5-10","10",
                                                         soil_temp_depth))))) %>% 
    mutate(soil_temp_depth_clean = abs(as.numeric(soil_temp_depth_clean))) %>% 
    mutate(soil_moist_depth = gsub("cm","",fluxinfo$soil_moist_depth)) %>%
    mutate(soil_moist_depth = gsub("integrated","",fluxinfo$soil_moist_depth)) %>% 
    mutate(soil_moist_depth = gsub(" ","",fluxinfo$soil_moist_depth)) %>%
    mutate(soil_moist_depth_clean = ifelse(soil_moist_depth == "0-7.5", "7.5",
                                           ifelse(soil_moist_depth == "0-5", "5",
                                                  ifelse(soil_moist_depth == "0-10", "10",
                                                         ifelse(soil_moist_depth == "0-20", "20",
                                                                ifelse(soil_moist_depth == "0-7", "7",
                                                                       ifelse(soil_moist_depth == "0-6", "6",
                                                                              ifelse(soil_moist_depth == "5-10", "10",
                                                                                     ifelse(soil_moist_depth == "0-15", "15",
                                                                                            ifelse(soil_moist_depth == "44109", "missing",
                                                                                                   soil_moist_depth
                                                                                     )))))))))) %>% 
  mutate(soil_moist_depth_clean = as.numeric(soil_moist_depth_clean))
  }

# table(infodata$soil_moist_depth_clean)

###### run ####

datasets <- importData(folder,files)

# list2env(datasets,envir=.GlobalEnv)v # convert datalist into seperate dataframes
# datasets <- Filter(function(x) is(x, "data.frame"), mget(ls())) # combine to list

arctic_flux <- bindData(datasets)

clean_flux <- cleanFlux(arctic_flux)

fluxdata <- splitData(clean_flux)

fluxinfo <- splitData(clean_flux, type = "info")


infodata <- cleanInfo(fluxinfo)

colnames(infodata)

# delete column X23-> wrongly named soil temp, but no infos given


colnames(fluxdata)











folder <- "C:/Users/jandi/OneDrive/Documents/arctictundra/arcticflux.database/data_tables/"


write.csv(fluxdata, paste(folder,
                              "flux/fluxdata.csv",
                              sep = ""          ))

write.csv(fluxinfo, paste(getwd(),
                             "/data_tables/flux/flux_meta.csv",
                             sep = ""))

