



# Arctic_flux_database Site Data

# 29/11/2021

# Jan Dietrich

### Import of site specific datqa in r dataframe

# this script pulls out all SITE_DATA sheets from all excel sheets
# and merges them into on big table with SITE_ID_YEAR as a unique key







##### set up envir ----
rm(list = ls())

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
folder <- "C:/Users/jandi/OneDrive/Documents/arctictundra/arcticflux.database/data_tables/raw/"

files <- dir(folder,
             pattern = ".xlsx")

file <- files[1]


importSiteData <- function(folder, files){
  site_data_list <- list()
  
  
  for (file in files) {
    path <- suppressMessages(paste(folder,
                                    file,
                                    sep = ""
    ))
  
  
    ## add climate data from sheets
    climate_data <- read_excel(path,
      sheet = "2_SITE_DATA",
      skip = 1
    )
  
  
  
  
    climate_data <- climate_data[, c(1, 2)]
  
    climate_data <- rbind(
      row_to_names(data.frame(c("Site_ID", "Year"),
                              c(names(climate_data)[2], str_sub(file, -9, -6)
                                )),
                   1,
                   remove_row = F),
      climate_data
    )
    if (climate_data[2,2] == "dart"){
      climate_data[2,2] <- str_sub(gsub("_dart.xlsx","",file),-4,-1)
    }
  
  
    print(file)
    names(climate_data) <- NULL
    climate_data <- as.data.frame(t(climate_data))
    climate_data <- row_to_names(climate_data, 1, remove_row = T, remove_rows_above = T)
    climate_data <- clean_names(climate_data)
    
    
    climate_data$n_coord_dd <-  as.numeric(climate_data$n_coord_dd)
    climate_data$e_coord_dd <-  as.numeric(climate_data$e_coord_dd)
    climate_data$mean_annual_temperature_c <-  as.numeric(climate_data$mean_annual_temperature_c)
    climate_data$mean_july_temperature_c <-  as.numeric(climate_data$mean_july_temperature_c)
    climate_data$mean_february_temperature_c <-  as.numeric(climate_data$mean_february_temperature_c)
    
    if ("annual_precipitation_mm_yr_1" %in% colnames(climate_data)){
    
      climate_data$mean_annual_precipitation_mm_yr_1 <-  as.numeric(climate_data$annual_precipitation_mm_yr_1)
    
    } else if ("mean_annual_precipitation_mm_yr_1" %in% colnames(climate_data)){
      
      climate_data$mean_annual_precipitation_mm_yr_1 <-  as.numeric(climate_data$mean_annual_precipitation_mm_yr_1)
      
    }
    
    
    colnames(climate_data) <- gsub("_dd","",colnames(climate_data))
    climate_data <- add_column(climate_data,
                               coord_unit = c("dd"),
                               .after = "e_coord")
  
    colnames(climate_data) <- gsub("_c$","",colnames(climate_data))
    
    climate_data <- add_column(climate_data,
                               mean_annual_temperature_unit = c("°C"),
                               .after = "mean_annual_temperature")
    
    climate_data <- add_column(climate_data,
                               mean_july_temperature_unit = c("°C"),
                               .after = "mean_july_temperature")
    
    climate_data <- add_column(climate_data,
                               mean_february_temperature_unit = c("°C"),
                               .after = "mean_february_temperature")
    
    colnames(climate_data) <- gsub("_mm_yr_1","",colnames(climate_data))
    
    
    if ("annual_precipitation" %in% colnames(climate_data)){
      colnames(climate_data) <- gsub("^annual_precipitation$","mean_annual_precipitation",colnames(climate_data))
    } 
    climate_data <- add_column(climate_data,
                                 mean_annual_precipitation_unit = c("mm yr-1"),
                                 .after = "mean_annual_precipitation")
    
    
    
    
    name <- paste(str_sub(file, 1, -6), "_site.xlsx", sep = "")
  
    
    site_data_list[[file]] <- climate_data
    #assign(name, climate_data)
  
  
    rm(climate_data)
  }
  return(site_data_list)
}


site_data <- importSiteData(folder,files)

bindSiteData <- function(site_data_list){
  
  arctic_sites <- data.frame()
  
  for (station in site_data_list) {
    arctic_sites <- rbind.fill(arctic_sites, station)
  }
  rm(station)
  arctic_sites$mean_annual_precipitation.1 <- NULL
  
  arctic_sites$e_coord[arctic_sites$site_id == "CAN_3"|arctic_sites$site_id == "CAN_4"] <- -arctic_sites$e_coord[arctic_sites$site_id == "CAN_3"|arctic_sites$site_id == "CAN_4"]
  arctic_sites <- distinct(arctic_sites)
  return(arctic_sites)
}


site_dataframe <- bindSiteData(site_data)

colnames(site_dataframe)


splitSiteData <- function(fluxdata, type = "data"){
  
  #assert("Select either data or info",(type %in% c("data","info")))
  
  
  unit_depth <- fluxdata[,grepl(paste(c("site_id","year","unit"), collapse="|"), colnames(fluxdata))]
  
  
  measurement_data <- fluxdata[,!grepl(paste(c("unit"), collapse="|"), colnames(fluxdata))]
  
  if(type == "data"){
    return(measurement_data)
  } else if (type == "info"){
    return(unit_depth)
  } else {
    return(NULL)
  }
}

siteMeta <- splitSiteData(site_dataframe,type = "info")


siteData <- splitSiteData(site_dataframe,type = "data")
#data_list <- Filter(function(x) is(x, "data.frame"), mget(ls()[grepl("_site.xlsx",ls())]))


#site_units <- arctic_sites[,grepl(paste(c("site_id","year","unit"), collapse="|"), colnames(arctic_sites))]
#site_data <-  arctic_sites[,!grepl(paste(c("unit"), collapse="|"), colnames(arctic_sites))]              


#view(arctic_sites)





# write.csv(arctic_sites, paste(getwd(),
#   "/data_tables/arctic_site_data.csv",
#   sep = ""
# ))


# 
# big_data <- merge(arctic_new,
#                   site_data,
#                   by.x = c("site_id_automatic","flux_year_automatic"),
#                   by.y = c("site_id", "year"),
#                   all.x = TRUE)
# 
# bigger_data <- merge(big_data,
#                      site_units,
#                      by.x = c("site_id_automatic","flux_year_automatic"),
#                      by.y = c("site_id", "year"),
#                      all.x = TRUE)



site_lvl <- read_csv("data_tables/summary/df_SITE_LEVEL.csv", 
                          col_types = cols(...1 = col_skip()))

SMD_final <- read_excel("data_tables/summary/SMD_final.xlsx")
colnames(SMD_final)

colnames(site_lvl)


 
contacts <- site_lvl[,c(1,2,3,4,5,6,7,8,9,28)] 

new_cont <- distinct(SMD_final[,c(colnames(site_lvl[,c(1,2,3,4,5,6,7,8,9,28)]))])

new_cont %in% contacts
harry <- anti_join(new_cont, contacts)
chiko <- setdiff(new_cont, contacts)


co2_method <- read_csv("C:/Users/Jan/Desktop/arctic_tundra/data/old/df_FLUX_METHOD_CO2.csv")
colnames(co2_method)

ch4_method <-  read_csv("C:/Users/Jan/Desktop/arctic_tundra/data/old/df_FLUX_METHOD_CH4.csv", 
                        col_types = cols(X1 = col_skip(), Site_Name.x = col_skip(), 
                                         Contact.x = col_skip(), Additional_Contacts.x = col_skip(), 
                                         Site_Name.y = col_skip(), Contact.y = col_skip(), 
                                         Additional_Contacts.y = col_skip()))
colnames(ch4_method)


methods <- rbind(co2_method[,-1],
                 ch4_method)


methods <- merge(methods,
                 site_lvl[,c(1,14:27,32,33,34,35,43)],
                 by= "Site_ID",
                 all.x = TRUE)


meta_data <- site_lvl[,c(1,2,3,4,5,6,7,8,9,28)]


arctic_sites <- merge(arctic_sites,
                      site_lvl[,c(1,10,11,29,30,31,34,35,36,37,38,39,40,41,42,43)],
                      by.x= c("site_id"),
                      by.y = c("Site_ID"))

colnames(arctic_sites)


site_metadata <- arctic_sites[,grepl(paste(c("site_id","year","unit","Weath","Comm"), collapse="|"), colnames(arctic_sites))]
site_data <-  arctic_sites[,!grepl(paste(c("unit","Weath","Comm"), collapse="|"), colnames(arctic_sites))]       



# write.csv(contacts, paste(getwd(),
#                           "/data_tables/contact/contact.csv",
#                           sep = ""
# ))
# 
# write.csv(methods, paste(getwd(),
#                           "/data_tables/method/method.csv",
#                           sep = ""
# ))
# write.csv(site_metadata, paste(getwd(),
#                          "/data_tables/site/site_meta.csv",
#                          sep = ""
# ))
# write.csv(site_data, paste(getwd(),
#                                "/data_tables/site/site_data.csv",
#                                sep = ""
# ))





# 
# for (file in files) {
#   path <- suppressMessages(paste("C:/Users/Jan/Desktop/arctic_tundra/data/final_data/",
#                                  file,
#                                  sep = ""
#   ))
#   
#   
#   ## add climate data from sheets
#   comment_data <- read_excel(path,
#                              sheet = "5_CHECK_ME",
#                              skip = 1
#   )
#   empty_columns <- colSums(is.na(comment_data) | comment_data == "") == nrow(comment_data)
#   
#   comment_data <- comment_data[, !empty_columns]
#   
#   t(comment_data[rowSums(is.na(comment_data)) != ncol(comment_data),])
# }






