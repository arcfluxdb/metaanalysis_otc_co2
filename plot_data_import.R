# Arctic_flux_database Plot Data

# 30/11/2021

# Jan Dietrich

### Import of site specific datqa in r dataframe

# this script pulls out all SITE_DATA sheets from all excel sheets
# and merges them into on big table with SITE_ID_YEAR as a unique key

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

######

# enter your directory here where the files are stored
files <- dir(paste(getwd(),
                   "/data_tables/raw/",
                   sep=""),
             pattern = ".xlsx")


#file <- files[65]
file <- files[str_detect(files, "sva_8")]
folder <- "C:/Users/jandi/OneDrive/Documents/arctictundra/arcticflux.database/data_tables/raw/"

##### add vegetation cover for GRE_6 ----
veg_cover <- read_excel(paste(getwd(),
                              "/data_tables/vegetation/veg_cover.xlsx",
                              sep=""))
colnames(veg_cover) <- c("plot_id",
                         "evergreen_shrubs",
                         "deciduous_shrubs",
                         "forbs",
                         "graminoids",
                         "lichens",
                         "mosses",
                         "other",
                         "sum")
veg_cover <- veg_cover[,c(1,2,3,4,5,6,7)]
veg_cover <- veg_cover %>% 
  mutate(total_vascular_gram_forb_shrub = evergreen_shrubs + 
           deciduous_shrubs +
           forbs + 
           graminoids)


##### startloop ----

importplatdata <- function(folder, files, veg_cover){
  datasets <- list()
  for (file in files){
    
    
    ### PLOT METADATA
    # create path to files
    
    path <- suppressMessages(paste(getwd(),
                                   "/data_tables/raw/",
                                   file,
                                   sep=""))
    
    
    plot_data <- read_excel(path, 
                            sheet = "4_PLOT_METADATA",
                            skip = 2)
    
    
    
    plot_data <- plot_data %>% 
      clean_names()
    
    if (sum(!is.na(plot_data$plot_id)) == 0){
      next()
    }
    
    mutates <- names(plot_data)[c(-1:-6)]
    
    # mutates <- mutates[!is.na(mutates)]
    
    
    plot_data[paste0(mutates,"_info")] <- NA
    plot_data[paste0(mutates,"_year")] <- NA
    plot_data[paste0(mutates,"_unit")] <- NA
    
    infos <- names(plot_data)[str_detect(names(plot_data), "_info")]
    
    for (i in 1:length(mutates)){
      
      plot_data[,c(infos[grep(mutates[i], infos)])] <- plot_data[1,c(mutates[i])]
    }
    
    years <- names(plot_data)[str_detect(names(plot_data), "_year")]
    
    for (i in 1:length(mutates)){
      
      plot_data[,c(years[grep(mutates[i], years)])] <- plot_data[2,c(mutates[i])]
    }
    
    units <- names(plot_data)[str_detect(names(plot_data), "_unit")]
    
    for (i in 1:length(mutates)){
      
      plot_data[,c(units[grep(mutates[i], units)])] <- ifelse(plot_data[3,
                                                                        c(mutates[i])] == "other",
                                                              plot_data[4,
                                                                        c(mutates[i])],
                                                              plot_data[3,
                                                                        c(mutates[i])])
    }
    
    plot_data <- plot_data[,which(unlist(lapply(plot_data, function(x)!all(is.na(x))))),with=F]
    
    
    plot_data <- add_column(plot_data,
                            site_id_automatic = rep(paste(toupper(unlist(strsplit(file,
                                                                                  "_"))[1]),
                                                          unlist(strsplit(file,
                                                                          "_"))[2],
                                                          sep = "_"),
                                                    length(plot_data$plot_id)),
                            .before = "treatment")
    
    
    plot_data <- add_column(plot_data,
                            flux_year_automatic = rep(as.numeric(gsub(".xlsx",
                                                                      "",
                                                                      unlist(strsplit(file,
                                                                                      "_"))[3])),
                                                      length(plot_data$plot_id)),
                            .before = "treatment")
    
    
    
    plot_data[plot_data == "NA"] <- NA
    
    
    plot_data <- plot_data %>%
      drop_na(`treatment`) %>% 
      clean_names() %>% 
      remove_empty("cols")
    
    
    plot_data <- data.frame(plot_data[,c(1:7)],plot_data[,sort(names(plot_data[-1:-7]))])
    
    
    if ( file == "sva_8_2018.xlsx"){
      plot_data$treatment[plot_data$treatment == "SVA_8"] <- plot_data$e_coord[plot_data$treatment == "SVA_8"] 
      plot_data$treatment <- gsub("otc", "OTC",plot_data$treatment)
      plot_data$treatment <- gsub("c", "CTL",plot_data$treatment)
    }
    
    # clean treatments 
    plot_data$treatment <- gsub("CTR", "CTL",plot_data$treatment)
    
    
    
    
    #### GRE_6 vegetation ----
    str_detect(file, "gre_6")
    if (str_detect(file, "gre_6")){
      
      plot_data <- plot_data %>% 
        mutate(plot_id = ifelse(treatment == "CTL",
                                paste(plot_id,"C",sep=""),
                                ifelse(treatment == "OTC",
                                       paste(plot_id,"T",sep=""),
                                       ""
                                )))
      
      
      plot_data <- merge(plot_data,
                         veg_cover,
                         by = "plot_id",
                         all = T)
      
      plot_data <- plot_data %>% 
        mutate(site_id_automatic = "GRE_6",
               flux_year_automatic = unique(flux_year_automatic[!is.na(flux_year_automatic)]),
               treatment = ifelse(substr(plot_data$plot_id,2,6)== "LG","LongGrowing",
                                  ifelse(substr(plot_data$plot_id,2,6)== "S","Shading",
                                         ifelse(substr(plot_data$plot_id,2,6)== "SG","ShortGrowing",treatment))))
      
      
      
      fillers <- colnames(plot_data)[str_detect(colnames(plot_data), "_unit$")]
      for(column in fillers){
        plot_data[,column] <- unique(plot_data[!is.na(plot_data[,column]),column]) 
      }  
    }
    
    
    if (file == "SVA_2018"){
      plot_data$treatment[plot_data$treatment == "SVA_8"] <- plot_data$e_coord[plot_data$treatment == "SVA_8"]
      plot_data$treatment[plot_data$treatment == "c"] <- "CTL"
      plot_data$treatment[plot_data$treatment == "otc"] <- "OTC"
    }
    
    
    file <- gsub(".xlsx","_plot",file)
    assign(file, plot_data)
    
    
    file <- gsub("_dart", "", file)
    datasets[[file]] <- plot_data
  }
  rm(plot_data)
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
  distinct(fluxes[,c("site_id_automatic","flux_year_automatic")])
  return(fluxes)
}



data_list <- importplatdata(folder,files, veg_cover)

plots <- bindData(data_list)

# list2env(datasets,envir=.GlobalEnv)v # convert datalist into seperate dataframes
# datasets <- Filter(function(x) is(x, "data.frame"), mget(ls())) # combine to list
colnames(plots)[grep("soil",colnames(plots))]


splitPlotData <- function(fluxdata, type = "data"){
  
  assert("Select either data or info",(type %in% c("data","info")))
  
  
  unit_depth <- fluxdata[,grepl(paste(c("site_id","year","treatment","plot_id","unit$","info$"), collapse="|"), colnames(fluxdata))]
  
  
  measurement_data <- fluxdata[,-c(grep(paste(c("unit$","info$","year$"), collapse="|"), colnames(fluxdata)))[-1]]
  
  if(type == "data"){
    return(measurement_data)
  } else if (type == "info"){
    return(unit_depth)
  } else {
    return(NULL)
  }
}


plotunits <- splitPlotData(plots, type = "info")

plotunits <- distinct(plotunits)

plotdata <- splitPlotData(plots, type = "data")

colnames(plotdata)

view(plotdata[plotdata$treatment == "SVA_8",])


write.csv(plotdata, paste(getwd(),
                                  "/data_tables/plot/plot_data.csv",
                                  sep = ""
))

write.csv(plotunits, paste(getwd(),
                            "/data_tables/plot/plot_meta.csv",
                            sep = ""
))





test <-c("community","abc_unit")
grep("unit$",test)
