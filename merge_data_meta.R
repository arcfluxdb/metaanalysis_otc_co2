



#### merge data and meta  


rm(list = ls())

library(tidyr)
library(stringr)
library(readr)



### data merge
fluxdata <- read_csv("data_tables/flux/fluxdata.csv", 
                     col_types = cols(...1 = col_skip()))


plotdata <- read_csv("data_tables/plot/plot_data.csv", 
                     col_types = cols(...1 = col_skip()))


site_data <- read_csv("data_tables/site/site_data.csv", 
                      col_types = cols(...1 = col_skip()))



method <- read_csv("data_tables/method/method.csv", 
                   col_types = cols(...1 = col_skip()))



colnames(fluxdata)
colnames(plotdata)[-1:-5] <- paste(colnames(plotdata)[-1:-5],"_plot",sep = "")
colnames(site_data)

fluxplot <- merge(fluxdata,
      plotdata,
      by= c("site_id_automatic","flux_year_automatic"  , "treatment"   , "plot_id"),
      all.x=TRUE
)

colnames(fluxplot)

colnames(site_data)[-1:-3] <- paste(colnames(site_data)[-1:-3],"_site",sep = "")
colnames(site_data)


fluxplotsite <- merge(fluxplot,
      site_data,
      by.x = c("site_id_automatic","flux_year_automatic"),
      by.y = c("site_id","year"),
      all.x = TRUE
      )






colnames(method)

method$duration <- method$Year - method$Exp_Start

colnames(method)
method$Site_Flux_ID <- NULL


test <- method %>% 
  gather(variable, value, -c(Site_ID:C_Loss,Exp_Warming_Type:OTC_Height,Exp_Start:Comm_Env)) %>%
  unite(temp, C_Loss, variable) %>%
  spread(temp, value)

colnames(test)[-1:-5] <- paste(colnames(test)[-1:-5],"_method",sep = "")

colnames(test)

methoddata <- test[,c(1,5,grep("ER|METH",colnames(test)))]
methodmeta <- test[,-c(grep("ER|METH",colnames(test)))]

fluxplotsitemeth <- merge(fluxplotsite,
                          methoddata,
      by.x = c("site_id_automatic", "flux_year_automatic"),
      by.y = c("Site_ID", "Year"),
      all.x = TRUE
      )


write.csv(fluxplotsitemeth, paste(getwd(),
                           "/data_tables/database/fluxdata.csv",
                           sep = ""
))

### meta data 

flux_meta <- read_csv("data_tables/flux/flux_meta.csv", 
                      col_types = cols(...1 = col_skip()))

flux_meta <- flux_meta %>% 
  mutate(par_unit = paste("u",stringr::str_sub((par_unit[1]),2),sep = ""),
         air_temp_unit = stringr::str_sub(air_temp_unit,2),
         soil_temp_unit = stringr::str_sub(soil_temp_unit,2),
         soil_temp_unit_1 = stringr::str_sub(soil_temp_unit_1,2),
         soil_temp_unit_2 = stringr::str_sub(soil_temp_unit_2,2),
         soil_temp_unit_3 = stringr::str_sub(soil_temp_unit_3,2))



plot_meta <- read_csv("data_tables/plot/plot_meta.csv", 
                      col_types = cols(...1 = col_skip()))





  
colnames(flux_meta)

colnames(plot_meta)[-1:-5] <- paste(colnames(plot_meta)[-1:-5],"_plot",sep = "")


fluxplot_meta <- merge(flux_meta,
      plot_meta,
      by = c("site_id_automatic", "flux_year_automatic", "plot_id" ),
      all.y = TRUE)


fluxplot_meta$plot_id <- NULL
fluxplot_meta$treatment     <- NULL                       
fluxplot_meta$stand_plot_id <- NULL
fluxplot_meta <- fluxplot_meta[!duplicated(fluxplot_meta),]
colnames(fluxplot_meta)
colnames(methodmeta)

fluxplotmethod_meta <- merge(fluxplot_meta,
                             methodmeta,
                       by.x = c("site_id_automatic", "flux_year_automatic"),
                       by.y = c("Site_ID", "Year"),
                       all.x = TRUE)

fluxplotmethod_meta$Site_Name <- NULL
fluxplotmethod_meta$Contact <- NULL
fluxplotmethod_meta$Additional_Contacts <- NULL

write.csv(fluxplotmethod_meta, paste(getwd(),
                                  "/data_tables/database/fluxmeta.csv",
                                  sep = ""
))


