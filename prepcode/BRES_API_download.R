#BRES API download
library(tidyverse)
library(nomisr)
options(scipen = 99)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DOWNLOAD LATEST BRES DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Find latest year
time <- nomis_get_metadata(id = "NM_189_1", concept = "TIME")
latestyear <- as.numeric(time$id[length(time$id)])

years = c(2015:latestyear)
# years = c(2015:2021)

#Reminder of geographies
#TYPE438 is nuts 2016 level 2, which matches ITL2 including SY
#print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 40)

#Looking for some other ways to reduce download size (industry never works...)
# nomis_get_metadata(id = "NM_189_1", concept = "INDUSTRY")
# nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS")
# nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
# nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
# nomis_get_metadata(id = "NM_189_1", concept = "TIME")


download_all_BRESopen <- function(year){
  z <- nomis_get_data(id = "NM_189_1", time = as.character(year), 
                      geography = "TYPE438", measures = 20100, 
                      EMPLOYMENT_STATUS = 2,
                      select = c('DATE','GEOGRAPHY_NAME','INDUSTRY_NAME','INDUSTRY_TYPE','OBS_VALUE')
                      )
  saveRDS(z,paste0('local/data/BRES_NUTS2_',year,'.rds'))
}

lapply(years, function(x) download_all_BRESopen(x))


#Combine into one DF
itl2.bres <- list.files(path = "local/data/", pattern = "BRES_NUTS2", full.names = T) %>% 
  map(readRDS) %>%
  bind_rows()

#Save for public repo
saveRDS(itl2.bres,paste0('data/BRES_NUTS2_',years[1],'_',years[length(years)],'.rds'))

#Next parts done in "BRES process' to keep this self contained



