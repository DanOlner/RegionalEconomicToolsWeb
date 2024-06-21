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
print(nomis_get_metadata(id = "NM_189_1", concept = "geography", type = "type"), n = 40)

#Some other ways to reduce download size (industry never works...)
nomis_get_metadata(id = "NM_189_1", concept = "INDUSTRY")
nomis_get_metadata(id = "NM_189_1", concept = "EMPLOYMENT_STATUS")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURE")
nomis_get_metadata(id = "NM_189_1", concept = "MEASURES")
nomis_get_metadata(id = "NM_189_1", concept = "TIME")


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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#USE 5 DIGIT SIC JOB COUNTS, SUM TO 2 DIGIT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Why? 5 digit sums seem to be more accurate than the 2 digit present in the data
#some checks
#Ah yes, doesn't have sections (despite downloadable version having them)
unique(itl2.bres$INDUSTRY_TYPE)
unique(itl2.bres$INDUSTRY_NAME[itl2.bres$INDUSTRY_TYPE=='SIC 2007 division (2 digit)'])

#So we need to merge in SIC lookup
SIClookup <- read_csv('data/SIClookup.csv')

itl2.lq <- itl2.lq %>% 
  left_join(SIClookup %>% select(-SIC_5DIGIT_NAME), by = c('INDUSTRY_CODE' = 'SIC_5DIGIT_CODE'))





