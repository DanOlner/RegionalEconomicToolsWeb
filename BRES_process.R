#BRES process for use
library(tidyverse)
library(nomisr)
options(scipen = 99)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#USE 5 DIGIT SIC JOB COUNTS, SUM TO 2 DIGIT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

itl2.bres <- readRDS('data/BRES_NUTS2_2015_2022.rds')


#Why? 5 digit sums seem to be more accurate than the 2 digit present in the data
#some checks
#Ah yes, doesn't have sections (despite downloadable version having them)
# unique(itl2.bres$INDUSTRY_TYPE)
# unique(itl2.bres$INDUSTRY_NAME[itl2.bres$INDUSTRY_TYPE=='SIC 2007 division (2 digit)'])

#Only need to keep the BRES 5 digit to sum...
itl2.bres <- itl2.bres %>% filter(INDUSTRY_TYPE == 'SIC 2007 subclass (5 digit)')


#So we need to merge in SIC lookup
SIClookup <- read_csv('data/SIClookup.csv')

#Check 5 digit name match... tick!
table(unique(itl2.bres$INDUSTRY_NAME) %in% unique(SIClookup$SIC_5DIGIT_NAME))

itl2.bres <- itl2.bres %>% 
  left_join(
    SIClookup %>% select(SIC_5DIGIT_NAME,SIC_SECTION_LETTER,SIC_SECTION_CODE,SIC_SECTION_NAME),
    by = c('INDUSTRY_NAME' = 'SIC_5DIGIT_NAME')
  )


#Sum 5 digit full time time job counts to SIC sections
itl2.bres.sections <- itl2.bres %>% 
  group_by(DATE,GEOGRAPHY_NAME,SIC_SECTION_NAME) %>% 
  summarise(COUNT = sum(OBS_VALUE)) %>% 
  ungroup()


#Save! Remove non job sections while we're at it
saveRDS(
  itl2.bres.sections %>% 
    filter(!SIC_SECTION_NAME %in% c('Activities of households','Activities of extraterritorial organisations and bodies')),
  'data/itl2_BRES_jobs_SIC_sections2015to2022.rds')


