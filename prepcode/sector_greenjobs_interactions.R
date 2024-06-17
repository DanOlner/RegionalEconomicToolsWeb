#Collate sector jobs / GVA data 
#Link to CPREE green jobs data
#Estimate regional scale
#Think about how jobs shifts might affect economy
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
source('functions/misc_functions.R')
options(scipen = 99)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GET ITL2 / SIC SECTION JOBS COUNTS AND QUICK LOOK----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ITL2 jobs counts for 20 SIC sections
#Via previous BRES data wranngling here:
#https://github.com/DanOlner/ukcompare
itl2.jobs <- readRDS('data/itl2_BRES_jobs_SIC_sections.rds')

#Missing activities of households (non paid...) and extraterritorial, so 19 sections
unique(itl2.jobs$SIC_SECTION_NAME)

#Let's just look at the LQs for that, to remind where SY is nationally
#Via https://github.com/DanOlner/regionalGVAbyindustry/blob/master/GVAcode.R
itl2.jobs <- itl2.jobs %>% 
  split(.$DATE) %>% 
  map(add_location_quotient_and_proportions,
      regionvar = GEOGRAPHY_NAME,
      lq_var = SIC_SECTION_NAME,
      valuevar = COUNT) %>% 
  bind_rows()

LQ_slopes <- compute_slope_or_zero(
  data = itl2.jobs, 
  GEOGRAPHY_NAME, SIC_SECTION_NAME,#slopes will be found within whatever grouping vars are added here
  y = LQ_log, x = DATE)

#Filter down to a single DATE
yeartoplot <- itl2.jobs %>% filter(DATE == 2021)

#Add slopes into data to get LQ plots
yeartoplot <- yeartoplot %>% 
  left_join(
    LQ_slopes,
    by = c('GEOGRAPHY_NAME','SIC_SECTION_NAME')
  )

#Get min/max values for LQ over time as well, for each sector and place, to add as bars so range of sector is easy to see
minmaxes <- itl2.jobs %>% 
  group_by(GEOGRAPHY_NAME,SIC_SECTION_NAME) %>% 
  summarise(
    min_LQ_all_time = min(LQ),
    max_LQ_all_time = max(LQ)
  )

#Join min and max
yeartoplot <- yeartoplot %>% 
  left_join(
    minmaxes,
    by = c('GEOGRAPHY_NAME','SIC_SECTION_NAME')
  )

# place = 'East Yorkshire and Northern Lincolnshire'#highest jobs LQ for manufacturing... 25% of jobs!!
# place = 'Lancashire'#5th highest jobs LQ for manufacturing... 
place = 'South Yorkshire'#11th highest

#Get a vector with sectors ordered by the place's LQs, descending order
#Use this next to factor-order the SIC sectors
sectorLQorder <- itl2.jobs %>% filter(
  GEOGRAPHY_NAME == place,
  DATE == 2021
) %>% 
  arrange(-LQ) %>% 
  select(SIC_SECTION_NAME) %>% 
  pull()

#Turn the sector column into a factor and order by LCR's LQs
yeartoplot$SIC_SECTION_NAME <- factor(yeartoplot$SIC_SECTION_NAME, levels = sectorLQorder, ordered = T)

# Reduce to SY LQ 1+
# lq.selection <- yeartoplot %>% filter(
#   ITL_region_name == place,
#   # slope > 1,#LQ grew relatively over time
#   LQ > 1
# )

#Keep only sectors that were LQ > 1 from the main plotting df
# yeartoplot <- yeartoplot %>% filter(
#   SIC07_description %in% lq.selection$SIC07_description
# )

p <- LQ_baseplot(df = yeartoplot, alpha = 0.1, sector_name = SIC_SECTION_NAME,
                 LQ_column = LQ, change_over_time = slope)

# debugonce(addplacename_to_LQplot)
p <- addplacename_to_LQplot(df = yeartoplot, placename = place,
                            plot_to_addto = p, shapenumber = 16,
                            min_LQ_all_time = min_LQ_all_time, max_LQ_all_time = max_LQ_all_time,#Range bars won't appear if either of these not included
                            value_column = COUNT, sector_regional_proportion = sector_regional_proportion,#Sector size numbers won't appear if either of these not included
                            region_name = GEOGRAPHY_NAME,#The next four, the function needs them all
                            sector_name = SIC_SECTION_NAME,
                            change_over_time = slope,
                            LQ_column = LQ,
                            value_col_ismoney = F
                            )

p

#Manufacturing largest job number, but not most concentrated in SY...

#Would quite like to see the map
LQspread <- itl2.jobs %>% 
  filter(DATE == 2021) %>% 
  group_by(SIC_SECTION_NAME) %>% 
  summarise(LQ_spread = diff(range(LQ))) %>% 
  arrange(-LQ_spread)


#Load ITL2 map data using the sf library
itl2.geo <- st_read('data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp', quiet = T) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)

#Some non matches, check
#Northern Ire just isn't in the BRES data, it's GB
itl2.geo$ITL221NM[!itl2.geo$ITL221NM %in% itl2.jobs$GEOGRAPHY_NAME]
itl2.jobs$GEOGRAPHY_NAME[!itl2.jobs$GEOGRAPHY_NAME %in% itl2.geo$ITL221NM] %>% unique

itl2.geo$ITL221NM[itl2.geo$ITL221NM == 'Northumberland, and Tyne and Wear'] <- 'Northumberland and Tyne and Wear'
itl2.geo$ITL221NM[itl2.geo$ITL221NM == 'West Wales and The Valleys'] <- 'West Wales'

#picking out the fourth highest geographical spread sector
x = 5

#Join map data to a subset of the GVA data
sector_LQ_map <- itl2.geo %>% 
  right_join(
    itl2.jobs %>% filter(
      DATE==2021,
      SIC_SECTION_NAME == LQspread$SIC_SECTION_NAME[x]
    ),
    by = c('ITL221NM'='GEOGRAPHY_NAME')
  )


#Plot map
tm_shape(sector_LQ_map) +
  tm_polygons('LQ_log', n = 9) +
  tm_layout(title = paste0('LQ spread of\n',LQspread$SIC_SECTION_NAME[x],'\nAcross ITL2 regions'), legend.outside = T)


#Sum GB job totals per SIC section (to save API hassle)
gb.jobs <- itl2.jobs %>% 
  select(DATE,SIC_SECTION_NAME,COUNT,SIC_SECTION_CODE) %>% 
  group_by(DATE,SIC_SECTION_NAME) %>% 
  summarise(COUNT = sum(COUNT))


#Random thing: total count of jobs in each ITL2?
itl2.totaljobcount <- itl2.jobs %>% 
  select(DATE,GEOGRAPHY_NAME,COUNT) %>% 
  group_by(DATE,GEOGRAPHY_NAME) %>% 
  summarise(COUNT = sum(COUNT))

#SY 376670, population 1.392 million.
#Those are full time jobs only though.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GET LCREE JOB COUNTS FOR SIC SECTIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data extracted from xls file downloaded from here:
#From the 'FTE by industry' tab, all sectors only
#https://www.ons.gov.uk/economy/environmentalaccounts/datasets/lowcarbonandrenewableenergyeconomyfirstestimatesdataset

#Manually rearranged... le sigh
lcree <- read_csv('data/LCREE_2022_SECTION_v_JOBCOUNT.csv', na = c('~','c'))




