#LCREE LINKED DATA
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
source('functions/misc_functions.R')
options(scipen = 99)

#Processed in LCREE_linkToBres_and_GVA_currentprices.R
itl2 <- readRDS('data/LCREE_BRES_GVAcurrentprices_combo_2015to2022.rds') %>% rename(jobcountFT = COUNT)

#Reminder: the "if then" we're doing here - if green jobs had the national proportion in e.g. SY
#What would be number be given the number of e.g. construction FT jobs there?
#This results in the LCREE proportion being the same for each place
#Observe for one year and one sector:
#Propcheck value is ~7.7% (+-6.7 to 8.7%) for all places in the same year
View(itl2 %>% filter(SIC_SECTION_NAME_LCREE =='Construction', DATE == 2022))

#Which means the only regional sectoral green differences here are due to sector size differences in places

#GVA per FT job
itl2$GVAperFT <- (itl2$GVA/itl2$jobcountFT)*1000000


#Check on GVA per FT values for places / sectors
#Will probably need to smooth...


#What proportion of SY’s GVA is green IF proportions match LCREE, compared to other places?
#That's just LCREE job count * GVA per FT summed for each place
greenjobs.gva <- itl2 %>% 
  mutate(
    green_gva_estimate = lcree_jobcount * GVAperFT,
    green_gva_lowerCI = lcree_lowerCI * GVAperFT,
    green_gva_upperCI = lcree_upperCI * GVAperFT
  ) %>% 
  group_by(DATE,GEOGRAPHY_NAME) %>% #then sum GVA for all those, including each place's whole economy for comparison
  summarise(
    green_gva_estimate = sum(green_gva_estimate, na.rm = T)/1000000,
    green_gva_lowerCI = sum(green_gva_lowerCI, na.rm = T)/1000000,
    green_gva_upperCI = sum(green_gva_upperCI, na.rm = T)/1000000,
    gva_total = sum(GVA)
  ) %>% 
  mutate(across(green_gva_estimate:green_gva_upperCI, ~ (. / gva_total) * 100, 
                .names = "percent_of_totalGVA_{.col}"))#find green GVA as proportion of place's total GVA



#What's the votality level across years? 
#Probably high due to BRES jobs number sample volality...
#Actually no, at the aggregate level, reasonably smooth already
p <- ggplot(greenjobs.gva %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'), 
       aes(x = DATE, y = percent_of_totalGVA_green_gva_estimate, colour = GEOGRAPHY_NAME, size = SY)) +
  geom_line() +
  geom_point() +
  # scale_y_log10() +
  scale_size_manual(values = c(0.5,2))

ggplotly(p, tooltip = 'GEOGRAPHY_NAME')


#What's the geography?
itl2.geo <- st_read('data/ITL_geographies/International_Territorial_Level_2_January_2021_UK_BFE_V2_2022_-4735199360818908762/ITL2_JAN_2021_UK_BFE_V2.shp', quiet = T) %>% 
  st_simplify(preserveTopology = T, dTolerance = 100)

itl2.geo$ITL221NM[itl2.geo$ITL221NM == 'Northumberland, and Tyne and Wear'] <- 'Northumberland and Tyne and Wear'
itl2.geo$ITL221NM[itl2.geo$ITL221NM == 'West Wales and The Valleys'] <- 'West Wales'

greensector_map <- itl2.geo %>% 
  right_join(
    greenjobs.gva %>% filter(
      DATE==max(DATE)
      # DATE==2015
    ),
    by = c('ITL221NM'='GEOGRAPHY_NAME')
  )


tm_shape(greensector_map) +
  tm_polygons('percent_of_totalGVA_green_gva_estimate', n = 9, style = 'fisher') +
  tm_layout(title = paste0('Percent green GVA',LQspread$SIC_SECTION_NAME[x],'\nAcross ITL2 regions'), legend.outside = T)






