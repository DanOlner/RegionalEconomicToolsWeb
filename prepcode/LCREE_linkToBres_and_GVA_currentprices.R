#Collate sector jobs / GVA data 
#Link to CPREE green jobs data
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
#BRES API download and processing done in two other local scripts
itl2.jobs <- readRDS('data/itl2_BRES_jobs_SIC_sections2015to2022.rds')

#Quick sanity check, comparison of values in previous 2015 to 2021 version
# chk <- readRDS('local/cuttings/itl2_BRES_jobs_SIC_sections2015to2021.rds') %>% select(-SIC_SECTION_CODE)
# 
# chk <- chk %>% 
#   rename(OLDCOUNT = COUNT) %>% 
#   left_join(
#     itl2.jobs,
#     by = c('DATE','GEOGRAPHY_NAME','SIC_SECTION_NAME')
#   ) %>% 
#   mutate(
#     countdiff = OLDCOUNT/COUNT
#   )
# 
# #Moooostly. Some missing values...
# plot(density(chk$countdiff, na.rm = T))
# 
# #Agri values in Scotland, a known issue - no data via DEFRA for these
# View(chk %>% filter(is.na(COUNT)))

#Generally some small shifts in values but nothing that's going to alter outcomes here


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

#Filter down to a single DATE, latest in data
yeartoplot <- itl2.jobs %>% filter(DATE == max(DATE))

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
  DATE == 2022
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
x = 4

#Join map data to a subset of the GVA data
sector_LQ_map <- itl2.geo %>% 
  right_join(
    itl2.jobs %>% filter(
      DATE==max(DATE),
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
  select(DATE,SIC_SECTION_NAME,COUNT) %>% 
  group_by(DATE,SIC_SECTION_NAME) %>% 
  summarise(COUNT = sum(COUNT))




#~~~~~~~~~~~~~~~~~~~~~~~~~~
#TOTAL JOB COUNT CHECKS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#Random thing: total count of jobs in each ITL2?
itl2.totaljobcount <- itl2.jobs %>% 
  select(DATE,GEOGRAPHY_NAME,COUNT) %>% 
  group_by(DATE,GEOGRAPHY_NAME) %>% 
  summarise(COUNT = sum(COUNT))

#save those for elsewhere
# write_csv(itl2.totaljobcount, 'data/itl2_totalFTjobcounts.csv')

#Check correlation to per hour worked (taken from gdp_gaps.qmd)
perhourworked <- read_csv('data/Table A4 Current Price unsmoothed GVA B per hour worked £ ITL2 and ITL3 subregions 2004 to 2021.csv') %>% 
  rename(ITL = `ITL level`, ITLcode = `ITL code`, region = `Region name`) %>% 
  filter(ITL == 'ITL2') %>% 
  pivot_longer(cols = `2004`:`2021`, names_to = 'year', values_to = 'gva') %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  group_by(region) %>%
  mutate(
    movingav = rollapply(gva,3,mean,align='right',fill=NA)
  )

#check ITL match. Lack of match probably NI... nope, it's same two.
table(perhourworked$region %in% itl2.totaljobcount$GEOGRAPHY_NAME)
perhourworked$region[!perhourworked$region %in% itl2.totaljobcount$GEOGRAPHY_NAME]

#After this, only NI doesn't match, which is correct (BRES only GB)
perhourworked$region[perhourworked$region == 'Northumberland, and Tyne and Wear'] <- 'Northumberland and Tyne and Wear'
perhourworked$region[perhourworked$region == 'West Wales and The Valleys'] <- 'West Wales'

#Join per hour worked
itl2.totaljobcount <- itl2.totaljobcount %>% 
  left_join(
    perhourworked %>% select(region,year,movingav),
    by = c('GEOGRAPHY_NAME' = 'region','DATE' = 'year')
  )

plot(itl2.totaljobcount$COUNT[itl2.totaljobcount$DATE == 2015] ~ itl2.totaljobcount$movingav[itl2.totaljobcount$DATE == 2015])
plot(itl2.totaljobcount$COUNT[itl2.totaljobcount$DATE == 2021] ~ itl2.totaljobcount$movingav[itl2.totaljobcount$DATE == 2021])

#Partly that's what economic theory says should happen - larger/denser places are more productive
#But...

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

#coefficient of variation:
#"also known as the relative standard error and is calculated by dividing the standard error of an estimate by the estimate itself"
#Multiplied by 100 to get the CV itself, as it's expressed in % terms
#https://www.ons.gov.uk/methodology/methodologytopicsandstatisticalconcepts/uncertaintyandhowwemeasureit#coefficient-of-variation

#So we can turn it back into a rough standard error easily (though noting there are rounding values on the estimate, so it's not perfect)
lcree$se <- (lcree$CV * lcree$estimate) / 100

#Sanity check on lower and upper 95% CI based on that SE
#Tick - that's good enough for simulation purposes below (can stick straight into rnorm to get sample guesses)
chk <- lcree %>% 
  mutate(
    chk_lower = estimate - (se * 1.96),
    chk_upper = estimate + (se * 1.96)
    )


#LCREE data is UK, BRES is GB
#So let's adjust the LCREE numbers crudely to recognise that
#2022 employment per country
emp.lcree <- read_csv('data/LCREE_2022_employmentpercountry.csv') %>% 
  mutate(percent = (estimate / sum(estimate))*100)

#Northern Ireland has around 1 percent of the jobs
#So that's good for here - adjusting won't be very far off correct if we just reduce all numbers by 1%
#(Though of course it's wrong - no account fo geog variability there)

#Take a percent off job counts
lcree <- lcree %>% 
  mutate(across(estimate:se, ~ . * .99))


#Save for elsewhere
write_csv(lcree,'data/lcree2022_adjustedToMatchGBjobnumbers.csv')

#Quick look at how the sector estimates have changed over time and what the trend is
lcree.plot <- lcree %>%
  filter(SIC_SECTION!='B Mining and quarrying') %>%
  mutate(below5000 = estimate < 4000) %>% 
  filter(!is.na(below5000))

#These should be zeroes now mining removed
# lcree.plot[is.na(lcree.plot)] <- 0


ggplot(lcree.plot %>% filter(!below5000), aes(x = year, y = estimate, colour = fct_reorder(SIC_SECTION,estimate, .desc = T) )) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = `lower CI`, ymax = `upper CI`), position = position_dodge(width = 0.5)) +
  # scale_y_log10() +
  # facet_wrap(~below5000, nrow = 2, scales = 'free_y') +
  scale_color_brewer(palette = 'Paired', direction = -1)
  
ggplot(lcree.plot %>% filter(below5000), aes(x = year, y = estimate, colour = fct_reorder(SIC_SECTION,estimate, .desc = T) )) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = `lower CI`, ymax = `upper CI`), position = position_dodge(width = 0.5)) +
  # scale_y_log10() +
  # facet_wrap(~below5000, nrow = 2, scales = 'free_y') +
  scale_color_brewer(palette = 'Paired', direction = -1)
  


#Needs doing as proportion of GB jobs as well...
#First, agggregate gb level jobs to match LCREE sections
gb.jobs.forplot <- gb.jobs %>% 
  mutate(
    SIC_SECTION_NAME_LCREE = gsub(pattern = ',', replacement = '', x = SIC_SECTION_NAME), 
    SIC_SECTION_NAME_LCREE = case_when(
      SIC_SECTION_NAME_LCREE %in% c("Accommodation and food service activities","Financial and insurance activities","Public administration and defence",
                                    "Human health and social work activities","Arts entertainment and recreation","Other service activities") ~ 'Other activities',
      .default = SIC_SECTION_NAME_LCREE
    )
  )

#Sum for those
gb.jobs.forplot <- gb.jobs.forplot %>% 
  select(DATE, COUNT, SIC_SECTION_NAME_LCREE) %>% 
  group_by(DATE,SIC_SECTION_NAME_LCREE) %>% 
  summarise(COUNT = sum(COUNT, na.rm = T)) %>% 
  ungroup()



lcree.plot.gbjobs <- lcree.plot %>% 
  inner_join(
    gb.jobs.forplot,
    by = c('SIC_SECTION_JOIN' = 'SIC_SECTION_NAME_LCREE','year' = 'DATE')
  )

lcree.plot.gbjobs <- lcree.plot.gbjobs %>% 
  mutate(across(estimate:`upper CI`, ~ (. / COUNT)*100,.names = "percent_of_gb_{.col}" ))

#https://stackoverflow.com/a/12188551
lcree.plot.gbjobs <- do.call(data.frame,lapply(lcree.plot.gbjobs, function(x) replace(x, is.infinite(x),NA)))

#What's the median percent so can break up for plotting?
median(lcree.plot.gbjobs$percent_of_gb_estimate, na.rm = T)

lcree.plot.gbjobs <- lcree.plot.gbjobs %>% 
  mutate(abovemedian = percent_of_gb_estimate > median(lcree.plot.gbjobs$percent_of_gb_estimate, na.rm = T))


# ggplot(lcree.plot.gbjobs %>% filter(abovemedian), aes(x = year, y = percent_of_gb_estimate, 
ggplot(lcree.plot.gbjobs %>% filter(SIC_SECTION!='S Other activities'), aes(x = year, y = percent_of_gb_estimate, 
                                                      colour = fct_reorder(SIC_SECTION,percent_of_gb_estimate, .desc = T) )) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = percent_of_gb_lower.CI, ymax = percent_of_gb_upper.CI), position = position_dodge(width = 0.5)) +
  # scale_y_log10() +
  # facet_wrap(~below5000, nrow = 2, scales = 'free_y') +
  scale_color_brewer(palette = 'Paired', direction = -1)



#~~~~~~~~~~~~~~~~~~~~~
#LCREE + BRES LINK----
#~~~~~~~~~~~~~~~~~~~~~


#CHECK LCREE AND BRES SECTOR CATEGORY MATCH
unique(lcree$SIC_SECTION)[order(unique(lcree$SIC_SECTION))]
# unique(itl2.jobs$SIC_SECTION_NAME)[order(unique(itl2.jobs$SIC_SECTION_CODE))]

#Bit of processing and check match
l.sectors <- substr(unique(lcree$SIC_SECTION),start = 3,stop = 100)
b.sectors <- gsub(pattern = ',', replacement = '', x = unique(itl2.jobs$SIC_SECTION_NAME))

#Quite a few matching sectors with different names that need manually matching up
b.sectors[!b.sectors %in% l.sectors]
l.sectors

l.sectors[l.sectors == 'Water supply; sewerage waste management and remediation activities'] <- 'Water supply; sewerage and waste management'
l.sectors[l.sectors == 'Wholesale and retail trade; repair of motor vehicles and motorcycles'] <- 'Wholesale and retail trade; repair of motor vehicles'

#Rest are all now in 'other activities'
l.sectors[!l.sectors %in% b.sectors]

#Which now includes these (inc. all major public services)
b.sectors[!b.sectors %in% l.sectors]

#So those will need labelling as 'other activities' to match
#replace commas before then either keeping default or grouping into 'other activities' to match LCREE
itl2.jobs <- itl2.jobs %>% 
  mutate(
    SIC_SECTION_NAME_LCREE = gsub(pattern = ',', replacement = '', x = SIC_SECTION_NAME), 
    SIC_SECTION_NAME_LCREE = case_when(
      SIC_SECTION_NAME_LCREE %in% c("Accommodation and food service activities","Financial and insurance activities","Public administration and defence",
                              "Human health and social work activities","Arts entertainment and recreation","Other service activities") ~ 'Other activities',
      .default = SIC_SECTION_NAME_LCREE
    )
  )

#Get rid of start letter
lcree$SIC_SECTION_JOIN <- substr(lcree$SIC_SECTION,start = 3,stop = 100)
lcree$SIC_SECTION_JOIN[lcree$SIC_SECTION_JOIN == 'Water supply; sewerage waste management and remediation activities'] <- 'Water supply; sewerage and waste management'
lcree$SIC_SECTION_JOIN[lcree$SIC_SECTION_JOIN == 'Wholesale and retail trade; repair of motor vehicles and motorcycles'] <- 'Wholesale and retail trade; repair of motor vehicles'

#should have full match now... tick
table(unique(itl2.jobs$SIC_SECTION_NAME_LCREE) %in% lcree$SIC_SECTION_JOIN)





#NOW... SUM THE JOBS IN THE BRES 'OTHER ACTIVITIES' CATEGORY
#Drop all the various LQ calc columns, don't need those here
#Keep sector total proportion so we can use below...
itl2.jobs.lcree <- itl2.jobs %>% 
  select(DATE, GEOGRAPHY_NAME, COUNT, SIC_SECTION_NAME_LCREE) %>% 
  group_by(DATE,GEOGRAPHY_NAME, SIC_SECTION_NAME_LCREE) %>% 
  summarise(COUNT = sum(COUNT)) %>% 
  ungroup()

#Add sector regional proportion back in, lost after summarise
# itl2.jobs.lcree <- itl2.jobs.lcree %>% 
#   split(.$DATE) %>% 
#   map(add_location_quotient_and_proportions,
#       regionvar = GEOGRAPHY_NAME,
#       lq_var = SIC_SECTION_NAME_LCREE,
#       valuevar = COUNT) %>% 
#   bind_rows()

#No, don't do that - we need a slightly different number (implicit in the LQ but we need explicitly)
#Proportion each region has of each sector
itl2.jobs.lcree <- itl2.jobs.lcree %>% 
  group_by(DATE,SIC_SECTION_NAME_LCREE) %>% 
  mutate(sector_national_proportion = COUNT / sum(COUNT, na.rm = T))

#Should sum to one for each sector in a particular year... tick
# itl2.jobs.lcree %>% 
#   group_by(DATE,SIC_SECTION_NAME_LCREE) %>% 
#   summarise(sum(sector_national_proportion))

#And can now join that to the LCREE data
#How many matching years?
#BRES 2015-21 vs LCREE 2014-22
unique(itl2.jobs.lcree$DATE)[unique(itl2.jobs.lcree$DATE) %in% lcree$year]
unique(lcree$year)

itl2.jobs.lcree <- itl2.jobs.lcree %>% 
  inner_join(
    lcree %>% rename(lcree_jobcount = estimate, lcree_lowerCI = `lower CI`, lcree_upperCI = `upper CI`, lcree_cv = CV, lcree_se = se),
    by = c('DATE' = 'year', 'SIC_SECTION_NAME_LCREE' = 'SIC_SECTION_JOIN')
  )

#Now just need to ADJUST ALL LCREE VALUES BY THE REGIONAL SECTOR PROPORTIONS
itl2.jobs.lcree <- itl2.jobs.lcree %>% 
  mutate(
    across(lcree_jobcount:lcree_se, ~ . * sector_national_proportion),
    propcheck = lcree_jobcount / COUNT,
    propcheck_lowerci = lcree_lowerCI / COUNT,
    propcheck_upperci = lcree_upperCI / COUNT
  )

#Look at one year for ease
View(itl2.jobs.lcree %>% filter(DATE == max(DATE), SIC_SECTION_NAME_LCREE == 'Manufacturing'))
View(itl2.jobs.lcree %>% filter(DATE == max(DATE), SIC_SECTION_NAME_LCREE == 'Wholesale and retail trade; repair of motor vehicles'))

#sanity check... looks OK
plot(hist(itl2.jobs.lcree$lcree_jobcount/itl2.jobs.lcree$COUNT, na.rm = T))

#Check LCREE job counts sum sensibly... tick
# itl2.jobs.lcree %>% 
#   group_by(DATE,SIC_SECTION_NAME_LCREE) %>% 
#   summarise(sum(lcree_jobcount, na.rm=T)) %>% View

#SY?
View(itl2.jobs.lcree %>% filter(DATE == max(DATE), GEOGRAPHY_NAME == 'South Yorkshire'))


#~~~~~~~~~~~~~~~~~~~~~~~~
#GVA for SIC SECTIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~

#Will need CURRENT PRICES, not least because 'other activities' sections need summing / summing only valid with CP
#Via previous code here: https://github.com/DanOlner/ukcompare/blob/2d6237cd4917c79c9111989248d1a32df66ceaec/explore_code/GVA_region_by_sector_explore.R#L6166
itl2.cp <- read_csv('data/Table 2c ITL2 current price estimates pounds million 2024.csv')
# chk <- read_csv('data/Table 2c ITL2 current price estimates pounds million 2024.csv')

names(itl2.cp) <- gsub(x = names(itl2.cp), pattern = ' ', replacement = '_')

#Keep SIC sections
#This gets all the letters, nice
cvSICkeeps <- itl2.cp$SIC07_code[substr(itl2.cp$SIC07_code,2,2) == ' '] %>% unique

#Filter out duplicate value rows and make long by year
#Also convert year to numeric
#NEED TO MANUALLY UPDATE LATEST YEAR
itl2.cp <- itl2.cp %>% 
  filter(SIC07_code %in% cvSICkeeps) %>% 
  pivot_longer(`1998`:`2022`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))


#Check against previous year's data up to 2021. Anything different?
#"local/cuttings/Table 2c ITL2 UK current price estimates pounds million 2023.csv"



#Repeat process of binning values to match LCREE categories
#Note also there's extra in the GVA data because it's the whole economy including ones with no jobs in (e.g. imputed rent)

#Do same thing - remove commas, bin LCREE's 'other activities'
itl2.cp <- itl2.cp %>% 
  mutate(
    SIC_SECTION_NAME_LCREE = gsub(pattern = ',', replacement = '', x = SIC07_description), 
    SIC_SECTION_NAME_LCREE = case_when(
      SIC_SECTION_NAME_LCREE %in% c("Accommodation and food service activities","Financial and insurance activities","Public administration and defence",
                                    "Human health and social work activities","Arts entertainment and recreation","Other service activities") ~ 'Other activities',
      .default = SIC_SECTION_NAME_LCREE
    )
  )


#Then need to sum GVA for 'other activities'
itl2.cp <- itl2.cp %>% 
  select(GEOGRAPHY_NAME = Region_name, DATE = year, GVA = value, SIC_SECTION_NAME_LCREE) %>% 
  group_by(DATE,GEOGRAPHY_NAME, SIC_SECTION_NAME_LCREE) %>% 
  summarise(GVA = sum(GVA)) %>% 
  ungroup()



#After that,, check non match... mostly perfect
#Just activities of households, which we don't want for our purposes here, so can just do left join and lose
unique(itl2.jobs.lcree$SIC_SECTION_NAME_LCREE)[!unique(itl2.jobs.lcree$SIC_SECTION_NAME_LCREE) %in% itl2.cp$SIC_SECTION_NAME_LCREE]
unique(itl2.cp$SIC_SECTION_NAME_LCREE)[!unique(itl2.cp$SIC_SECTION_NAME_LCREE) %in% unique(itl2.jobs.lcree$SIC_SECTION_NAME_LCREE)]

#Check region name match, might have those usual suspects being wrong... yeeeeaaah
#2024 data has a new one: "Gloucestershire, Wiltshire and Bath/Bristol Area"
#For that last one, only difference is capitalised A in the 2024 ITL2 data... le sigh
unique(itl2.cp$GEOGRAPHY_NAME)[!unique(itl2.cp$GEOGRAPHY_NAME) %in% unique(itl2.jobs.lcree$GEOGRAPHY_NAME)]

#Fix:
itl2.cp$GEOGRAPHY_NAME[itl2.cp$GEOGRAPHY_NAME == 'Northumberland, and Tyne and Wear'] <- 'Northumberland and Tyne and Wear'
itl2.cp$GEOGRAPHY_NAME[itl2.cp$GEOGRAPHY_NAME == 'West Wales and The Valleys'] <- 'West Wales'
itl2.cp$GEOGRAPHY_NAME[itl2.cp$GEOGRAPHY_NAME == 'Gloucestershire, Wiltshire and Bath/Bristol Area'] <- 'Gloucestershire, Wiltshire and Bath/Bristol area'

#Again, dropping NI because not in BRES data. (Dropping via non match implicitly)
#Same for non matching years and that one sector

#REDUCE NAME now we have all bits in same DF
itl2 <- itl2.jobs.lcree %>% 
  left_join(
    itl2.cp,
    by = c('DATE','GEOGRAPHY_NAME','SIC_SECTION_NAME_LCREE')
    )

#check match

#Far fewer years in LCREE
unique(itl2$DATE)
unique(itl2.cp$DATE)


#Does year difference solely account for row difference?
#Also sector and place match
#Those three should...
#Tick
nrow(
  itl2.cp %>% filter(
    DATE %in% unique(itl2$DATE),
    GEOGRAPHY_NAME %in% unique(itl2$GEOGRAPHY_NAME),
    SIC_SECTION_NAME_LCREE %in% unique(itl2$SIC_SECTION_NAME_LCREE)
    )
  )

#So: this is now LCREE linked to BRES and GVA current price data for latest year
#With an if... then estimate (and CIs) for ITL2 green jobs: "If jobs there were proportional to its sector size"
#Saaaave
saveRDS(itl2,'data/LCREE_BRES_GVAcurrentprices_combo_2015to2022.rds')






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#VERSION EXCLUDING IMPUTED RENT: GVA for SIC SECTIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Will need CURRENT PRICES, not least because 'other activities' sections need summing / summing only valid with CP
#Via previous code here: https://github.com/DanOlner/ukcompare/blob/2d6237cd4917c79c9111989248d1a32df66ceaec/explore_code/GVA_region_by_sector_explore.R#L6166
itl2.cp <- read_csv('data/Table 2c ITL2 current price estimates pounds million 2024.csv')
# chk <- read_csv('data/Table 2c ITL2 current price estimates pounds million 2024.csv')

names(itl2.cp) <- gsub(x = names(itl2.cp), pattern = ' ', replacement = '_')

#Keep SIC sections
#This gets all the letters, nice
cvSICkeeps <- itl2.cp$SIC07_code[substr(itl2.cp$SIC07_code,2,2) == ' '] %>% unique



#REPLACE "L (68)" REAL ESTATE ACTIVITIES (WHICH INCLUDES IMPUTED RENT) WITH JUST 68 "Real estate activities, excluding imputed rental"  
cvSICkeeps[cvSICkeeps == 'L (68)'] <- '68'


#Filter out duplicate value rows and make long by year
#Also convert year to numeric
#NEED TO MANUALLY UPDATE LATEST YEAR
itl2.cp <- itl2.cp %>% 
  filter(SIC07_code %in% cvSICkeeps) %>% 
  pivot_longer(`1998`:`2022`, names_to = 'year', values_to = 'value') %>% 
  mutate(year = as.numeric(year))


#Check against previous year's data up to 2021. Anything different?
#"local/cuttings/Table 2c ITL2 UK current price estimates pounds million 2023.csv"



#Repeat process of binning values to match LCREE categories
#Note also there's extra in the GVA data because it's the whole economy including ones with no jobs in (e.g. imputed rent)

#Do same thing - remove commas, bin LCREE's 'other activities'
itl2.cp <- itl2.cp %>% 
  mutate(
    SIC_SECTION_NAME_LCREE = gsub(pattern = ',', replacement = '', x = SIC07_description), 
    SIC_SECTION_NAME_LCREE = case_when(
      SIC_SECTION_NAME_LCREE %in% c("Accommodation and food service activities","Financial and insurance activities","Public administration and defence",
                                    "Human health and social work activities","Arts entertainment and recreation","Other service activities") ~ 'Other activities',
      .default = SIC_SECTION_NAME_LCREE
    )
  )


#Then need to sum GVA for 'other activities'
itl2.cp <- itl2.cp %>% 
  select(GEOGRAPHY_NAME = Region_name, DATE = year, GVA = value, SIC_SECTION_NAME_LCREE) %>% 
  group_by(DATE,GEOGRAPHY_NAME, SIC_SECTION_NAME_LCREE) %>% 
  summarise(GVA = sum(GVA)) %>% 
  ungroup()



#After that,, check non match... mostly perfect
#Just activities of households, which we don't want for our purposes here, so can just do left join and lose
unique(itl2.jobs.lcree$SIC_SECTION_NAME_LCREE)[!unique(itl2.jobs.lcree$SIC_SECTION_NAME_LCREE) %in% itl2.cp$SIC_SECTION_NAME_LCREE]
unique(itl2.cp$SIC_SECTION_NAME_LCREE)[!unique(itl2.cp$SIC_SECTION_NAME_LCREE) %in% unique(itl2.jobs.lcree$SIC_SECTION_NAME_LCREE)]


#FOR THIS VERSION, NEED TO FIX REAL ESTATE ACTIVITIES LINK
#Change it in LCREE so it indicates imputed rental not included
itl2.jobs.lcree.noimputedrent <- itl2.jobs.lcree %>% 
  mutate(
    SIC_SECTION_NAME_LCREE = case_when(
      SIC_SECTION_NAME_LCREE == 'Real estate activities' ~ 'Real estate activities excluding imputed rental',
      .default = SIC_SECTION_NAME_LCREE 
    )
  )

#Check again... tick
unique(itl2.jobs.lcree.noimputedrent$SIC_SECTION_NAME_LCREE)[!unique(itl2.jobs.lcree.noimputedrent$SIC_SECTION_NAME_LCREE) %in% itl2.cp$SIC_SECTION_NAME_LCREE]



#Check region name match, might have those usual suspects being wrong... yeeeeaaah
#2024 data has a new one: "Gloucestershire, Wiltshire and Bath/Bristol Area"
#For that last one, only difference is capitalised A in the 2024 ITL2 data... le sigh
unique(itl2.cp$GEOGRAPHY_NAME)[!unique(itl2.cp$GEOGRAPHY_NAME) %in% unique(itl2.jobs.lcree$GEOGRAPHY_NAME)]

#Fix:
itl2.cp$GEOGRAPHY_NAME[itl2.cp$GEOGRAPHY_NAME == 'Northumberland, and Tyne and Wear'] <- 'Northumberland and Tyne and Wear'
itl2.cp$GEOGRAPHY_NAME[itl2.cp$GEOGRAPHY_NAME == 'West Wales and The Valleys'] <- 'West Wales'
itl2.cp$GEOGRAPHY_NAME[itl2.cp$GEOGRAPHY_NAME == 'Gloucestershire, Wiltshire and Bath/Bristol Area'] <- 'Gloucestershire, Wiltshire and Bath/Bristol area'

#Again, dropping NI because not in BRES data. (Dropping via non match implicitly)
#Same for non matching years and that one sector

#REDUCE NAME now we have all bits in same DF
itl2 <- itl2.jobs.lcree.noimputedrent %>% 
  left_join(
    itl2.cp,
    by = c('DATE','GEOGRAPHY_NAME','SIC_SECTION_NAME_LCREE')
    )

#check match

#Far fewer years in LCREE
unique(itl2$DATE)
unique(itl2.cp$DATE)


#Does year difference solely account for row difference?
#Also sector and place match
#Those three should...
#Tick
nrow(
  itl2.cp %>% filter(
    DATE %in% unique(itl2$DATE),
    GEOGRAPHY_NAME %in% unique(itl2$GEOGRAPHY_NAME),
    SIC_SECTION_NAME_LCREE %in% unique(itl2$SIC_SECTION_NAME_LCREE)
    )
  )

#So: this is now LCREE linked to BRES and GVA current price data for latest year
#With an if... then estimate (and CIs) for ITL2 green jobs: "If jobs there were proportional to its sector size"
#Saaaave
saveRDS(itl2,'data/LCREE_BRES_GVAcurrentprices_combo_2015to2022_MINUS_IMPUTED_RENT.rds')













