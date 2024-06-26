#LCREE LINKED DATA
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
source('functions/misc_functions.R')
options(scipen = 99)

#Processed in LCREE_linkToBres_and_GVA_currentprices.R
itl2 <- readRDS('data/LCREE_BRES_GVAcurrentprices_combo_2015to2022.rds') %>% rename(jobcountFT = COUNT) %>% ungroup()

#Reminder: the "if then" we've done here (in LCREE_linkToBres...) - if green jobs had the national proportion in e.g. SY
#What would be number be given the number of e.g. construction FT jobs there?
#This results in the LCREE proportion being the same for each place
#Observe for one year and one sector:
#Propcheck value is ~7.7% (+-6.7 to 8.7%) for all places in the same year
# View(itl2 %>% filter(SIC_SECTION_NAME_LCREE =='Construction', DATE == 2022))

#Which means the only regional sectoral green differences here are due to sector size differences in places

#GVA per FT job
itl2$GVAperFT <- (itl2$GVA/itl2$jobcountFT)*1000000


#Check on GVA per FT values for places / sectors
#Will probably need to smooth...


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#What proportion of SY’s GVA is green IF proportions match LCREE, compared to other places?----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                .names = "percent_of_totalGVA_{.col}")) %>% #find green GVA as proportion of place's total GVA
  ungroup()









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



#First and last year in data, include error rates
ggplot(greenjobs.gva %>% filter(DATE %in% c(min(DATE),max(DATE))) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'), 
       aes(x = fct_reorder(GEOGRAPHY_NAME,percent_of_totalGVA_green_gva_estimate), y = percent_of_totalGVA_green_gva_estimate, colour = factor(DATE), size = SY)) +
  geom_point(position = position_dodge(width = 0.75), size = 2) +
  geom_errorbar(aes(ymin = percent_of_totalGVA_green_gva_lowerCI, ymax = percent_of_totalGVA_green_gva_upperCI), position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_size_manual(values = c(0.5,1.5)) +
  xlab("")







#VERSION THAT GIVES GREEN JOB GVA AS PERCENT OF NATIONAL GVA BASED ON JOB NUMBERS AND GVA IN THOSE SECTORS
GB.greenjobs.gva <- itl2 %>% 
  mutate(
    green_gva_estimate = lcree_jobcount * GVAperFT,
    green_gva_lowerCI = lcree_lowerCI * GVAperFT,
    green_gva_upperCI = lcree_upperCI * GVAperFT
  ) %>% 
  group_by(DATE) %>% #then sum GVA for all those, including each place's whole economy for comparison
  summarise(
    green_gva_estimate = sum(green_gva_estimate, na.rm = T)/1000000,
    green_gva_lowerCI = sum(green_gva_lowerCI, na.rm = T)/1000000,
    green_gva_upperCI = sum(green_gva_upperCI, na.rm = T)/1000000,
    gva_total = sum(GVA)
  ) %>% 
  mutate(across(green_gva_estimate:green_gva_upperCI, ~ (. / gva_total) * 100, 
                .names = "percent_of_totalGVA_{.col}")) %>% #find green GVA as proportion of place's total GVA
  ungroup()

ggplot(GB.greenjobs.gva, 
       aes(x = DATE, y = percent_of_totalGVA_green_gva_estimate)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = percent_of_totalGVA_green_gva_lowerCI, ymax = percent_of_totalGVA_green_gva_upperCI), position = position_dodge(width = 0.75), width = 0.15) +
  xlab("")







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
  tm_layout(title = paste0('Percent green GVA\nAcross ITL2 regions'), legend.outside = T)



#What gives humber that higher %? Per sector % of green as stacked bar would be useful
#Which should just involve straight row multiplying up the GVA numbers...
sectors.greenjobs.gva <- itl2 %>% 
  mutate(
    green_gva_estimate = lcree_jobcount * GVAperFT,
    green_gva_lowerCI = lcree_lowerCI * GVAperFT,
    green_gva_upperCI = lcree_upperCI * GVAperFT
  ) %>% 
  group_by(DATE,GEOGRAPHY_NAME) %>% 
  mutate(
    totalGVAperITL2 = sum(GVA) * 1000000,
    across(green_gva_estimate:green_gva_upperCI, ~ (. / totalGVAperITL2) * 100, 
                .names = "percent_of_totalGVA_{.col}")
    ) %>% 
  ungroup()


#That looks sensible but just check regional green GVA numbers look sane
#Tick
# sectors.greenjobs.gva %>% 
#   group_by(DATE,GEOGRAPHY_NAME) %>% 
#   summarise(sumgreengvapercent = sum(percent_of_totalGVA_green_gva_estimate,na.rm = T))

#Remove two mostly non existent sectors
sectors4plot <- sectors.greenjobs.gva %>% filter(DATE == 2022, !grepl(x = SIC_SECTION_NAME_LCREE, pattern = 'mining|agri', ignore.case = T)) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  mutate(totalGREENgva_percent = sum(percent_of_totalGVA_green_gva_estimate)) %>% 
  ungroup()

ggplot(sectors4plot %>% mutate(GEOGRAPHY_NAME = ifelse(GEOGRAPHY_NAME == 'South Yorkshire', '>>>>>>>>>>>>>>>>>>> SOUTH YORKSHIRE',GEOGRAPHY_NAME)), 
       aes(x = fct_reorder(GEOGRAPHY_NAME,totalGREENgva_percent), y = percent_of_totalGVA_green_gva_estimate, fill = fct_reorder(SIC_SECTION_NAME_LCREE,percent_of_totalGVA_green_gva_estimate))) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  coord_flip() +
  xlab('') +
  theme(axis.text.x = element_text()) +
  # geom_bar(
  #   data = sectors4plot %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'), 
  #   aes(colour = SY, x = fct_reorder(GEOGRAPHY_NAME,totalGREENgva_percent), y = percent_of_totalGVA_green_gva_estimate, fill = fct_reorder(SIC_SECTION_NAME_LCREE,percent_of_totalGVA_green_gva_estimate)),
  #   position="stack", stat="identity", size = 0.5) +
  scale_colour_manual(values = c('white','black')) 
  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK AGAIN ON SECTOR GVA PER FT JOB VOLATILITY----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#If same as before, volatile - but that was cruder sectors. Let's see.
#Pick single sector, compare across places

#CP GVA needs to be in proportion to whole economy, if we're comparing places
#i.e. GVA per FT job as a fraction of total GB GVA
#So inflation isn't included
#(Is then only relative change of course, but that'll do)
itl2 <- itl2 %>% 
  group_by(DATE) %>% 
  mutate(
    GVA_TOTAL_GB = sum(GVA) * 1000000,
    FT_TOTAL_GB = sum(jobcountFT,na.rm = T)
    ) %>% 
  ungroup()

#These will be tiny fractions - a more intuitive way to express them is 
#AS A FRACTION OF AVERAGE GB GVA PER JOB
#It'll still be a perfectly linear scale
itl2 <- itl2 %>% 
  mutate(
    GBaverageGVAperFT = GVA_TOTAL_GB / FT_TOTAL_GB,
    GVAperFT_asFractionOfAv = GVAperFT / GBaverageGVAperFT
    ) 

#Quick look... long tail...
ggplot(itl2, aes(x = GVAperFT_asFractionOfAv)) +
  geom_density()



#Break down by size of GVA per FT for facet viewing
#Use average of latest years to do that
itl2 <- itl2 %>% 
  group_by(GEOGRAPHY_NAME,SIC_SECTION) %>% 
  mutate(
    GVAperFT_movingav = rollapply(GVAperFT,3,mean,na.rm = TRUE,align='right',fill=NA),
    GVAASFRACTIONperFT_movingav = rollapply(GVAperFT_asFractionOfAv,3,mean,na.rm = TRUE,align='right',fill=NA)
  ) %>% ungroup()

#Groups from most recent moving av year
cutgroups <- itl2 %>% filter(DATE == max(DATE)) %>% 
  group_by(SIC_SECTION) %>% 
  mutate(GVAperFT_FRACTION_group = cut_number(GVAASFRACTIONperFT_movingav,4) %>% as.numeric)




#Get order of sectors with highest GVA per FT on average
#Hmm - that's weird and interesting for lots of different reasons. Not as big a gap as expected
#Is place much more important?
sectororderGVAperFT <- itl2 %>% filter(DATE == max(DATE)) %>% 
  group_by(SIC_SECTION) %>% 
  summarise(
    avGVAperFT_fraction = mean(GVAperFT_asFractionOfAv)
    # avGVAperFT_fraction = mean(GVAASFRACTIONperFT_movingav)
  ) %>% 
  arrange(-avGVAperFT_fraction)


#How has that changed over the years?
sectororderGVAperFT_OVERTIME <- itl2 %>% 
  group_by(DATE, SIC_SECTION) %>% 
  summarise(
    avGVAperFT_fraction = mean(GVAperFT_asFractionOfAv),
    # avGVAperFT_fraction = median(GVAperFT_asFractionOfAv),
    avGVAperFT_fraction_MIN = min(GVAperFT_asFractionOfAv),
    avGVAperFT_fraction_MAX = max(GVAperFT_asFractionOfAv)
    # avGVAperFT_fraction = mean(GVAASFRACTIONperFT_movingav)
  ) %>% 
  arrange(-avGVAperFT_fraction) %>% 
  ungroup()

#THIS IS VERY STRIKING: HOW LITTLE AVERAGES HAVE CHANGED OVER TIME FOR GB FOR GVA PER FT JOB IN THESE SECTIONS
#More digging to be done to get to the story here...
#NOTE: MINING REGULARLY LARGEST SECTION - leaving out because a specific case but need to check
ggplot(sectororderGVAperFT_OVERTIME %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|real|elec', ignore.case = T)),
       aes(x = DATE, y = avGVAperFT_fraction, colour = fct_reorder(SIC_SECTION,avGVAperFT_fraction))) +
  geom_point() +
  geom_line() +
  scale_colour_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  coord_cartesian(xlim = c(2015,2022)) 
  
  

#VERSION WITH MINMAX BARs
ggplot(sectororderGVAperFT_OVERTIME %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|real|elec', ignore.case = T)), 
       aes(x = DATE, y = avGVAperFT_fraction, colour = fct_reorder(SIC_SECTION,avGVAperFT_fraction))) +
  # geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = avGVAperFT_fraction_MIN, ymax = avGVAperFT_fraction_MAX), position = position_dodge(width = 0.5), width = 0.5) +
  scale_colour_brewer(palette = 'Paired', direction = 1, name = "Sector") +
  coord_cartesian(xlim = c(2015,2022))

#Different view:
ggplot(sectororderGVAperFT_OVERTIME %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|real|elec', ignore.case = T)), 
       aes(colour = factor(DATE), y = avGVAperFT_fraction, x = fct_reorder(SIC_SECTION,avGVAperFT_fraction, .desc = T))) +
  # geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = avGVAperFT_fraction_MIN, ymax = avGVAperFT_fraction_MAX), position = position_dodge(width = 0.5), width = 0.5) +
  scale_colour_brewer(palette = 'Paired', direction = 1, name = "Year") +
  coord_flip()




#BUT: the average can shift about. 
#Check using GVA per job as fraction of whole GB output
#Multiply by ten^5 to make more readable
itl2 <- itl2 %>% 
  mutate(
    GVAperFT_asPERCENTofGB_times_ten_to_seven  = (GVAperFT / GVA_TOTAL_GB) * 10^7
  ) %>% ungroup()

# View(itl2 %>% filter(DATE == 2022, grepl(x = SIC_SECTION, pattern = 'manuf', ignore.case = T)))

GVAperFT_as_percentofwhole_timesTentoSeven <- itl2 %>% 
  group_by(DATE, SIC_SECTION) %>% 
  summarise(
    GVAperFT_asPERCENTofGB_times_ten_to_seven_AVE = mean(GVAperFT_asPERCENTofGB_times_ten_to_seven),
    GVAperFT_asPERCENTofGB_times_ten_to_seven_MIN = min(GVAperFT_asPERCENTofGB_times_ten_to_seven),
    GVAperFT_asPERCENTofGB_times_ten_to_seven_MAX = max(GVAperFT_asPERCENTofGB_times_ten_to_seven)
    # avGVAperFT_fraction = mean(GVAASFRACTIONperFT_movingav)
  ) %>% 
  ungroup()

ggplot(GVAperFT_as_percentofwhole_timesTentoSeven %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|real|elec', ignore.case = T)),
       aes(x = DATE, y = GVAperFT_asPERCENTofGB_times_ten_to_seven_AVE, colour = fct_reorder(SIC_SECTION,GVAperFT_asPERCENTofGB_times_ten_to_seven_AVE))) +
  geom_point() +
  geom_line() +
  scale_colour_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  coord_cartesian(xlim = c(2015,2022)) 

#minmax version
ggplot(GVAperFT_as_percentofwhole_timesTentoSeven %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|real|elec', ignore.case = T)),
       aes(x = DATE, y = GVAperFT_asPERCENTofGB_times_ten_to_seven_AVE, colour = SIC_SECTION)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = GVAperFT_asPERCENTofGB_times_ten_to_seven_MIN, ymax = GVAperFT_asPERCENTofGB_times_ten_to_seven_MAX), position = position_dodge(width = 0.5), width = 0.5) +
  scale_colour_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  coord_cartesian(xlim = c(2015,2022)) 





#Add cut numbers back into whole itl2 df so it can be split using that
itl2 <- itl2 %>%
  left_join(
    cutgroups %>% select(SIC_SECTION,GEOGRAPHY_NAME,GVAperFT_FRACTION_group),
    by = c('SIC_SECTION','GEOGRAPHY_NAME')
    )


#Should now be able to facet...
p <- ggplot(
  itl2 %>% filter(grepl('construction',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  # itl2 %>% filter(grepl('manuf',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  # aes(x = DATE, y = GVAASFRACTIONperFT_movingav, colour = GEOGRAPHY_NAME, size = SY)
  aes(x = DATE, y = GVAperFT_asFractionOfAv, colour = GEOGRAPHY_NAME, size = SY)
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(~GVAperFT_FRACTION_group, scales = 'free_y') +
  coord_cartesian(xlim = c(2017,2022)) +
  scale_size_manual(values = c(0.5,1.5))

ggplotly(p, tooltip = 'GEOGRAPHY_NAME')


#Or... looking at all years since denom is same
#https://stackoverflow.com/a/60673533 for sina geom
p <- ggplot(
  # itl2 %>% filter(grepl('construction',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  itl2 %>% filter(grepl('comm',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  # itl2 %>% filter(grepl('manuf',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  aes(x = DATE, y = GVAASFRACTIONperFT_movingav, group = GEOGRAPHY_NAME, size = SY)) +
  # ggforce::geom_sina()
  coord_cartesian(xlim = c(2017,2022)) +
  geom_jitter(width = 0.1)
  
ggplotly(p, tooltip = 'GEOGRAPHY_NAME')



#What have SY's SIC sections done over time, internally?
ggplot(
  itl2 %>% filter(GEOGRAPHY_NAME == 'South Yorkshire', !grepl(x = SIC_SECTION, pattern = 'real|other', ignore.case = T)),
  aes(x = DATE, y = GVAASFRACTIONperFT_movingav, colour = SIC_SECTION_NAME_LCREE)) +
  geom_point() +
  geom_line() +
  scale_colour_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  coord_cartesian(xlim = c(2017,2022)) 

  
#We prob want to see jobs vs GVA change don't we?
itl2 <- itl2 %>% 
  group_by(GEOGRAPHY_NAME,SIC_SECTION_NAME_LCREE) %>% 
  mutate(
    gva_movingav = rollapply(GVA,3,mean,na.rm = TRUE,align='right',fill=NA),
    jobcount_movingav = rollapply(jobcountFT,3,mean,na.rm = TRUE,align='right',fill=NA)
  ) %>% 
  ungroup()

# debugonce(twod_generictimeplot_multipletimepoints)
p <- twod_generictimeplot_multipletimepoints(
  df = itl2 %>% filter(grepl(x= GEOGRAPHY_NAME, pattern = 'south york', ignore.case = T), !grepl(x = SIC_SECTION, pattern = 'real|other', ignore.case = T)),
  category_var = SIC_SECTION,
  x_var = gva_movingav,
  y_var = jobcount_movingav,
  timevar = DATE,
  label_var = `GVAASFRACTIONperFT_movingav`,
  times = c(2017:2022)
)

p + theme(aspect.ratio=1) +
  xlab("GVA (3 year moving average)") +
  ylab("Job count (3 year moving average)")


# View(itl2 %>% filter(DATE==2022,grepl(x= GEOGRAPHY_NAME, pattern = 'south york', ignore.case = T), !grepl(x = SIC_SECTION, pattern = 'real', ignore.case = T)))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SOME FIRST GUESTTIMATES AT GVA MOVEMENTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






























