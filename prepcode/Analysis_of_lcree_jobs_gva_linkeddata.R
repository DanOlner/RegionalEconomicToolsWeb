#LCREE LINKED DATA
library(tidyverse)
library(zoo)
library(sf)
library(tmap)
library(plotly)
source('functions/misc_functions.R')
options(scipen = 99)

#Processed in LCREE_linkToBres_and_GVA_currentprices.R
# itl2 <- readRDS('data/LCREE_BRES_GVAcurrentprices_combo_2015to2022.rds') %>% rename(jobcountFT = COUNT) %>% ungroup()
#MINUS IMPUTED RENT, BETTER FOR ACCURATE GVA PER JOB
itl2 <- readRDS('data/LCREE_BRES_GVAcurrentprices_combo_2015to2022_MINUS_IMPUTED_RENT.rds') %>% rename(jobcountFT = COUNT) %>% ungroup()

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
  mutate(
    totalGREENgva_percent = sum(percent_of_totalGVA_green_gva_estimate)
    ) %>% 
  ungroup()


#data for overlaying range - summarise rather than mutate
sectors4plot.RANGE <- sectors.greenjobs.gva %>% filter(DATE == 2022, !grepl(x = SIC_SECTION_NAME_LCREE, pattern = 'mining|agri', ignore.case = T)) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  summarise(
    totalGREENgva_percent = sum(percent_of_totalGVA_green_gva_estimate),
    totalGREENgva_percent_lowerCI = sum(percent_of_totalGVA_green_gva_lowerCI, na.rm = T),
    totalGREENgva_percent_upperCI = sum(percent_of_totalGVA_green_gva_upperCI, na.rm = T)
  ) %>% 
  ungroup()


# ggplot(sectors4plot %>% mutate(GEOGRAPHY_NAME = ifelse(GEOGRAPHY_NAME == 'South Yorkshire', '>>>>>>>>>>>>>>>>>>> SOUTH YORKSHIRE',GEOGRAPHY_NAME)), 
#        aes(x = fct_reorder(GEOGRAPHY_NAME,totalGREENgva_percent), y = percent_of_totalGVA_green_gva_estimate, fill = fct_reorder(SIC_SECTION_NAME_LCREE,percent_of_totalGVA_green_gva_estimate))) +
#   geom_bar(position="stack", stat="identity") +
#   scale_fill_brewer(palette = 'Paired', direction = -1, name = "Sector") +
#   coord_flip() +
#   xlab('') +
#   theme(axis.text.x = element_text()) +
#   geom_errorbar(data = sectors4plot.RANGE, aes(x = fct_reorder(GEOGRAPHY_NAME,totalGREENgva_percent),ymin =  totalGREENgva_percent_lowerCI, ymax = totalGREENgva_percent_upperCI  ))
  # geom_bar(
  #   data = sectors4plot %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'), 
  #   aes(colour = SY, x = fct_reorder(GEOGRAPHY_NAME,totalGREENgva_percent), y = percent_of_totalGVA_green_gva_estimate, fill = fct_reorder(SIC_SECTION_NAME_LCREE,percent_of_totalGVA_green_gva_estimate)),
  #   position="stack", stat="identity", size = 0.5) +
  # scale_colour_manual(values = c('white','black')) 
  

ggplot() +
  geom_bar(data = sectors4plot %>% mutate(GEOGRAPHY_NAME = ifelse(GEOGRAPHY_NAME == 'South Yorkshire', '>>>>>>>>>>>>>>>>>>> SOUTH YORKSHIRE',GEOGRAPHY_NAME)), 
           aes(x = fct_reorder(GEOGRAPHY_NAME,totalGREENgva_percent), 
               y = percent_of_totalGVA_green_gva_estimate, 
               fill = fct_reorder(SIC_SECTION_NAME_LCREE,percent_of_totalGVA_green_gva_estimate)),
           position="stack", stat="identity") +
  scale_fill_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  coord_flip() +
  xlab('') +
  theme(axis.text.x = element_text()) +
  geom_errorbar(data = sectors4plot.RANGE %>% mutate(GEOGRAPHY_NAME = ifelse(GEOGRAPHY_NAME == 'South Yorkshire', '>>>>>>>>>>>>>>>>>>> SOUTH YORKSHIRE',GEOGRAPHY_NAME)), 
                aes(x = fct_reorder(GEOGRAPHY_NAME,totalGREENgva_percent),ymin =  totalGREENgva_percent_lowerCI, ymax = totalGREENgva_percent_upperCI  ),
                alpha = 0.5,width = 0.25
                ) +
  geom_hline(yintercept = sectors4plot.RANGE$totalGREENgva_percent_lowerCI[sectors4plot.RANGE$GEOGRAPHY_NAME=='South Yorkshire'], size = 2, alpha = 0.2) +
  geom_hline(yintercept = sectors4plot.RANGE$totalGREENgva_percent_upperCI[sectors4plot.RANGE$GEOGRAPHY_NAME=='South Yorkshire'], size = 2, alpha = 0.2) 






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
  group_by(GEOGRAPHY_NAME,SIC_SECTION) %>% #this assumes dates in order
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
# ggplot(sectororderGVAperFT_OVERTIME %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri', ignore.case = T)),
# ggplot(sectororderGVAperFT_OVERTIME %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|elec', ignore.case = T)),
ggplot(sectororderGVAperFT_OVERTIME %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|real|elec', ignore.case = T)),
       aes(x = DATE, y = avGVAperFT_fraction, colour = fct_reorder(SIC_SECTION,avGVAperFT_fraction))) +
  geom_point() +
  geom_line() +
  scale_colour_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  coord_cartesian(xlim = c(2015,2022)) 
  
  

#VERSION WITH MINMAX BARs
ggplot(sectororderGVAperFT_OVERTIME %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri', ignore.case = T)), 
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

ggplot(GVAperFT_as_percentofwhole_timesTentoSeven %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|elec|real', ignore.case = T)),
# ggplot(GVAperFT_as_percentofwhole_timesTentoSeven %>% filter(!grepl(x = SIC_SECTION, pattern = 'mining|agri|elec', ignore.case = T)),
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
  # itl2 %>% filter(grepl('profes',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  # itl2 %>% filter(grepl('construction',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  itl2 %>% filter(grepl('manuf',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
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
  # itl2 %>% filter(grepl('prof',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  aes(x = DATE, y = GVAASFRACTIONperFT_movingav, group = GEOGRAPHY_NAME, size = SY)) +
  # ggforce::geom_sina()
  coord_cartesian(xlim = c(2017,2022)) +
  geom_jitter(width = 0.1)
  
ggplotly(p, tooltip = 'GEOGRAPHY_NAME')

#FACET ALL
p <- ggplot(
  itl2 %>% 
    filter(!grepl('steam|estate|waste|mining|agri',SIC_SECTION,ignore.case=T)) %>% 
    mutate(
      SY = GEOGRAPHY_NAME == 'South Yorkshire',
      SIC_SECTION = fct_reorder(SIC_SECTION, GVAASFRACTIONperFT_movingav, .desc = T)
    ),
  # itl2 %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire', SIC_SECTION = fct_reorder(SIC_SECTION, GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav, .desc = T)),
  aes(x = DATE, y = GVAASFRACTIONperFT_movingav, group = GEOGRAPHY_NAME, size = SY, colour = SY)) +
  # ggforce::geom_sina()
  coord_cartesian(xlim = c(2017,2022)) +
  geom_jitter(width = 0.1) +
  scale_size_manual(values = c(1,5)) +
  scale_colour_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  # facet_wrap(~SIC_SECTION, scales = 'free_y') +
  facet_wrap(~SIC_SECTION, nrow = 1, labeller = labeller(groupwrap = label_wrap_gen(10))) +
  guides(colour = F, size = F)

p
ggplotly(p, tooltip = 'GEOGRAPHY_NAME')





#Get moving av of GVA as percent of total GB...
itl2 <- itl2 %>% 
  group_by(GEOGRAPHY_NAME,SIC_SECTION) %>% #this assumes dates in order
  mutate(
    GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav = rollapply(GVAperFT_asPERCENTofGB_times_ten_to_seven,3,mean,na.rm = TRUE,align='right',fill=NA)
  ) %>% ungroup()


p <- ggplot(
  itl2 %>% filter(grepl('construction',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  # itl2 %>% filter(grepl('comm',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  # itl2 %>% filter(grepl('manuf',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  # itl2 %>% filter(grepl('prof',SIC_SECTION,ignore.case=T)) %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire'),
  aes(x = DATE, y = GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav, group = GEOGRAPHY_NAME, size = SY)) +
  # ggforce::geom_sina()
  coord_cartesian(xlim = c(2017,2022)) +
  geom_jitter(width = 0.1)
  
ggplotly(p, tooltip = 'GEOGRAPHY_NAME')


#FACET ALL
p <- ggplot(
  itl2 %>% 
    filter(!grepl('steam|estate|waste|mining|agri',SIC_SECTION,ignore.case=T)) %>% 
    mutate(
    SY = GEOGRAPHY_NAME == 'South Yorkshire',
    # SY = grepl('Cambridge',GEOGRAPHY_NAME,ignore.case = T),
    SIC_SECTION = fct_reorder(SIC_SECTION, GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav, .desc = T)
    ),
  # itl2 %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire', SIC_SECTION = fct_reorder(SIC_SECTION, GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav, .desc = T)),
  aes(x = DATE, y = GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav, group = GEOGRAPHY_NAME, size = SY, colour = SY)) +
  # ggforce::geom_sina()
  coord_cartesian(xlim = c(2017,2022)) +
  geom_jitter(width = 0.1) +
  scale_size_manual(values = c(1,5)) +
  scale_colour_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  # facet_wrap(~SIC_SECTION, scales = 'free_y') +
  facet_wrap(~SIC_SECTION, nrow = 1, labeller = labeller(groupwrap = label_wrap_gen(10))) +
  guides(colour = F, size = F)

p
ggplotly(p, tooltip = 'GEOGRAPHY_NAME')






#What have SY's SIC sections done over time, internally?
ggplot(
  itl2 %>% filter(GEOGRAPHY_NAME == 'South Yorkshire', !grepl(x = SIC_SECTION, pattern = 'agri|other', ignore.case = T)),
  aes(x = DATE, y = GVAASFRACTIONperFT_movingav, colour = fct_reorder(SIC_SECTION_NAME_LCREE,GVAASFRACTIONperFT_movingav) )) +
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

# View(itl2 %>% filter(DATE == 2017))

# debugonce(twod_generictimeplot_multipletimepoints)
p <- twod_generictimeplot_multipletimepoints(
  df = itl2 %>% filter(grepl(x= GEOGRAPHY_NAME, pattern = 'south york', ignore.case = T), !grepl(x = SIC_SECTION, pattern = 'other', ignore.case = T)),
  category_var = SIC_SECTION,
  x_var = GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav,
  # x_var = gva_movingav,
  y_var = jobcount_movingav,
  timevar = DATE,
  label_var = `GVA`,
  times = c(2017:2022)
  # times = c(2017:2019)
)

p + theme(aspect.ratio=1) +
  # xlab("GVA (3 year moving average)") +
  xlab("GVA per FT as percent of GB whole econ (3 year moving average)") +
  ylab("Job count (3 year moving average)")


#Can we compare same sector in different places? Probably too messy, but could pull some out...
# debugonce(twod_generictimeplot_multipletimepoints)
p <- twod_generictimeplot_multipletimepoints(
  df = itl2 %>% filter(grepl(x= GEOGRAPHY_NAME, pattern = 'south york|west york|manc|mersey|west mid', ignore.case = T),
  # df = itl2 %>% filter(
                       grepl(x = SIC_SECTION, pattern = 'scient', ignore.case = T)),
                       # grepl(x = SIC_SECTION, pattern = 'construction', ignore.case = T)),
                       # grepl(x = SIC_SECTION, pattern = 'information', ignore.case = T)),
                       # grepl(x = SIC_SECTION, pattern = 'manuf', ignore.case = T)),
  category_var = GEOGRAPHY_NAME,
  # x_var = GVAASFRACTIONperFT_movingav,
  x_var = GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav,
  # x_var = gva_movingav,
  y_var = jobcount_movingav,
  timevar = DATE,
  label_var = `GVA`,
  # compasspoints_to_display = c('NW','SW'),
  # compasspoints_to_display = c('NE','SE'),
  times = c(2017:2022)
  # times = c(2017:2019)
)

p + theme(aspect.ratio=1) +
  xlab("GVA per FT as percent of GB whole econ (3 year moving average)") +
  ylab("Job count (3 year moving average)")



# View(itl2 %>% filter(DATE==2022,grepl(x= GEOGRAPHY_NAME, pattern = 'south york', ignore.case = T), !grepl(x = SIC_SECTION, pattern = 'real', ignore.case = T)))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SOME FIRST GUESTTIMATES AT GVA MOVEMENTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Example for testing single case...
#Take example sector, manufacturing
#South Yorks has had consistently near lowest per worker manuf productivity

#But let's imagine there's a single average productivity addition to the manuf workforce...
#We'll be wanting an informed guess at a distribution around this mean
job.mean <- itl2 %>%
  filter(
    grepl('south york', GEOGRAPHY_NAME, ignore.case = T),
    grepl(pattern = 'manuf', SIC_SECTION, ignore.case = T),
    DATE == 2022
    ) %>% 
  pull(GVAperFT_movingav)


#Test getting spread of GVA per FT from other sectors based on existing job count
#Removing some sectors that would be unlikely to move to manuf
sample_GVAperFT <- itl2 %>%
  filter(
    grepl('south york', GEOGRAPHY_NAME, ignore.case = T),
    !grepl('real|elec|mining|other', SIC_SECTION, ignore.case = T),
    DATE == 2022
  ) 

# weighted.sample.GVAperFT <- sample(
#   sample_GVAperFT$GVAperFT, size = 100000, replace = T, prob = sample_GVAperFT$jobcountFT
# )
# 
# #Not very many values to sample from!
# plot(density(weighted.sample.GVAperFT))
# hist(weighted.sample.GVAperFT)


#And try another approach
#Eyeballed until amount overlaps values
jittered.GVAperFT.repeats <- replicate(100, jitter(sample_GVAperFT$GVAperFT, amount = 9000)) %>% as.vector() 
plot(jittered.GVAperFT.repeats)

#Some neg
jittered.jobcountFT.repeats <- replicate(100, jitter(sample_GVAperFT$jobcountFT, amount = 7000)) %>% as.vector() %>% abs()
plot(jittered.jobcountFT.repeats)

plot(sample_GVAperFT$GVAperFT, sample_GVAperFT$jobcountFT)
#That linear relationship again - GVA per job is problematic isn't it? This is happening I think due to sector size and jobs needed in each sector not being strongly linked?
#But then I thought it was...
plot(jittered.GVAperFT.repeats,jittered.jobcountFT.repeats)

#Yep, that's about the right relationship between them. Can now get smoother resampling estimate
weighted.sample.GVAperFT <- sample(
  jittered.GVAperFT.repeats, size = 100000, replace = T, prob = jittered.jobcountFT.repeats
)

#Better... long right tail, is right
# plot(density(weighted.sample.GVAperFT))
hist(weighted.sample.GVAperFT)
abline(v = job.mean, lwd = 6)


#Could then get a guesstimate about what manuf wage spread might be - attracting a regional worker, wage would need to be higher (most likely)
#It would seem reasonable to use two assumptions:
#The new job takes its possible spread from the spread of GVA per jobs across all local sectors
#(i.e. some could higher than manuf mean, some lower)
#And also constrain so isn't beyond other sectors
#I think also sensible to still remove outlier sectors that have specific dynamics
#E.g. elec / gas massive profit margins warps GVA per FT out of all proportion, won't work in other sectors

#So: 
#Dividing SD by 2 I think makes it more reasonable - sector has own spread of value that's within overall regional value per job
job.spread <- rnorm(10000, mean = job.mean, sd = sd(weighted.sample.GVAperFT)/2)

hist(job.spread)
abline(v = job.mean, lwd = 6)

#Both?
ggplot(
  data.frame(job.spread = job.spread, weighted.sample.GVAperFT = weighted.sample.GVAperFT) %>%
    pivot_longer(job.spread:weighted.sample.GVAperFT),
  aes(x = value, colour = name, fill = name)
) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = job.mean) +
  geom_vline(xintercept = mean(weighted.sample.GVAperFT), colour = 'green') 


#Single sample from manuf spread
single.job <- sample(job.spread,1)

#That is the gross GVA addition to South Yorkshire
#But then there are several things to consider
#Including mulling how these differently affect raw GVA change, GVA per capita and GVA per FT job

#Assuming GVA per FT reflects job value to employee
#Assume the job will attract candidates from lower GVA positions
#But how much lower?
#If we pick from anywhere in the lower distribution, this can bundle any knockon effects of chain of shifting work

#So e.g.
displaced.job <- sample(weighted.sample.GVAperFT[weighted.sample.GVAperFT < single.job],1)

#net GVA difference?
single.job - displaced.job

#Issue there: assuming that GVA is directly lost from other firm or indeed gained here
#Whole issue of "total GVA over total jobs" as the correct measure...
#But still let's see the spread

net.newjobvalue <- function(job.spread,jobmarket.spread){
  single.job <- sample(job.spread,1)
  displaced.job <- sample(jobmarket.spread[jobmarket.spread < single.job],1)
  #net GVA difference?
  single.job - displaced.job
}

net.newjobspread <- replicate(1000, net.newjobvalue(job.spread, weighted.sample.GVAperFT))

hist(net.newjobspread)

#So we're saying a thousand new manuf jobs would, if jobs all poached locally, net GVA of:
#About £14M or net of average of 14K per new job a year (which I'm guessing stays quite stable?)
#There's some spread, maybe a million each side, but yes.
sum(net.newjobspread)

#So that's likely the LOWEST extreme with a 1:1 net poaching ratio?



#Then we want some assumptions on whether a new job triggers / is filled by migration or commuting
#(Those two v different headline output diffs)
#From SY viewpoint, whether migration from surrounding areas or elsewhere doesn't matter
#Note: remote working too - the GVA will still 'commute' into where the job is


#In-commutes: around 20% of overall workforce, maybe
#See http://fryford.github.io/dvc193/indexv3.html#sty=true&flow=flow0&period=0&fix=E08000018&view=436.25,198.125,157.5,158.75&tr=-8.84210205078125,-29.01312255859375&sc=1

#So if new job created... higher value ones likelier to be commute jobs?
#What does this do to net job value?
#If outside of SY, it's effectively AN ADDITIONAL JOB to the region entirely.
#(Poached from elsewhere)

#Which means in terms of raw GVA increase, by current measures
#A commute / distance remote working job is probably better

#Example:
#750 new jobs filled from SY
#£10M or ... again, it's always 14K per job net from this route!
net.newjobspread <- replicate(750, net.newjobvalue(job.spread, weighted.sample.GVAperFT))
sum(net.newjobspread)

#250 in-commute jobs of same value:
incommutes <- sample(job.spread,250)
sum(incommutes)#16,425,375
sum(incommutes)/250#65K GVA per job coming in, no other work displacement regionally means this is four times higher

#BUT THAT JOB ISN'T BENEFITING AN EMPLOYEE IN SY!!
#Spillover effects of employee spending go elsewhere (number for that?)


#Next assumption:
#New job is resident in SY but new
#Either due to new recruit moving from inactive
#Or migrating in
#Two things there:
#If "moving from inactive" but was previously there, effect on av GVA per FT will depend on if job is > av GVA per FT
#It'll increase GVA per capita tho
#And won't be net of any other job

#i.e. adds one person to resident pop
#Note: breaking down by resident vs workplace pop will be useful, if time






