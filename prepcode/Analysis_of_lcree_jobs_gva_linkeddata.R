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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TOTAL GREEN JOBS PER YEAR----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#totgreenjobs <- itl2 %>% 

#Let's just get from the source:
lcree <- read_csv('data/lcree2022_adjustedToMatchGBjobnumbers.csv')

#Sum per year
yearlytot <- lcree %>% 
  group_by(year) %>% 
  summarise(
    total = sum(estimate, na.rm = T),
    total_lowerCI = sum(`lower CI`, na.rm = T),
    total_upperCI = sum(`upper CI`, na.rm = T)
    )

#So that's about a third of the CBI count, which makes sense as that includes spillovers
ggplot(yearlytot, aes(x = year, y = total)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = total_lowerCI, ymax = total_upperCI)) +
  scale_color_brewer(palette = 'Paired', direction = -1)


#What number / proportion in SY of those? (For latest year, according to method used here)
#SY 5125, which is 1.9% of GB total
lcree.regional <- itl2 %>% 
  filter(DATE == 2022) %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  summarise(lcree_jobcount = sum(lcree_jobcount, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(jobcount_percent = (lcree_jobcount/sum(lcree_jobcount))*100 ) 


#Taking Labour manifesto number:
#"At the heart of our approach will be our Green Prosperity Plan where, in partnership with business through our National Wealth Fund, we will invest in the industries of the future. 
#Our plan will create 650,000 jobs across the country by 2030."
#So that's about a doubling of green jobs if going by CBI report (Green sector “supported 765,700 Full Time Equivalent (FTE) jobs, equal to nearly 3% of total UK employment”)
#How many in LCREE…? ~270K though (a) error bars are wide and (b) that’s direct, not the 1:2 spillover ratios the CBI mentions.

#Adjust down to GB with same basic assumption as for LCREE:
labouraim <- 650000 * .99

#SY's proportion if same... 12234
labouraim * (lcree.regional %>% filter(GEOGRAPHY_NAME=='South Yorkshire') %>% select(jobcount_percent) %>% pull() / 100)

#If that's direct, spillover maybe triples that


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GREEN JOBS AS A PROPORTION OF TOTAL PER YEAR AND PER SECTION----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

greenjobs.percent <- itl2 %>% 
  group_by(DATE,SIC_SECTION_NAME_LCREE) %>% 
  summarise(
    jobcountFT = sum(jobcountFT, na.rm = T),
    lcree_jobcount = sum(lcree_jobcount, na.rm = T),
    lcree_lowerCI = sum(lcree_lowerCI, na.rm = T),
    lcree_upperCI = sum(lcree_upperCI, na.rm = T),
    lcree_jobcount_percent = (lcree_jobcount / jobcountFT) * 100,
    lcree_jobcount_percent_lowerCI = (lcree_lowerCI / jobcountFT) * 100,
    lcree_jobcount_percent_upperCI = (lcree_upperCI / jobcountFT) * 100
  ) %>% ungroup()


#Get split of sectors into two groups by size in latest year, for plotting
sectorsplit <- greenjobs.percent %>% 
  filter(DATE == max(DATE)) %>% select(lcree_jobcount, SIC_SECTION_NAME_LCREE) %>% 
  mutate(
    sectorsplit = ifelse(
      lcree_jobcount > median(lcree_jobcount), 0, 1
      )
  )

greenjobs.percent <- greenjobs.percent %>% 
  left_join(sectorsplit, by = 'SIC_SECTION_NAME_LCREE')


##PLOT: GREEN JOBS AS A PERCENT OF TOTAL JOBS IN SIC SECTION----
ggplot(greenjobs.percent %>% filter(!grepl(x = SIC_SECTION_NAME_LCREE, pattern = 'agri|mining|other', ignore.case = T)), 
       aes(x = DATE, y = lcree_jobcount_percent, colour = fct_reorder(SIC_SECTION_NAME_LCREE,lcree_jobcount_percent) )) +
  geom_line(position = position_dodge(width = 0.75)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin = lcree_jobcount_percent_lowerCI, ymax = lcree_jobcount_percent_upperCI), position = position_dodge(width = 0.75)) +
  scale_color_brewer(palette = 'Paired', direction = -1) +
  facet_wrap(~sectorsplit, scales = 'free_y')

  



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



#PLOT: PERCENT OF TOTAL GVA THAT IS GREEN JOBS----
ggplot(GB.greenjobs.gva, 
       aes(x = DATE, y = percent_of_totalGVA_green_gva_estimate)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = percent_of_totalGVA_green_gva_lowerCI, ymax = percent_of_totalGVA_green_gva_upperCI), position = position_dodge(width = 0.75), width = 0.15) +
  xlab("") +
  ylab("Green GVA as percent of GB total GVA") +
  ggtitle("Green GVA as percent of GB total GVA") +
  coord_cartesian(ylim = c(0,1.7))







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
  

#PLOT: ITL2 REGION % GREEN GVA BY SECTOR----
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
  geom_hline(yintercept = sectors4plot.RANGE$totalGREENgva_percent_upperCI[sectors4plot.RANGE$GEOGRAPHY_NAME=='South Yorkshire'], size = 2, alpha = 0.2) +
  ggtitle("If/then estimate of ITL2 region % green (LCREE) GVA by sector\nSouth Yorkshire in caps/arrows\nGrey lines are South Yorkshire 95% CIs")






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
# ggplot(sectororderGVAperFT_OVERTIME,
       aes(x = DATE, y = avGVAperFT_fraction, colour = fct_reorder(SIC_SECTION,avGVAperFT_fraction))) +
  geom_point() +
  geom_line() +
  scale_colour_brewer(palette = 'Paired', direction = -1, name = "Sector") +
  coord_cartesian(xlim = c(2015,2022)) +
  ggtitle('Average GVA per FT job for SIC sections\nExpressed as ratio to GB average GVA per FT overall\nMinus outlier sectors (real estate, power)')
  


  

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
      # SY = GEOGRAPHY_NAME == 'South Yorkshire',
      SY = GEOGRAPHY_NAME == 'West Yorkshire',
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
#Add a line for GVA per FT for SY
#A job above that line increases the average, below decreases it
#WEIGHTED MEAN to get value of each job

#NOTE TIHS IS ADDING 2022 DATA OVER A TIME SERIES
#Possibly tweak or use different plot to make point
sy_avGVAperFT <- itl2 %>% 
  filter(GEOGRAPHY_NAME=='South Yorkshire',DATE==max(DATE)) %>% 
  summarise(
    avGVAperFT = weighted.mean(GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav,jobcountFT)
  ) %>% pull()


#reduce names
unique(itl2$SIC_SECTION_NAME_LCREE)

itl2 <- itl2 %>% 
  mutate(
    SIC_SECTION_REDUCED = case_when(
      grepl('admin',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Admin',
      grepl('agri',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Agri',
      grepl('electr',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'power',
      grepl('information',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'ICT',
      grepl('manuf',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Manuf',
      grepl('mining',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Mining',
      grepl('other',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'other',
      grepl('scientific',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Scientific',
      grepl('real estate',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Real est',
      grepl('transport',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Transport',
      grepl('water',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Water',
      grepl('wholesale',SIC_SECTION_NAME_LCREE,ignore.case = T) ~ 'Retail',
      .default = SIC_SECTION_NAME_LCREE
    )
  )

#PLOT: GVA PER FT JOB FOR SIC SECTIONS OVER TIME, SY OVERLAID
p <- ggplot(
  itl2 %>% 
    # filter(!grepl('steam|estate|waste|mining|agri',SIC_SECTION,ignore.case=T)) %>%
    mutate(
    # SY = GEOGRAPHY_NAME == 'West Yorkshire',
    SY = GEOGRAPHY_NAME == 'South Yorkshire',
    # SY = grepl('Cambridge',GEOGRAPHY_NAME,ignore.case = T),
    SIC_SECTION_REDUCED = fct_reorder(SIC_SECTION_REDUCED, GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav, .desc = T)
    ),
  # itl2 %>% mutate(SY = GEOGRAPHY_NAME == 'South Yorkshire', SIC_SECTION = fct_reorder(SIC_SECTION, GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav, .desc = T)),
  aes(x = DATE, y = GVAperFT_asPERCENTofGB_times_ten_to_seven_movingav, group = GEOGRAPHY_NAME, size = SY, colour = SY)) +
  # ggforce::geom_sina()
  coord_cartesian(xlim = c(2017,2022)) +
  geom_jitter(width = 0.1) +
  scale_size_manual(values = c(1,5)) +
  scale_colour_brewer(palette = 'Set1', direction = -1, name = "Sector") +
  # facet_wrap(~SIC_SECTION, scales = 'free_y') +
  facet_wrap(~SIC_SECTION_REDUCED, nrow = 1, labeller = labeller(groupwrap = label_wrap_gen(10))) +
  guides(colour = F, size = F) +
  ggtitle('GVA per FT job (as proportion of whole GB economy), smoothed 3 yr average. South Yorkshire is light/large spot.')
  

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
  df = itl2 %>% filter(
    # grepl(x= GEOGRAPHY_NAME, pattern = 'cheshire', ignore.case = T), 
    grepl(x= GEOGRAPHY_NAME, pattern = 'south york', ignore.case = T),
    !grepl(x = SIC_SECTION, pattern = 'other', ignore.case = T),
    jobcount_movingav > 8000
    ),
  category_var = SIC_SECTION_REDUCED,
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
  xlab("GVA per FT as percent of GB whole econ * 10^7 (3 year moving average)") +
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
mean(weighted.sample.GVAperFT)
quantile(weighted.sample.GVAperFT, probs = c(0.05,0.95))

abline(v = job.mean, lwd = 6)


#Could then get a guesstimate about what manuf wage spread might be - 
#attracting a regional worker, wage would need to be higher (most likely)
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
quantile(job.spread, probs = c(0.05,0.95))

hist(weighted.sample.GVAperFT)



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


#Function to repeat that to get a distribution from random new manufacturing jobs

#COMMENTING OUT TO USE THE VERSION IN MISCFUNCTIONS.R

# net.newjobvalue <- function(job.spread,jobmarket.spread){
#   single.job <- sample(job.spread,1)
#   displaced.job <- sample(jobmarket.spread[jobmarket.spread < single.job],1)
#   
#   #net GVA difference?
#   #Return both net and the full value of the jobs displaced
#   #So can estimate proportion addition
#   #(Which is probably just going to be close to the mean diff but let's do anyway, as can get spread)
#   return(
#     list(
#       new = single.job,
#       displaced = displaced.job,
#       net = single.job - displaced.job
#     )
#   )
# 
# }


#Repeat!
net.newjobspread <- purrr::map(1:1000, ~net.newjobvalue(job.spread, weighted.sample.GVAperFT)) %>% bind_rows

hist(net.newjobspread$net)
quantile(net.newjobspread$net, c(0.05,0.95))

#So we're saying a thousand new manuf jobs would, if jobs all poached locally, net GVA of:
#About £14M or net of average of 14K per new job a year (which I'm guessing stays quite stable?)
#There's some spread, maybe a million each side, but yes.
sum(net.newjobspread$net)
mean(net.newjobspread$net)

#So that's likely the LOWEST extreme with a 1:1 net poaching ratio?

#Compare to displaced jobs amount to get proportion gained
sum(net.newjobspread$displaced)

#So what is percent extra GVA per job gained? Here in the ballpark of 27%
(sum(net.newjobspread$net)/sum(net.newjobspread$displaced))*100 
(sum(net.newjobspread$net[1:250])/sum(net.newjobspread$displaced[1:250]))*100 
(sum(net.newjobspread$net[251:500])/sum(net.newjobspread$displaced[251:500]))*100 
(sum(net.newjobspread$net[501:1000])/sum(net.newjobspread$displaced[501:1000]))*100 


#Can I get a credible spread of that percent difference please?
net.newjobspread <- net.newjobspread %>% 
  mutate(percent_gain = (net / displaced) * 100)

#mean should be same as previous line... err nope!
mean(net.newjobspread$percent_gain)

hist(net.newjobspread$percent_gain)

#quick sidequest: why are those values different? Look at smaller sample and figure out
#Just five jobs

#Random point in it
rndpoint <- sample(nrow(net.newjobspread),1)
five <- net.newjobspread[rndpoint:(rndpoint+1),]

(sum(five$net)/sum(five$displaced))*100 
mean(five$percent_gain)


#Check random sample from it produces fairly similar results
chk <- net.newjobspread[sample(nrow(net.newjobspread),50),]

(sum(chk$net)/sum(chk$displaced))*100 
mean(chk$percent_gain)



#Yep, regardless of sample, mean of rowwise percent_gain is higher
#Let's stare at that
#I think it's probably due to the distribution, which is very left-heavy

#OK, let's just save this single random outcome to use for the basic maths
#Reduced to two
# saveRDS(five,'local/randtest.rds')
# five <- readRDS('local/randtest.rds')
# 
# #Displaced jobs:
# d1 = 62914.07
# d2 = 33472.19
# 
# #Net gain from the two new jobs:
# n1 = 5110.31
# n2 = 31392.60
# 
# new1 = 68024.38
# new2 = 64864.79
# 
# #So we're just after what % difference the new jobs are from the original displaced
# #Two different things being asked
# #If 1000 jobs with random differences in the spread of manuf vs SY jobs generally
# #What's the total diff in net vs the displaced jobs?
# #We can sum those I think because that's accounting for the entire spread of the values
# #And probably what we want
# 
# #Which is
# ((n1+n2) / (d1+d2)) * 100
# 
# #Maybe that's the wrong sum? Do we have to sum new and displaced and then find diff?
# newtot = new1 + new2
# displacedtot = d1 + d2
# 
# nettot = newtot - displacedtot
# n1 + n2
# 
# #Exactly the same. So no, makes no difference
# (nettot / displacedtot) * 100
# (sum(five$net)/sum(five$displaced))*100 


#Percent total net gain overall and average net gain per job are two different things
#Can we leave it at that?

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Oh. It's because the average is of a percentage, which removes the actual magnitude of the values
#The summed total is closer to what I want

#Checking that it stays fairly consistent when using the miscfunctions.R version
#Yep, very similar numbers, good
net.newjobspread <- purrr::map(1:10000, ~net.newjobvalue(job.spread, weighted.sample.GVAperFT)) %>% bind_rows

net.newjobspread <- net.newjobspread %>% 
  mutate(percent_gain = (net / displaced) * 100)

#Average net gain
mean(net.newjobspread$net)
quantile(net.newjobspread$net, c(0.05,0.95))

(sum(net.newjobspread$net)/sum(net.newjobspread$displaced))*100 

#mean(net.newjobspread$percent_gain)





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
net.newjobspread <- purrr::map(1:750, ~net.newjobvalue(job.spread, weighted.sample.GVAperFT)) %>% bind_rows()
sum(net.newjobspread$net)

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






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GENERALISING THAT CODE TO ANY SECTOR----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#For which we will need some other functions
#Just broad sector name for now should do, no?
FTsample.sy <- makeSampleOf_GVAperFTjob(
  itl2 %>%
    filter(
      grepl('south york', GEOGRAPHY_NAME, ignore.case = T),
      !grepl('real|elec|mining|other', SIC_SECTION, ignore.case = T),
      DATE == 2022
    ),
  samplesize = 100000
)

#Just comparing to other places - might need better method to get smooth curve generically
#But will do for SY
FTsample2 <- makeSampleOf_GVAperFTjob(
  itl2 %>%
    filter(
      grepl('manc', GEOGRAPHY_NAME, ignore.case = T),
      grepl('manc', GEOGRAPHY_NAME, ignore.case = T),
      # grepl('Inner London - West', GEOGRAPHY_NAME, ignore.case = T),
      !grepl('real|elec|mining|other', SIC_SECTION, ignore.case = T),
      DATE == 2022
    ),
  samplesize = 100000
)


both <- rbind(
  data.frame(sampleGVAperFT = FTsample.sy, place = 'SY'),
  data.frame(sampleGVAperFT = FTsample2, place = '2')
)

ggplot(both, aes(x = sampleGVAperFT, fill = place)) +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = mean(FTsample.sy), colour = "blue") +
  geom_vline(xintercept = mean(FTsample2), colour = "red")





job.mean <- itl2 %>%
  filter(
    grepl('south york', GEOGRAPHY_NAME, ignore.case = T),
    # grepl(pattern = 'construction', SIC_SECTION, ignore.case = T),
    # grepl(pattern = 'scient', SIC_SECTION, ignore.case = T),
    grepl(pattern = 'information', SIC_SECTION, ignore.case = T),
    DATE == 2022
  ) %>% 
  pull(GVAperFT_movingav)


job.spread <- rnorm(100000, mean = job.mean, sd = sd(FTsample.sy)/2)

hist(job.spread)
abline(v = job.mean, lwd = 6)

hist(FTsample.sy)

#Set so if new job is lower than any existing, we take value of that new job as one displaced
net.newjobspread <- purrr::map(1:10000, ~net.newjobvalue(job.spread, FTsample.sy)) %>% bind_rows

hist(net.newjobspread$net)

#average of new job GVA
mean(net.newjobspread$new)
#Old job
mean(net.newjobspread$displaced)


#So we're saying a thousand new manuf jobs would, if jobs all poached locally, net GVA of:
#About £14M or net of average of 14K per new job a year (which I'm guessing stays quite stable?)
#There's some spread, maybe a million each side, but yes.
sum(net.newjobspread$net)
mean(net.newjobspread$net)
quantile(net.newjobspread$net, 0.95)

#So that's likely the LOWEST extreme with a 1:1 net poaching ratio?

#Compare to displaced jobs amount to get proportion gained
sum(net.newjobspread$displaced)

#So what is percent extra GVA per job gained? 
(sum(net.newjobspread$net)/sum(net.newjobspread$displaced))*100 
#Same, good!
(mean(net.newjobspread$net)/mean(net.newjobspread$displaced))*100 

#So find diff if using 95th percentile
netnew95 <- net.newjobspread %>% filter(net > quantile(net, 0.95))#which is just top 500 values out of 10000 obv

#And percent diff then?
(mean(netnew95$net)/mean(netnew95$displaced))*100 



#OK, now a version of the above that pulls out those key values we need for every sector and stores in a DF for table making
#Let's make a function for it.

#We want to put in:
#1. The distribution of GVA per job for SY overall (with justification for keeping certain sectors out here)
FTsample.sy <- makeSampleOf_GVAperFTjob(
  itl2 %>%
    filter(
      grepl('south york', GEOGRAPHY_NAME, ignore.case = T),
      !grepl('real|elec|mining|other', SIC_SECTION, ignore.case = T),
      DATE == 2022
    ),
  samplesize = 100000
)

#2. Details for getting the job mean and a guess at its spread of values, inside the function so sector can be selected


#Test
newjobnumbers(itl2, FTsample.sy, sectorname = 'J Information and communication', placename = 'South Yorkshire')

#RUN ALL!
jobnumbers <- purrr::map(unique(itl2$SIC_SECTION_REDUCED), ~newjobnumbers(itl2, FTsample.sy, sectorname = ., placename = 'South Yorkshire')) %>% bind_rows

saveRDS(jobnumbers, 'local/GVAjobnumberestimates.rds')


#Version for outputting in table
jobnumbers.table <- jobnumbers %>% 
  mutate(
    `percent gained` = round(`percent gained`,2)
  ) %>% 
  mutate(
    across(`av new job GVA`:`av net GVA`, ~round(., 0))
  ) %>% 
  arrange(-`percent gained`) %>% 
  rename(`percent GVA gained` = `percent gained`)

  
saveRDS(jobnumbers.table, 'data/GVAjobnumberestimates.rds')

#reload!
jobnumbers.table <- readRDS('data/GVAjobnumberestimates.rds')

#What's difference between net and just new?
jobnumbers.table <- jobnumbers.table %>% 
  mutate(
    net_v_new_diff = (`av new job GVA` - `av net GVA`),
    net_v_new_diff_percent = (`av new job GVA`/`av net GVA`)*100
    )





#Repeat for 95th percentile
jobnumbers <- purrr::map(unique(itl2$SIC_SECTION_REDUCED), ~newjobnumbers(itl2, FTsample.sy, sectorname = ., placename = 'South Yorkshire', percentile95 = TRUE)) %>% bind_rows

#Version for outputting in table
jobnumbers.table <- jobnumbers %>% 
  mutate(
    `percent gained 95` = round(`percent gained 95`,2)
  ) %>% 
  mutate(
    across(`new job GVA 95`:`net GVA 95`, ~round(., 0))
  ) %>% 
  arrange(-`percent gained 95`) %>% 
  rename(`percent GVA gained 95` = `percent gained 95`)


saveRDS(jobnumbers.table, 'data/GVAjobnumberestimates_95thpercentile.rds')
