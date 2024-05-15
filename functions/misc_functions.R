#Misc functions
library(tidyverse)
library(ggrepel)

#Compute series of slopes within groups safely, returning 0 if can't calculate
compute_slope_or_zero <- function(data, ..., y, x) {
  
  groups <- quos(...) 
  y <- enquo(y)
  x <- enquo(x)

  #Function to compute slope
  get_slope <- function(data) {
    # model <- lm(data = data, formula = as.formula(paste0(!!y, " ~ ", !!x)))
    model <- lm(data = data, formula = as.formula(paste0(quo_name(y), " ~ ", quo_name(x))))
    coef(model)[2]
  }

  #Make it a safe function using purrr::possibly
  safe_get_slope <- possibly(get_slope, otherwise = 0)

  #Group and summarize
  data %>%
    group_by(!!!groups) %>%
    nest() %>%
    mutate(slope = map_dbl(data, safe_get_slope)) %>%
    select(-data)
  
}



#Create location quotients (and the regional and larger scale proportions needed to calculate it) and return attached to original dataframe
add_location_quotient_and_proportions <- function(df, regionvar, lq_var, valuevar){
  
  regionvar <- enquo(regionvar)
  lq_var <- enquo(lq_var)
  valuevar <- enquo(valuevar)
  
  df <- df %>%
    group_by(!!regionvar) %>% 
    mutate(
      region_totalsize = sum(!!valuevar, na.rm = T),#a. Current price per region per year, for regional denominator
      sector_regional_proportion = !!valuevar / region_totalsize#b. regional sector proportion (noting that a single row in this group is a single sector)
    ) %>% 
    group_by(!!lq_var) %>% 
    mutate(
      total_sectorsize = sum(!!valuevar, na.rm = T),#c. Summed current prices for EACH SECTOR, UK-wide
    ) %>% 
    ungroup() %>% 
    mutate(
      totalsize = sum(!!valuevar, na.rm = T),#d. Summed current prices for WHOLE UK per year, for UK denominator
      sector_total_proportion = total_sectorsize / totalsize#e. UK-level sector proportion
    ) %>% 
    mutate(
      LQ = sector_regional_proportion / sector_total_proportion#f. Location quotient!
    ) %>% 
    mutate(LQ_log = log(LQ)) 
  
  return(df)
  
}


#Get regional and full geography proportions
#But the full geography minus the region value so they're
#Both entirely separate calculations
#So total can be plotted on one axis and region on the other
#Without the total including part of the region amount
# get_sector_total_proportions <- function(df, lq_var, valuevar){
# 
#   lq_var <- enquo(lq_var) 
#   valuevar <- enquo(valuevar)
# 
#   df <- df %>%
#     group_by(!!lq_var) %>%
#     mutate(
#       total_sectorsize = sum(!!valuevar, na.rm = T),#c. Summed current prices for EACH SECTOR, UK-wide
#     ) %>%
#     ungroup() %>%
#     mutate(
#       totalsize = sum(!!valuevar, na.rm = T),#d. Summed current prices for WHOLE UK per year, for UK denominator
#       sector_total_proportion = total_sectorsize / totalsize#e. UK-level sector proportion, minus the excluded place
#     )
# 
#   return(df)
# 
# }
# 











#Make base plot for LQ plot, with option of setting alpha to zero if we don't want to see all other places
LQ_baseplot <- function(df, alpha = 0.1, sector_name, LQ_column, change_over_time){
  
  sector_name <- enquo(sector_name)
  LQ_column <- enquo(LQ_column)
  change_over_time <- enquo(change_over_time)
  
  p <- ggplot() +
  geom_point(
    data = df %>% filter(!!change_over_time > 0), 
    aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time),
    alpha = alpha,
    shape = 16,
    colour = 'green'
  ) +
  geom_point(
    data = df %>% filter(!!change_over_time < 0), 
    aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1),
    alpha = alpha,
    shape = 16,
    colour = 'red'
  )  +
  scale_size_continuous(range = c(1,17)) +
  scale_x_continuous(trans = 'log10') +
  geom_vline(xintercept = 1, colour = 'blue') +
  guides(size = F) +
  ylab("")
  
  return(p)

}
  

#For LQ change plots, overlay another place on the base plot
#It expects the following:
#dataframe containing a region and sector column, where the sector column is an ordered factor, ordered before it gets here
#a column with the LQ value
#a column with values of change over time showing growth or shrinkage
#Optional columns to include:
#a column with the raw value the LQ is based on, and a column containing the sector regional proportion from the LQ calculation (both appear in text if included)
#a column with min and max values to overlay as bars to indicate full range of the data
addplacename_to_LQplot <- function(df, plot_to_addto, placename, shapenumber=16, backgroundcolour='black', add_gva = F, setalpha = 1,
                                   region_name, sector_name,change_over_time, value_column, LQ_column, sector_regional_proportion,
                                   min_LQ_all_time,max_LQ_all_time){
  
  region_name <- enquo(region_name) 
  sector_name <- enquo(sector_name)
  change_over_time <- enquo(change_over_time)
  LQ_column <- enquo(LQ_column)
  
  plot_to_addto <- plot_to_addto +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time > 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time *1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time < 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time *-1.75),
      shape = shapenumber,
      colour = backgroundcolour,
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time > 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time),
      shape = shapenumber,
      colour = 'green',
      alpha = setalpha
    ) +
    geom_point(
      data = df %>% filter(!!region_name == placename, !!change_over_time < 0), 
      aes(y = !!sector_name, x = !!LQ_column, size = !!change_over_time * -1),
      shape = shapenumber,
      colour = 'red',
      alpha = setalpha
    ) 
  
  #Test for one of these missing, don't display if so
  if(!(missing(value_column)|missing(sector_regional_proportion))){
    value_column <- enquo(value_column)
    sector_regional_proportion <- enquo(sector_regional_proportion)
    
    plot_to_addto <- plot_to_addto +  
      geom_text(
        data = df %>% filter(!!region_name == placename), 
        aes(y = !!sector_name, x = 20, label = paste0('£',value,'M, ',round(!!sector_regional_proportion * 100, 2),'%')),
        nudge_x = 0.3, hjust = 1, alpha = 0.7, size = 3
      )
    
    #Test for one of these missing, don't display if so
    if(!(missing(min_LQ_all_time)|missing(max_LQ_all_time)) ){
      
      min_LQ_all_time <- enquo(min_LQ_all_time)
      max_LQ_all_time <- enquo(max_LQ_all_time)
      
      plot_to_addto <- plot_to_addto +
        geom_errorbar(
          data = df %>% filter(!!region_name == placename),
          aes(y = !!sector_name, xmin = !!min_LQ_all_time, xmax = !!max_LQ_all_time),
          width = 0.1
        )
      
    }
    
    
  }
  
  return(plot_to_addto)
  
}










#2D PROPORTION PLOT COMPARING TWO TIME POINTS
# X axis name or names (of subregions)
# Y axis name or names (of subregions)
# Column to be comparing (so e.g. we can do smoothing on it beforehand if we want)
# time variable
# Start time
# End time
# Compass position to display
twod_proportionplot <- function(df, regionvar, category_var, valuevar, timevar, start_time, end_time, x_regionnames, y_regionnames, compasspoints_to_display = c('NE','NW','SE','SW')){
  
  regionvar <- enquo(regionvar)
  category_var <- enquo(category_var)
  valuevar <- enquo(valuevar)
  timevar <- enquo(timevar)
  
  x_region_results <- df %>% 
    filter(!!regionvar %in% x_regionnames) %>% 
    group_split(!!timevar) %>% 
    map(add_location_quotient_and_proportions, 
        regionvar = !!regionvar,
        lq_var = !!category_var,
        valuevar = !!valuevar) %>% 
    bind_rows() %>% 
    group_by(!!category_var,!!timevar) %>% 
    summarise(x_sector_total_proportion = min(sector_total_proportion))#just get unique values
  
  y_region_results <- df %>% 
    filter(!!regionvar %in% y_regionnames) %>% 
    group_split(!!timevar) %>% 
    map(add_location_quotient_and_proportions, 
        regionvar = !!regionvar,
        lq_var = !!category_var,
        valuevar = !!valuevar) %>% 
    bind_rows() %>% 
    group_by(!!category_var,!!timevar) %>% 
    summarise(y_sector_total_proportion = min(sector_total_proportion))#just get unique values
  
  #Join both
  both <- x_region_results %>% 
    left_join(
      y_region_results,
      by = c(quo_name(category_var),quo_name(timevar))
    )
  
  twoy <- both %>% filter(!!timevar %in% c(start_time, end_time))

  twoy_lags <- twoy %>%
    arrange(!!category_var,!!timevar) %>%
    mutate(
      lag_x_sector_total_proportion = x_sector_total_proportion - lag(x_sector_total_proportion),
      lag_y_sector_total_proportion = y_sector_total_proportion - lag(y_sector_total_proportion)
    ) %>%
    filter(!!timevar == end_time) %>% #using final year to mark when going in particular compass direction
    mutate(
      compass = case_when(
        lag_x_sector_total_proportion < 0 & lag_y_sector_total_proportion < 0 ~ 'SW',
        lag_x_sector_total_proportion < 0 & lag_y_sector_total_proportion > 0 ~ 'NW',
        lag_x_sector_total_proportion > 0 & lag_y_sector_total_proportion > 0 ~ 'NE',
        lag_x_sector_total_proportion > 0 & lag_y_sector_total_proportion < 0 ~ 'SE'
      )
    )

  twoy <- twoy %>%  
    left_join( 
      twoy_lags %>%
        select(!!category_var,compass),
      by = quo_name(category_var)
    )

  
  twoy.wide <- twoy %>% filter(compass %in% compasspoints_to_display) %>%
    mutate(!!timevar := ifelse(!!timevar == min(!!timevar), 'start', 'end')) %>%
    select(!!category_var,!!timevar,x_sector_total_proportion,y_sector_total_proportion) %>%
    pivot_wider(names_from = !!timevar, values_from = c(x_sector_total_proportion,y_sector_total_proportion))

  p <- ggplot() +
    geom_segment(data = twoy.wide, aes(x = x_sector_total_proportion_start * 100, y = y_sector_total_proportion_start  * 100,
                                       xend = x_sector_total_proportion_end * 100, yend = y_sector_total_proportion_end * 100),
                 arrow = arrow(length = unit(0.5, "cm")),
                 size = 1
    )

  p <- p +
    geom_point(data = twoy %>% filter(compass%in%compasspoints_to_display), size = 5, alpha = 0.75,
               aes(x = x_sector_total_proportion * 100, y = y_sector_total_proportion * 100,colour = factor(!!timevar), group = !!category_var)) +
    geom_line(data = twoy %>% filter(compass %in% compasspoints_to_display), size = 1, aes(x = x_sector_total_proportion * 100, y = y_sector_total_proportion * 100, group = !!category_var), colour = 'red') +
    geom_abline(slope = 1, size = 1, colour='blue', alpha = 0.5) +
    # coord_cartesian(xlim = c(0.1,11), ylim = c(0.1,11)) + # good for log scale
    # scale_y_log10() +
    # scale_x_log10() +
    guides(colour=guide_legend(title=" "))
  
  p <- p + geom_text_repel(
    data = twoy %>% filter(!!timevar==max(!!timevar), compass%in%compasspoints_to_display),
    aes(x = x_sector_total_proportion * 100, y = y_sector_total_proportion * 100,label = !!category_var, colour = compass),
    alpha=1,
    nudge_x = .05,
    box.padding = 1,
    nudge_y = 0.05,
    segment.curvature = -0.1,
    segment.ncp = 0.3,
    segment.angle = 20,
    max.overlaps = 20
  ) +
    scale_color_manual(values = setNames(c("red", "black",'#7fc97f','#beaed4','#fdc086','#1f78b4'),
                                         c(start_time, end_time,'NE','SE','NW','SW')))

  p

  
}






