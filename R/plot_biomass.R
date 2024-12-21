plot_biomass <- function() { 
  # calculate difference between most recent survey and previous survey
  this_year <- rev(sort(unique(gap_biomass$year)))[1]
  prev_year <- rev(sort(unique(gap_biomass$year)))[2]
  this_year_biomass <- (gap_biomass %>% filter(year==this_year))$biomass_mt
  prev_year_biomass <- (gap_biomass %>% filter(year==prev_year))$biomass_mt
  biomass_diff_prev <- round(this_year_biomass / prev_year_biomass * 100 - 100 , 2)
  first_year <- min(gap_biomass$year)
  # calculate value of difference between the most recent year and long term mean
  survey_mean_biomass <- mean(gap_biomass$biomass_mt, na.rm=T)
  biomass_diff_mean <- round(this_year_biomass / survey_mean_biomass * 100 - 100 , 2)
  
  
ggplot(data=gap_biomass)+
  geom_ribbon(aes(x=year, 
                  ymin=biomass_mt-sqrt(biomass_var),
                  ymax=biomass_mt+sqrt(biomass_var)), fill="lightgray")+
  geom_line(aes(x=year, y=biomass_mt))+
  geom_point(aes(x=year, y=biomass_mt))+
    # set lower y limit to 0
  scale_y_continuous(limits=c(0,NA))+
    # add time series mean as horrizontal line
  geom_hline(yintercept=survey_mean_biomass, linetype="dashed")+
  # print the biomass_diff_prev and biomass_diff_mean on the plot
  annotate("text", x=2015, y=1.1*max(gap_biomass$biomass_mt), 
           label=paste(this_year," change from ", prev_year, ": ", biomass_diff_prev, "%"))+
  annotate("text", x=2015, y=1*max(gap_biomass$biomass_mt),
           label=paste(this_year, " change from survey mean: ", biomass_diff_mean, "%"))+
  ylab("Biomass (mt)")+xlab("")+
  # show more years on the x axis
  scale_x_continuous(breaks=seq(first_year, this_year, 2))+
  theme_bw()
  
}

#biomass_plot <- plot_biomass()
#biomass_plot
