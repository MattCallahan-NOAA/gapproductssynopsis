plot_agecomp_bubble <- function() {
  gap_age <-gap_age %>%
    mutate(year=ifelse(sex=="male", year, year+0.3))
  
  this_year <- rev(sort(unique(gap_biomass$year)))[1]
  first_year <- min(gap_biomass$year)
  
  mcolor <-  "darkblue"
  fcolor <-  "firebrick"
  ncolor <- '#D0D0D0'
  mycolors<-c(mcolor,fcolor, ncolor)
  ggplot()+
    geom_point(data=gap_age #%>% filter(sex !="unsexed")
               , 
               aes(x=year, y=age, size=proportion, color=sex), 
               fill=NA, shape=1, stroke=1.5, alpha=0.75)+
    scale_color_manual(name="sex",values=mycolors)+
    scale_x_continuous(breaks=seq(first_year, this_year, 2))+
    theme_bw()+
    theme(strip.text.y = element_text(angle = 0))
}

#plot_agecomp()