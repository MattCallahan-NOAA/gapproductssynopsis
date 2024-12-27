plot_agecomp <- function() {

  
  mcolor <-  "darkblue"
  fcolor <-  "firebrick"
  ncolor <- '#D0D0D0'
  mycolors<-c(mcolor,fcolor, ncolor)
  
  ggplot(data=gap_age, aes(x=age, y= proportion, color=sex, fill=sex))+
    geom_area(position="identity", alpha = 0.25)+
    scale_color_manual(name="sex",values=mycolors)+
    scale_fill_manual(name="sex",values=mycolors)+
    facet_grid(rows=vars(year))+
    scale_y_continuous(n.breaks = 3)+
    theme_bw()+
    theme(strip.text.y = element_text(angle = 0))
}

#plot_agecomp()
