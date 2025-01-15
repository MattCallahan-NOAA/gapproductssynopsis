plot_length_age <- function() {
  
  mcolor <-  "darkblue"
  fcolor <-  "firebrick"
  ncolor <- '#D0D0D0'
  mycolors<-c(mcolor,fcolor, ncolor)
  
  #### plot growth ####
    ageplot_m<-gap_specimen %>%
      filter(age>0 & length_mm >0 & sex == "male")
    
    ageplot_f<-gap_specimen %>%
      filter(age>0 & length_mm >0 & sex == "female")
    
    vbmod <- length_mm ~ Linf * (1 - exp(-K * (age - t0)))
    
    startsm <- FSA::vbStarts(formula = length_mm ~ age, data = ageplot_m)
    startsf <- FSA::vbStarts(formula = length_mm ~ age, data = ageplot_f)
    
    # Fails due to bad starts for shortraker. These starts fixed it.
    # startsf[1]=800
    # startsf[2]=0.1
    # startsf[3]=4
    
    age_modm <-nls(vbmod, data = ageplot_m, start = startsm)
    age_modf <-nls(vbmod, data = ageplot_f, start = startsf)
    
    
    predm <- predict(age_modm)
    predf <- predict(age_modf)
    
    ageplot_m$pred <-predm
    ageplot_f$pred <-predf
    
    age_coef_m<-round(coef(age_modm),2)
    age_coef_f<-round(coef(age_modf),2)
    
    age_label_m <- paste0("male: ", names(age_coef_m[1]),"=", age_coef_m[1], ", ",
                          names(age_coef_m[2]), "=", age_coef_m[2], ", ",
                          names(age_coef_m[3]), "=", age_coef_m[3])
    
    age_label_f <- paste0("female: ", names(age_coef_f[1]),"=", age_coef_f[1], ", ",
                          names(age_coef_f[2]), "=", age_coef_f[2], ", ",
                          names(age_coef_f[3]), "=", age_coef_f[3])
    
    age_lab_x_m <- max(gap_specimen$age, na.rm=T)*0.6
    age_lab_x_f <- max(gap_specimen$age, na.rm=T)*0.6
    age_lab_y_m <- max(gap_specimen$length_mm, na.rm=T)*0.3
    age_lab_y_f <- max(gap_specimen$length_mm, na.rm=T)*0.2
    
    age_label_m_df <- tibble(x=age_lab_x_m, y=age_lab_y_m, label=age_label_m)
    age_label_f_df <- tibble(x=age_lab_x_f, y=age_lab_y_f, label=age_label_f)
    
    
   ggplot()+
      geom_jitter(data=gap_specimen, aes(x=age, y=length_mm, color=sex), width = 0.1, alpha = 0.15, size = 2)+
      geom_line(data=ageplot_m, aes(x=age, y=pred), color=mcolor, linewidth=2)+
      geom_line(data=ageplot_f, aes(x=age, y=pred), color=fcolor, linewidth=2)+
      geom_text(data=age_label_m_df, aes(x=x, y=y, label=label))+
      geom_text(data=age_label_f_df, aes(x=x, y=y, label=label))+
      scale_color_manual(name="sex",values=mycolors)+
      ylab("length (mm)")+
      theme_bw()

}

#plot_length_age()
