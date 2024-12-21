plot_length_weight <- function() {
  
  mcolor <-  "darkblue"
  fcolor <-  "firebrick"
  ncolor <- '#D0D0D0'
  mycolors<-c(mcolor,fcolor, ncolor)
  
  #### plot length/weight ####
  lwm<-gap_specimen %>%
    filter(weight_g>0 & length_mm >0 & sex == "male")
  
  lwf<-gap_specimen %>%
    filter(weight_g>0 & length_mm >0 & sex == "female")
  
  lwmodm <- lm(log(weight_g)~log(length_mm), data=lwm)
  lwmodf <- lm(log(weight_g)~log(length_mm), data=lwf)
  
  lwpredm <- exp(predict(lwmodm))
  lwpredf <- exp(predict(lwmodf))
  
  lwm$pred <-lwpredm
  lwf$pred <-lwpredf
  
  lw_coef_m<-round(coef(lwmodm),2)
  lw_coef_f<-round(coef(lwmodf),2)
  
  lw_label_m <- paste0("male: ","log(a)=", lw_coef_m[2], ", ",
                       "b=", lw_coef_m[1])
  
  lw_label_f <- paste0("female: ","log(a)=", lw_coef_f[2], ", ",
                       "b=", lw_coef_f[1])
  
  lw_lab_x_m <- max(gap_specimen$length_mm, na.rm=T)*0.4
  lw_lab_x_f <- max(gap_specimen$length_mm, na.rm=T)*0.4
  lw_lab_y_m <- max(gap_specimen$weight_g, na.rm=T)*0.9
  lw_lab_y_f <- max(gap_specimen$weight_g, na.rm=T)*0.8
  
  lw_label_m_df <- tibble(x=lw_lab_x_m, y=lw_lab_y_m, label=lw_label_m)
  lw_label_f_df <- tibble(x=lw_lab_x_f, y=lw_lab_y_f, label=lw_label_f)
  
  ggplot()+geom_point(data=gap_specimen, aes(x=length_mm, y=weight_g, color=sex), alpha=0.15, size=2)+
    geom_line(data=lwm, aes(x=length_mm, y=pred), color=mcolor, linewidth=2)+
    geom_line(data=lwf, aes(x=length_mm, y=pred), color=fcolor, linewidth=2)+
    scale_color_manual(name="sex",values=mycolors)+
    geom_text(data=lw_label_m_df, aes(x=x, y=y, label=label))+
    geom_text(data=lw_label_f_df, aes(x=x, y=y, label=label))+
    xlab("length (mm)")+ ylab("weight (g)")+
    theme_bw()
  
  
}

#plot_length_weight()
