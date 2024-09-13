##################################################
# code to generate plot

library(egg)
library(tidyverse)
library(akfingapdata)
library(akgfmaps)
library(keyring)
library(gfplot)
library(akmarineareas2)
library(sf)
library(FSA)
library(classInt)



generate_synopsis <- function(species_code=NA, 
                              survey_definition_id=NA, 
                              area_id=NA,
                              start_year=NA,
                              end_year=3000) {
  
  
 # # For testing component, comment out
  # options(scipen = 999)
  # token <-create_token("akfin_secret")
 #  species_code=10110
 #  survey_definition_id=47
 #  area_id=99903
 #  start_year=1990
 #  end_year=3000
  
  # define species common name
  sc<-species_code
  taxa <- get_gap_taxonomic_classification()%>%
    filter(species_code==sc)
  
  common_name<-taxa$common_name
  scientific_name <- taxa$species_name
  
  # region for akgfmaps
  region <- ifelse(survey_definition_id==47, "goa",
                   ifelse(survey_definition_id==98, "bs.south",
                          iselse(survey_definition_id==51, "ai")))
  
  
  # pull biomass 
  gap_biomass <- get_gap_biomass(survey_definition_id = survey_definition_id,
                                 area_id = area_id,
                                 species_code = species_code,
                                 start_year = start_year,
                                 end_year = end_year)
  # pull sizecomp  
  gap_sizecomp <- get_gap_sizecomp(survey_definition_id = survey_definition_id,
                                   area_id = area_id,
                                   species_code = species_code,
                                   start_year = start_year,
                                   end_year = end_year)%>%
    filter(length_mm>=0)%>%
    mutate(sex=factor(case_match(sex, 1~"male", 2~"female", 3~"unsexed"),
                      levels=c("male", "female", "unsexed")))
  
  gap_size_sum <-gap_sizecomp %>%
    group_by(year) %>%
    summarize(total_pop=sum(population_count))
  
  gap_sizecomp <-gap_sizecomp %>%
    left_join(gap_size_sum, by="year")%>%
    mutate(proportion=population_count/total_pop)
  
  
  # pull agecomps
  gap_age <- get_gap_agecomp(survey_definition_id = survey_definition_id,
                             area_id = area_id,
                             species_code = species_code,
                             start_year = start_year,
                             end_year = end_year)
  if(nrow(gap_age>0)) {
    gap_age<-gap_age%>%
      filter(age>=0) %>%
      mutate(sex=factor(case_match(sex, 1~"male", 2~"female", 3~"unsexed"),
                        levels=c("male", "female", "unsexed")))
    
    gap_age_sum <-gap_age %>%
      group_by(year) %>%
      summarize(total_pop=sum(population_count))
    
    gap_age <-gap_age %>%
      left_join(gap_age_sum, by="year")%>%
      mutate(proportion=population_count/total_pop)
  }
  # pull specimen data
  gap_specimen <- get_gap_specimen(survey_definition_id = survey_definition_id,
                                   species_code = species_code,
                                   start_year = start_year,
                                   end_year = end_year)%>%
    mutate(sex=factor(case_match(sex, 1~"male", 2~"female", 3~"unsexed"),
                      levels=c("male", "female", "unsexed")))
  
  #set colors
  # mcolor <- '#0055A4' "darksalmon"
  # fcolor <- '#FF4438' "seagreen4"
  mcolor <-  "darkblue"
  fcolor <-  "firebrick"
  ncolor <- '#D0D0D0'
  mycolors<-c(mcolor,fcolor, ncolor)
  
  #### plot biomass ####
  biomass_plot <- ggplot(data=gap_biomass)+
    geom_ribbon(aes(x=year, 
                    ymin=biomass_mt-sqrt(biomass_var),
                    ymax=biomass_mt+sqrt(biomass_var)), fill="lightgray")+
    geom_line(aes(x=year, y=biomass_mt))+
    geom_point(aes(x=year, y=biomass_mt))+
    ylab("Biomass (mt)")+
    
    theme_bw()
  
  # set plot column number
 # facetcol <- ifelse(survey_definition_id == 98, 2, 1)
  
  #### plot sizecomp ####
  length_plot <- ggplot(data=gap_sizecomp, aes(x=length_mm, y= proportion, color=sex, fill=sex))+
    geom_area(position="identity", alpha = 0.25)+
    scale_color_manual(name="sex",values=mycolors)+
    scale_fill_manual(name="sex",values=mycolors)+
    facet_grid(rows=vars(year))+
    scale_y_continuous(n.breaks = 3)+
    theme_bw()+
    theme(strip.text.y = element_text(angle = 0))
  
  
  #### plot age comp ####
  if(any(gap_age$age>0)) {
    age_plot <- ggplot(data=gap_age, aes(x=age, y= proportion, color=sex, fill=sex))+
      geom_area(position="identity", alpha = 0.25)+
      scale_color_manual(name="sex",values=mycolors)+
      scale_fill_manual(name="sex",values=mycolors)+
      facet_grid(rows=vars(year))+
      scale_y_continuous(n.breaks = 3)+
      theme_bw()+
      theme(strip.text.y = element_text(angle = 0))
  } 
  
  
  #### cpue map ####
  # download haul data
  # gap_haul <- get_gap_haul()
  # saveRDS(gap_haul, "gap_haul.RDS")
  gap_haul<-readRDS("gap_haul.RDS")
  
  #download CPUE data
  gap_cpue <- get_gap_cpue(survey_definition_id = survey_definition_id,
                           species_code = species_code,
                           start_year = start_year,
                           end_year = end_year)
  
  
  recent_years <- rev(sort(unique(gap_cpue$year)))[1:3]
  
  
  gap_cpue_recent <- gap_cpue %>%
    filter(year %in% recent_years) %>%
    left_join(gap_haul, by="hauljoin")
  
  # Set breaks for CPUE
  mapbreaks<-round(classInt::classIntervals(gap_cpue_recent$cpue_kgkm2, n = 5, style = "jenks")$brks,0)
  
  gap_cpue_1 <- gap_cpue_recent %>%
    filter(year == recent_years[1])

  gap_cpue_2 <- gap_cpue_recent %>%
    filter(year == recent_years[2])

  gap_cpue_3 <- gap_cpue_recent %>%
    filter(year == recent_years[3])

  # load modified make_idw_map function
  source("make_idw_map_gs.R")
  
  if(nrow((gap_cpue_1%>%filter(cpue_kgkm2>0)))>5) {
    
    
    idw_map_1 <- make_idw_map_gs(x = gap_cpue_1, # Pass data as a data frame
                                 region = region, # Predefined bs.all area
                                 extrapolation.grid.type = "sf",
                                 in.crs = "+proj=longlat", # Set input coordinate reference system
                                 out.crs = "EPSG:3338", # Set output coordinate reference system
                                 set.breaks = mapbreaks,
                                 key.title=paste0(recent_years[1], " ", common_name),
                                 use.survey.bathymetry = FALSE)
  } else {
    
    x_min<-min(gap_cpue_1$longitude_dd_start)
    x_max<-max(gap_cpue_1$longitude_dd_start)
    y_min<- min(gap_cpue_1$latitude_dd_start)
    y_max<- max(gap_cpue_1$latitude_dd_start)
    idw_map_1 <-ggplot()+
      geom_sf(data=ak_dd %>%st_transform(crs=4326))+
      geom_point(data=gap_cpue_1%>%filter(cpue_kgkm2>0),
                 aes(x=longitude_dd_start, y=latitude_dd_start))+
      xlim(c(x_min, x_max))+ylim(c(y_min, y_max))+
      ggtitle(paste0(recent_years[1], " ", common_name))+
      theme_bw()
  }
  
  #2nd most recent year
  if(nrow((gap_cpue_2%>%filter(cpue_kgkm2>0)))>5) {
    idw_map_2 <- make_idw_map_gs(x = gap_cpue_2, # Pass data as a data frame
                                 region = region, # Predefined bs.all area
                                 extrapolation.grid.type = "sf",
                                 in.crs = "+proj=longlat", # Set input coordinate reference system
                                 out.crs = "EPSG:3338", # Set output coordinate reference system
                                 set.breaks = mapbreaks,
                                 key.title=paste0(recent_years[2], " ", common_name),
                                 use.survey.bathymetry = FALSE)
  } else {
    
    x_min<-min(gap_cpue_2$longitude_dd_start)
    x_max<-max(gap_cpue_2$longitude_dd_start)
    y_min<- min(gap_cpue_2$latitude_dd_start)
    y_max<- max(gap_cpue_2$latitude_dd_start)
    idw_map_2 <-ggplot()+
      geom_sf(data=ak_dd %>%st_transform(crs=4326))+
      geom_point(data=gap_cpue_2%>%filter(cpue_kgkm2>0),
                 aes(x=longitude_dd_start, y=latitude_dd_start))+
      xlim(c(x_min, x_max))+ylim(c(y_min, y_max))+
      ggtitle(paste0(recent_years[2], " ", common_name))+
      theme_bw()
  }
  
  #3rd most recent year
  if(nrow((gap_cpue_3%>%filter(cpue_kgkm2>0)))>5) {
    idw_map_3 <- make_idw_map_gs(x = gap_cpue_3, # Pass data as a data frame
                                 region = region, # Predefined bs.all area
                                 extrapolation.grid.type = "sf",
                                 in.crs = "+proj=longlat", # Set input coordinate reference system
                                 out.crs = "EPSG:3338", # Set output coordinate reference system
                                 set.breaks = mapbreaks,
                                 key.title=paste0(recent_years[3], " ", common_name),
                                 use.survey.bathymetry = FALSE)
  } else {
    
    x_min<-min(gap_cpue_3$longitude_dd_start)
    x_max<-max(gap_cpue_3$longitude_dd_start)
    y_min<- min(gap_cpue_3$latitude_dd_start)
    y_max<- max(gap_cpue_3$latitude_dd_start)
    idw_map_3 <-ggplot()+
      geom_sf(data=ak_dd %>%st_transform(crs=4326))+
      geom_point(data=gap_cpue_3%>%filter(cpue_kgkm2>0),
                 aes(x=longitude_dd_start, y=latitude_dd_start))+
      xlim(c(x_min, x_max))+ylim(c(y_min, y_max))+
      ggtitle(paste0(recent_years[3], " ", common_name))+
      theme_bw()
  }
  
  idw_map <- egg::ggarrange(idw_map_1, idw_map_2, idw_map_3)
  
  #### plot growth ####
  if(any(!is.na(gap_specimen$age))) {
    ageplot_m<-gap_specimen %>%
      filter(age>0 & length_mm >0 & sex == "male")
    
    ageplot_f<-gap_specimen %>%
      filter(age>0 & length_mm >0 & sex == "female")
    
    vbmod <- length_mm ~ Linf * (1 - exp(-K * (age - t0)))
    
    startsm <- vbStarts(formula = length_mm ~ age, data = ageplot_m)
    startsf <- vbStarts(formula = length_mm ~ age, data = ageplot_f)
    
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
  
    
    length_at_age_plot<-ggplot()+
      geom_jitter(data=gap_specimen, aes(x=age, y=length_mm, color=sex), width = 0.1, alpha = 0.15, size = 2)+
      geom_line(data=ageplot_m, aes(x=age, y=pred), color=mcolor, linewidth=2)+
      geom_line(data=ageplot_f, aes(x=age, y=pred), color=fcolor, linewidth=2)+
      geom_text(data=age_label_m_df, aes(x=x, y=y, label=label))+
      geom_text(data=age_label_f_df, aes(x=x, y=y, label=label))+
      scale_color_manual(name="sex",values=mycolors)+
      ylab("length (mm)")+
      theme_bw()
    
  } 
  
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
  
  length_weight_plot<-ggplot()+geom_point(data=gap_specimen, aes(x=length_mm, y=weight_g, color=sex), alpha=0.15, size=2)+
    geom_line(data=lwm, aes(x=length_mm, y=pred), color=mcolor, linewidth=2)+
    geom_line(data=lwf, aes(x=length_mm, y=pred), color=fcolor, linewidth=2)+
    scale_color_manual(name="sex",values=mycolors)+
    geom_text(data=lw_label_m_df, aes(x=x, y=y, label=label))+
    geom_text(data=lw_label_f_df, aes(x=x, y=y, label=label))+
    xlab("length (mm)")+ ylab("weight (g)")+
    theme_bw()
  
  
  
  #### length weight age table ####
  # pull length frequencies
  gap_length <- get_gap_length(survey_definition_id = survey_definition_id,
                               species_code = species_code,
                               start_year = start_year,
                               end_year = end_year)
  
  length_count <- gap_length %>%
    group_by(year) %>%
    summarize(n_samples=sum(frequency, na.rm=T))%>%
    mutate(sample_type = "length")
  weight_count <- gap_specimen %>%
    filter(weight_g > 0) %>%
    group_by(year) %>%
    summarize(n_samples=n())%>%
    mutate(sample_type = "weight")
  if(any(!is.na(gap_specimen$age))) {
    age_count <-gap_specimen %>%
      filter(age > 0) %>%
      group_by(year) %>%
      summarize(n_samples=n())%>%
      mutate(sample_type = "age") }
  else {
    age_count<-gap_specimen %>%
      group_by(year) %>%
      summarize() %>%
      mutate(n_samples=0,
             sample_type = "age")
  }
  biodata_count <-length_count %>%
    bind_rows(weight_count) %>%
    bind_rows(age_count) %>%
    arrange(year) %>%
    mutate(sample_type=factor(sample_type, levels=c("length", "weight", "age")))
  
  min_break<-min(biodata_count$year)
  max_break<-max(biodata_count$year)
  
  sample_size_plot<-ggplot(data=biodata_count)+
    geom_text(aes(x=sample_type, y=year, label=n_samples))+
    scale_y_continuous(breaks=seq(min_break, max_break, by=1))+
    xlab("")+ylab("")+
    ggtitle("sample sizes")+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x=element_blank())
  
  #### set up list of plots to return ####
  
  potential_plots<-c("biomass_plot", "length_plot",  
                     "age_plot", 
                     "length_at_age_plot", "length_weight_plot",
                     "idw_map", "sample_size_plot")
  
  existing_objects <- lapply(potential_plots, function(name) {
    if (exists(name)) {
      return(get(name))
    }
  })
  
  existing_objects <- Filter(Negate(is.null), existing_objects)
  
  return(existing_objects)
  
  # return(list(biomass_plot, length_plot,  
  #              age_plot, 
  #              length_at_age_plot, length_weight_plot,
  #              idw_map, sample_size_plot))
}
