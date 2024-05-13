# devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
# devtools::install_github("MattCallahan-NOAA/akfingapdata")
# devtools::install_github("pbs-assess/gfplot")
#devtools::install_github("MattCallahan-NOAA/akmarineareas2")

library(gridExtra)
library(tidyverse)
library(akfingapdata)
library(akgfmaps)
library(keyring)
library(gfplot)
library(akmarineareas2)
library(sf)
library(FSA)

generate_synopsis <- function(species_code=NA, 
                              survey_definition_id=47, 
                              area_id=99903,
                              start_year=1990,
                              end_year=3000) {
  # define token for pulling gap data from akfin api
  token <-create_token("akfin_secret")
  
  # species_code=10110
  # survey_definition_id=47 
  # area_id=99903
  # start_year=1990
  # end_year=3000

  # define species common name
  sc<-species_code
  taxa <- get_gap_taxonomic_classification()%>%
    filter(species_code==sc)
  
  common_name<-taxa$common_name
  scientific_name <- taxa$species_name
  

  # pull biomass 
  gap_biomass <- get_gap_biomass(survey_definition_id = survey_definition_id,
                             area_id = area_id,
                             species_code = species_code,
                             start_year = start_year,
                             end_year = end_year)
  # pull length frequencies
  gap_length <- get_gap_length(survey_definition_id = survey_definition_id,
                             species_code = species_code,
                             start_year = start_year,
                             end_year = end_year)
  # pull agecomps
  gap_age <- get_gap_agecomp(survey_definition_id = survey_definition_id,
                          area_id = area_id,
                          species_code = species_code,
                          start_year = start_year,
                          end_year = end_year)
  # pull specimen data
  gap_specimen <- get_gap_specimen(survey_definition_id = survey_definition_id,
                               species_code = species_code,
                               start_year = start_year,
                               end_year = end_year)

  #set colors
  mcolor <- '#0055A4'
  fcolor <- '#FF4438'
  ncolor <- '#D0D0D0'
  mycolors<-c(mcolor,fcolor, ncolor)
  
# plot biomass
biomass_plot <- ggplot(data=gap_biomass)+
  geom_line(aes(x=year, y=biomass_mt))+
  geom_line(aes(x=year, y=biomass_mt-sqrt(biomass_var)), lty=2)+
  geom_line(aes(x=year, y=biomass_mt+sqrt(biomass_var)), lty=2)+
  ylab("Biomass (mt)")+
  theme_bw()

# plot length
gap_length_p <-gap_length %>% 
  group_by(length_mm, year, sex) %>%
  summarize(frequency = sum(frequency))

length_plot <- ggplot(data=gap_length_p)+
  geom_line(aes(x=length_mm, y= frequency, color=factor(sex)))+
  scale_color_manual(name="sex",values=mycolors)+
  facet_grid(rows=vars(year))+
  theme_bw()+
  theme()

# plot age comp
if(any(gap_age$age>0)) {
age_plot <- ggplot(data=gap_age %>% filter(age>=0))+
  geom_line(aes(x=age, y= population_count, color=factor(sex)))+
  scale_color_manual(name="sex",values=mycolors)+
  facet_grid(rows=vars(year))+
  theme_bw()
} else {
  age_plot <- ggplot()+ggtitle("no age data")
}
# plot age as in the gfsynopsis
# if(any(!is.na(gap_specimen$age))) {
# gap_spec_p <- gap_specimen %>%
#   filter(age >=0) %>%
#   group_by(age, sex, year) %>%
#   summarize(N=n())%>%
#   rowwise() %>%
#   mutate(year2=ifelse(sex==1, year, year+0.5))
# 
# age_plot2 <- ggplot(data=gap_spec_p)+
#   geom_point(aes(x=year2, y=age, size=N, color=factor(sex)), shape=21 )+
#   scale_color_manual(name="sex",values=mycolors)+
#   theme_bw()
# } else {
#   age_plot2 <- ggplot()+ggtitle("no age data")
# }
  
# cpue map
# download haul data
gap_haul <- get_gap_haul()

#download CPUE data
gap_cpue <- get_gap_cpue(survey_definition_id = survey_definition_id,
                         species_code = species_code,
                         start_year = start_year,
                         end_year = end_year)

recent_year<- max(gap_cpue$year)


gap_cpue_recent <- gap_cpue %>%
  filter(year==recent_year) %>%
  left_join(gap_haul, by="hauljoin")

if(nrow((gap_cpue_recent%>%filter(cpue_kgkm2>0)))<5) {
  x_min<-min(gap_cpue_recent$longitude_dd_start)
  x_max<-max(gap_cpue_recent$longitude_dd_start)
  y_min<- min(gap_cpue_recent$latitude_dd_start)
  y_max<- max(gap_cpue_recent$latitude_dd_start)
  idw_map <-ggplot()+
    geom_sf(data=ak_dd %>%st_transform(crs=4326))+
    geom_point(data=gap_cpue_recent%>%filter(cpue_kgkm2>0), 
               aes(x=longitude_dd_start, y=latitude_dd_start))+
    xlim(c(x_min, x_max))+ylim(c(y_min, y_max))+
    theme_bw()
    
} else {

# load modified make_idw_map function
source("make_idw_map_gs.R")

idw_map <- make_idw_map_gs(x = gap_cpue_recent, # Pass data as a data frame
                     region = "goa", # Predefined bs.all area
                     extrapolation.grid.type = "sf",
                     set.breaks = "jenks", # Gets Jenks breaks from classint::classIntervals()
                     in.crs = "+proj=longlat", # Set input coordinate reference system
                     out.crs = "EPSG:3338", # Set output coordinate reference system
                     #grid.cell = c(20000, 20000), # 20x20km grid
                     key.title = common_name,
                     use.survey.bathymetry = FALSE)
}

# plot growth
# If we want the actual VBF fit and equations that will take a little more work.
if(any(!is.na(gap_specimen$age))) {
  ageplot_m<-gap_specimen %>%
    filter(age>0 & length_mm >0 & sex == 1)
  
  ageplot_f<-gap_specimen %>%
    filter(age>0 & length_mm >0 & sex == 2)
  
  vbmod <- length_mm ~ Linf * (1 - exp(-K * (age - t0)))
  
  startsm <- vbStarts(formula = length_mm ~ age, data = ageplot_m)
  startsf <- vbStarts(formula = length_mm ~ age, data = ageplot_f)
  
  age_modm <-nls(vbmod, data = ageplot_m, start = startsm)
  age_modf <-nls(vbmod, data = ageplot_f, start = startsf)
  
  predm <- predict(age_modm)
  predf <- predict(age_modf)
  
  ageplot_m$pred <-predm
  ageplot_f$pred <-predf
  
  length_at_age_plot<-ggplot()+
    geom_jitter(data=gap_specimen, aes(x=age, y=length_mm), width = 0.1, alpha = 0.15, size = 2)+
    geom_line(data=ageplot_m, aes(x=age, y=pred), color=mcolor)+
    geom_line(data=ageplot_f, aes(x=age, y=pred), color=fcolor)+
    theme_bw()
  length_at_age_plot

} else {
  length_at_age_plot <- ggplot()+ggtitle("no age data")
}
length_weight_plot<-ggplot()+geom_point(data=gap_specimen, aes(x=length_mm, y=weight_g), color="lightgray", size=2)+
  geom_smooth(data=gap_specimen%>%filter(sex==1),aes(x=length_mm, y=weight_g), method="loess", se=FALSE, color=mcolor)+
  geom_smooth(data=gap_specimen%>%filter(sex==2),aes(x=length_mm, y=weight_g), method="loess", se=FALSE, color=fcolor)+
  theme_bw()

### length weight age ###
length_count <- gap_length %>%
  group_by(year) %>%
  summarize(n_samples=sum(frequency))%>%
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
  arrange(year)

min_break<-min(biodata_count$year)-1
max_break<-max(biodata_count$year)

sample_size_plot<-ggplot(data=biodata_count)+
  geom_text(aes(x=sample_type, y=year, label=n_samples))+
  scale_y_continuous(breaks=seq(min_break, max_break, by=2))+
  xlab("")+ylab("")+
  ggtitle("sample sizes")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x=element_blank())


grid.arrange(top=paste0(common_name,"\n",scientific_name), ncol=1,
             length_plot, biomass_plot, 
             age_plot, 
             length_at_age_plot, length_weight_plot,
             idw_map, sample_size_plot)
}


png("arrowtooth_test.png", width=20, height = 30, units="in",res=300)
generate_synopsis(species_code=10110)
dev.off()

png("sleeper_shark_test.png", width=20, height = 30, units="in",res=300)
generate_synopsis(species_code=320)
dev.off()

png("Northern_rockfish_test.png", width=20, height = 60, units="in",res=300)
generate_synopsis(species_code=30420)
dev.off()

codes<-readRDS("common_fishes.RDS")

