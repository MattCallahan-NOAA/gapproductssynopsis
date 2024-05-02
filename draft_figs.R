devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
devtools::install_github("MattCallahan-NOAA/akfingapdata")
pak::pak("afsc-gap-products/akgfmaps")

library(gridExtra)
library(tidyverse)
library(akfingapdata)
library(akgfmaps)
library(keyring)

generate_synopsis <- function(species) {
  
}

token <-create_token("akfin_secret")


# define api inputs
survey_definition_id <- 47
area_id <-99903
species_code <- 10110
start_year <- 1990
end_year <- 3000

  
gap_biomass <- get_gap_biomass(survey_definition_id = survey_definition_id,
                             area_id = area_id,
                             species_code = species_code,
                             start_year = start_year,
                             end_year = end_year)

gap_length <- get_gap_length(survey_definition_id = survey_definition_id,
                             species_code = species_code,
                             start_year = start_year,
                             end_year = end_year)

# gap_age <- get_gap_agecomp(survey_definition_id = survey_definition_id,
#                           area_id = area_id,
#                           species_code = species_code,
#                           start_year = start_year,
#                           end_year = end_year)

gap_specimen <- get_gap_specimen(survey_definition_id = survey_definition_id,
                               species_code = species_code,
                               start_year = start_year,
                               end_year = end_year)

# biomass
biomass_plot <- ggplot(data=gap_biomass)+
  geom_line(aes(x=year, y=biomass_mt))+
  ylab("Biomass (mt)")+
  theme_bw()

biomass_plot

#length
gap_length_p <-gap_length %>% 
  #filter(sex %in% c(1,2)) %>%
  group_by(length_mm, year, sex) %>%
  summarize(frequency = sum(frequency))


length_plot <- ggplot(data=gap_length_p)+
  geom_line(aes(x=length_mm, y= frequency, color=factor(sex)))+
  facet_grid(rows=vars(year))+
  theme_bw()

length_plot

# age comp
age_plot <- ggplot(data=gap_age %>% filter(age>=0))+
  geom_line(aes(x=age, y= population_count, color=factor(sex)))+
  facet_grid(rows=vars(year))+
  theme_bw()

age_plot

# age frequency
gap_spec_p <- gap_specimen %>%
  filter(age >=0) %>%
  group_by(age, sex, year) %>%
  summarize(N=n())%>%
  rowwise() %>%
  mutate(year2=ifelse(sex==1, year, year+0.5))

age_plot2 <- ggplot(data=gap_spec_p)+
  geom_point(aes(x=year2, y=age, size=N, color=factor(sex)), shape=21 )+
  theme_bw()
age_plot2

# cpue map
# download haul data
gap_haul <- get_gap_haul()
#download CPUE data
gap_cpue <- get_gap_cpue()

yfs2017 <- akgfmaps::YFS2017
head(yfs2017)

opt1 <- make_idw_map(x = yfs2017, # Pass data as a data frame
                     region = "goa", # Predefined bs.all area
                     set.breaks = "jenks", # Gets Jenks breaks from classint::classIntervals()
                     in.crs = "+proj=longlat", # Set input coordinate reference system
                     out.crs = "EPSG:3338", # Set output coordinate reference system
                     grid.cell = c(20000, 20000), # 20x20km grid
                     key.title = species_code,
                     use.survey.bathymetry = FALSE) # Include yellowfin sole in the legend title

opt1



# Growth
fit_vb_mc <- function(data, sex) {
  
}
g_vb <- ggplot() +
  theme_pbs() +
  xlab("Age (years)")) +
  ylab("Length"" (cm)") +
  ggtitle("Growth"))
g_length_weight <- ggplot() +
  theme_pbs() +
  xlab("Length"), " (cm)")) +
  ylab(paste0("Weight"), " (kg)")) +
  ggtitle("Length-weight relationship") 


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
age_count <-gap_specimen %>%
  filter(age > 0) %>%
  group_by(year) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "age")
biodata_count <-length_count %>%
  bind_rows(weight_count) %>%
  bind_rows(age_count)

ggplot(data=biodata_count)+
  geom_text(aes(x=year, y=sample_type, label=n_samples))
