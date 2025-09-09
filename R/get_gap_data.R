# get gap data
get_gap_data <- function(species_code=NA, 
                         survey_definition_id=NA, 
                         area_id=NA,
                         start_year=NA,
                         end_year=3000) {
  
  token <- create_token("callahan_akfin_api")
  
  sc<-species_code
  taxa <- get_gap_taxonomic_classification()%>%
    filter(species_code==sc)
  
  common_name<-taxa$common_name
  scientific_name <- taxa$species_name
  
  # region for akgfmaps
  region <- ifelse(survey_definition_id==47, "goa",
                   ifelse(survey_definition_id==98, "bs.south",
                          ifelse(survey_definition_id==52, "ai",NA)))
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
  # pull length frequencies
  gap_length <- get_gap_length(survey_definition_id = survey_definition_id,
                               species_code = species_code,
                               start_year = start_year,
                               end_year = end_year)
  # CPUE
  gap_cpue <- akfingapdata::get_gap_cpue(survey_definition_id = survey_definition_id,
                                         species_code = species_code,
                                         start_year = start_year,
                                         end_year = end_year)
  
  # haul
  #gap_haul <- get_gap_haul()
  #saveRDS(gap_haul, "gap_haul.RDS")
  gap_haul<-readRDS("gap_haul.RDS")
  
  # make data available in the global environment
  gap_data <- list(common_name=common_name,
                   scientific_name=scientific_name,
                   region=region,
                   gap_biomass=gap_biomass,
                   gap_sizecomp=gap_sizecomp,
                   gap_age=gap_age,
                   gap_specimen=gap_specimen,
                   gap_length=gap_length,
                   gap_cpue=gap_cpue,
                   gap_haul=gap_haul)
  #return(gap_data)
  for (name in names(gap_data)) {
    assign(name, gap_data[[name]], envir = .GlobalEnv)
  }
  
}

# version with assign() loop
#get_gap_data(species_code=10110, survey_definition_id=47, area_id=99903,start_year=2019, end_year=2024)
