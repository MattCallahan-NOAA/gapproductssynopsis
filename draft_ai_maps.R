# 
# source("make_idw_map_nolegend.R")
# source("make_idw_map_gs.R")
# #test AI map
# options(scipen = 999)
# token <-create_token("akfin_secret")
#  species_code=10110
#  survey_definition_id=52
#  area_id=99904
#  start_year=1990
#  end_year=3000
#  
#  sc<-species_code
#  taxa <- get_gap_taxonomic_classification()%>%
#    filter(species_code==sc)
#  
#  common_name<-taxa$common_name
#  scientific_name <- taxa$species_name
#  
# 
#  # region for akgfmaps
#  region <- ifelse(survey_definition_id==47, "goa",
#                   ifelse(survey_definition_id==98, "bs.south",
#                          ifelse(survey_definition_id==52, "ai", NA)))
#  
#  gap_haul<-readRDS("gap_haul.RDS")
#  
#  #download CPUE data
#  gap_cpue <- get_gap_cpue(survey_definition_id = survey_definition_id,
#                           species_code = species_code,
#                           start_year = start_year,
#                           end_year = end_year)
#  
#  
#  recent_years <- rev(sort(unique(gap_cpue$year)))[1:3]
#  
#  
#  gap_cpue_recent <- gap_cpue %>%
#    filter(year %in% recent_years) %>%
#    left_join(gap_haul, by="hauljoin")
#  
#  # Set breaks for CPUE
#  mapbreaks<-round(classInt::classIntervals(gap_cpue_recent$cpue_kgkm2, n = 5, style = "jenks")$brks,0)
#  
#  gap_cpue_1 <- gap_cpue_recent %>%
#    filter(year == recent_years[1])
#  
#  gap_cpue_2 <- gap_cpue_recent %>%
#    filter(year == recent_years[2])
#  
#  gap_cpue_3 <- gap_cpue_recent %>%
#    filter(year == recent_years[3])
#  
 make_ai_idw_maps <- function(x,y, recent_years,common_name, mapbreaks) {
 idw_map_1w <- make_idw_map_nolegend(x = x, # Pass data as a data frame
                              region = "ai.west", # Predefined bs.all area
                              extrapolation.grid.type = "sf",
                              in.crs = "+proj=longlat", # Set input coordinate reference system
                              out.crs = "EPSG:3338", # Set output coordinate reference system
                              set.breaks = mapbreaks,
                              key.title=paste0(recent_years[y], " ", common_name),
                              use.survey.bathymetry = FALSE)+
   ggplot2::ggtitle("ai.west")


 
 idw_map_1e   <- make_idw_map_nolegend(x = x, # Pass data as a data frame
                                           region = "ai.east", # Predefined bs.all area
                                           extrapolation.grid.type = "sf",
                                           in.crs = "+proj=longlat", # Set input coordinate reference system
                                           out.crs = "EPSG:3338", # Set output coordinate reference system
                                           set.breaks = mapbreaks,
                                           key.title=paste0(recent_years[y], " ", common_name),
                                           use.survey.bathymetry = FALSE)+
   ggplot2::ggtitle("ai.east")

 
 
 idw_map_1c <- make_idw_map_gs(x = x, # Pass data as a data frame
                                           region = "ai.central", # Predefined bs.all area
                                           extrapolation.grid.type = "sf",
                                           in.crs = "+proj=longlat", # Set input coordinate reference system
                                           out.crs = "EPSG:3338", # Set output coordinate reference system
                                           set.breaks = mapbreaks,
                                           key.title=paste0(recent_years[y], " ", common_name),
                                           use.survey.bathymetry = FALSE)+
   ggplot2::ggtitle("ai.central")

 
 #aimap <- ggarrange(idw_map_1w, idw_map_1e, idw_map_1c, nrow=2)

aimap <- grid.arrange(
  arrangeGrob(idw_map_1w, idw_map_1e, ncol = 2, widths = c(1, 2.6)),  
  idw_map_1c,                                                  
  nrow = 2,                                               
  heights = c(1, 2)    
)

aimap

}

 #xx <-make_ai_idw_maps(x=gap_cpue_1,y=1,recent_years=recent_years, mapbreaks=mapbreaks, common_name=common_name)
# xx <-make_ai_idw_maps(x=gap_cpue_2,y=2)
 