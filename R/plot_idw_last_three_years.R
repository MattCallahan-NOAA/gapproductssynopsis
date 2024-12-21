library(akfingapdata)
library(akgfmaps)
library(keyring)
library(tidyverse)

plot_idw_last_three_years <- function() {
source("make_idw_map_gs.R")

# configure haul and cpue data
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

idw_map_1 <- make_idw_map_gs(x = gap_cpue_1, # Pass data as a data frame
                             region = region, # Predefined bs.all area
                             extrapolation.grid.type = "sf",
                             in.crs = "+proj=longlat", # Set input coordinate reference system
                             out.crs = "EPSG:3338", # Set output coordinate reference system
                             set.breaks = mapbreaks,
                             key.title=paste0(recent_years[1], " ", common_name),
                             use.survey.bathymetry = FALSE)

idw_map_2 <- make_idw_map_gs(x = gap_cpue_2, # Pass data as a data frame
                             region = region, # Predefined bs.all area
                             extrapolation.grid.type = "sf",
                             in.crs = "+proj=longlat", # Set input coordinate reference system
                             out.crs = "EPSG:3338", # Set output coordinate reference system
                             set.breaks = mapbreaks,
                             key.title=paste0(recent_years[2], " ", common_name),
                             use.survey.bathymetry = FALSE)

idw_map_3 <- make_idw_map_gs(x = gap_cpue_3, # Pass data as a data frame
                             region = region, # Predefined bs.all area
                             extrapolation.grid.type = "sf",
                             in.crs = "+proj=longlat", # Set input coordinate reference system
                             out.crs = "EPSG:3338", # Set output coordinate reference system
                             set.breaks = mapbreaks,
                             key.title=paste0(recent_years[3], " ", common_name),
                             use.survey.bathymetry = FALSE)

idw_map <- egg::ggarrange(idw_map_1, idw_map_2, idw_map_3)
return(idw_map)
}

#idw_map<-plot_idw_last_three_years()
#idw_map