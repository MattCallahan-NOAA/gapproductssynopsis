library(quarto)
library(keyring)
library(akfingapdata)
library(magrittr)
library(dplyr)
library(here)
library(akgfmaps)
library(ggplot2)
library(gridExtra)
library(grid)

token <- create_token("callahan_akfin_api")
atf_cpue <- akfingapdata::get_gap_cpue(survey_definition_id = 47,
                                       species_code = 10110,
                                       start_year = 2021,
                                       end_year = 2025)

rex_cpue <- akfingapdata::get_gap_cpue(survey_definition_id = 47,
                                       species_code = 10200,
                                       start_year = 2021,
                                       end_year = 2025)
gap_haul <- readRDS("gap_haul.RDS")

###############################################################################
make_cpue_bubble_map <- function(x = NA,
                                 common_name = NA,
                                 latitude_dd_start = NA,
                                 longitude_dd_start = NA,
                                 cpue_kgkm2 = NA,
                                 region = "bs.south",
                                 extrap.box = NULL,
                                 extrapolation.grid.type = "sf",
                                 set.breaks = NULL,
                                 set.limits = NULL,
                                 set.labels = NULL,
                                 year_label = NULL,
                                 grid.cell = c(5000,5000),
                                 in.crs = "+proj=longlat",
                                 out.crs = "EPSG:3338",
                                 key.title = "auto",
                                 key.title.units = "CPUE (kg/km^2)",
                                 log.transform = FALSE,
                                 idw.nmax = 4,
                                 use.survey.bathymetry = FALSE,
                                 return.continuous.grid = TRUE) {
  
  stopifnot("make_idw_map: extra.grid.type must be 'stars', 'sf', or 'sf.simple'" = extrapolation.grid.type %in% c("stars", "sf", "sf.simple"))
  
  if(!is.data.frame(x)) {
    stopifnot("make_idw_map: latitude_dd_start must be a numeric vector." = is.numeric(latitude_dd_start))
    stopifnot("make_idw_map: longitude_dd_start must be a numeric vector." = is.numeric(longitude_dd_start))
    stopifnot("make_idw_map: cpue_kgkm2 must be a numeric vector." = is.numeric(cpue_kgkm2))
    
    x <- data.frame(common_name = common_name,
                    latitude_dd_start = latitude_dd_start,
                    longitude_dd_start = longitude_dd_start,
                    cpue_kgkm2 = cpue_kgkm2)
  }
  
  x <- as.data.frame(x)
  
  if(key.title == "auto") {
    key.title <- x$common_name[1]
  }
  
  map_layers <- akgfmaps::get_base_layers(select.region = region, set.crs = out.crs)
  
  if(is.null(extrap.box)) {
    extrap.box <- sf::st_bbox(map_layers$survey.area)
  }
  
  if(out.crs == "auto") {
    out.crs <- map_layers$crs
  }
  
  if(use.survey.bathymetry) {
    map_layers$bathymetry <- akgfmaps::get_survey_bathymetry(select.region = region, set.crs = out.crs)
  }
  
  x <- sf::st_as_sf(x,
                    coords = c(x = "longitude_dd_start", y = "latitude_dd_start"),
                    crs = sf::st_crs(in.crs)) |>
    sf::st_transform(crs = map_layers$crs)
  
  # Separate zero catch points from positive catch points
  x_zero <- x %>% dplyr::filter(cpue_kgkm2 == 0)
  x_pos  <- x %>% dplyr::filter(cpue_kgkm2 > 0)
  
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = map_layers$survey.area, fill = NA) +
    # Layer 1: Zero CPUE represented as small gray 'x'
    ggplot2::geom_sf(data = x_zero, shape = 4, color = "black", size = 2, alpha = 0.6) +
    # Layer 2: Positive CPUE represented as bubbles
    ggplot2::geom_sf(data = x_pos,
                     ggplot2::aes(size = cpue_kgkm2, fill = cpue_kgkm2, color = cpue_kgkm2),
                     shape = 21, alpha = 0.7) +
    ggplot2::geom_sf(data = map_layers$akland, fill = "grey80") +
    ggplot2::geom_sf(data = map_layers$graticule, color = ggplot2::alpha("grey70", 0.3)) +
    ggplot2::scale_size_continuous(
      name = key.title.units,
      limits = set.limits,
      breaks = set.breaks,
      labels = set.labels
    ) +
    ggplot2::scale_color_viridis_c(
      name = key.title.units,
      limits = set.limits,
      breaks = set.breaks,
      labels = set.labels
    ) +
    ggplot2::scale_fill_viridis_c(
      name = key.title.units,
      limits = set.limits,
      breaks = set.breaks,
      labels = set.labels
    ) +
    ggplot2::scale_x_continuous(breaks = map_layers$lon.breaks) +
    ggplot2::scale_y_continuous(breaks = map_layers$lat.breaks) +
    ggplot2::coord_sf(xlim = map_layers$plot.boundary$x,
                      ylim = map_layers$plot.boundary$y) +
    ggplot2::labs(subtitle = year_label) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
      panel.background = ggplot2::element_rect(fill = NA, color = "black"),
      legend.key = ggplot2::element_rect(fill = NA, color = "grey70"),
      legend.position = "bottom",
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 9),
      plot.subtitle = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
      plot.background = ggplot2::element_rect(fill = NA, color = NA)
    )
  
  return(p1)
}

region <- "goa"

plot_cpue_bubble_last_three_years <- function(gap_cpue, species_name = "Arrowtooth Flounder") {
  
  recent_years <- rev(sort(unique(gap_cpue$year)))[1:3]
  
  gap_cpue_recent <- gap_cpue %>%
    filter(year %in% recent_years) %>%
    left_join(gap_haul, by = "hauljoin")
  
  max_cpue <- max(gap_cpue_recent$cpue_kgkm2, na.rm = TRUE)
  mapbreaks <- seq(0, max_cpue, length.out = 4) %>% round(0)
  
  maplimits <- c(0, max(mapbreaks))
  maplabels <- c(format(mapbreaks[1], big.mark = ","), "", "", format(mapbreaks[4], big.mark = ","))
  
  sp_code <- gap_cpue$species_code[1]
  sp_name <- if("common_name" %in% names(gap_cpue)) gap_cpue$common_name[1] else species_name
  plot_title <- paste0("Region: ", toupper(region), " | Species Code: ", sp_code, " (", sp_name, ")")
  
  gap_cpue_1 <- gap_cpue_recent %>% filter(year == recent_years[1])
  gap_cpue_2 <- gap_cpue_recent %>% filter(year == recent_years[2])
  gap_cpue_3 <- gap_cpue_recent %>% filter(year == recent_years[3])
  
  map_1 <- make_cpue_bubble_map(x = gap_cpue_1,
                                region = region,
                                set.breaks = mapbreaks,
                                set.limits = maplimits,
                                set.labels = maplabels,
                                year_label = as.character(recent_years[1]))
  
  map_2 <- make_cpue_bubble_map(x = gap_cpue_2,
                                region = region,
                                set.breaks = mapbreaks,
                                set.limits = maplimits,
                                set.labels = maplabels,
                                year_label = as.character(recent_years[2]))
  
  map_3 <- make_cpue_bubble_map(x = gap_cpue_3,
                                region = region,
                                set.breaks = mapbreaks,
                                set.limits = maplimits,
                                set.labels = maplabels,
                                year_label = as.character(recent_years[3]))
  
  map <- gridExtra::grid.arrange(
    map_1, map_2, map_3, 
    ncol = 1, 
    top = grid::textGrob(plot_title, gp = grid::gpar(fontsize = 14, font = 2))
  )
  
  return(map)
}


plot_cpue_bubble_last_three_years(rex_cpue, species_name = "Rex sole")
plot_cpue_bubble_last_three_years(atf_cpue, species_name = "Arrowtooth flounder")
