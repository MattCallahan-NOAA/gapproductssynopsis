
###############################################################################
make_idw_map_gs <- function(x = NA,
                         common_name = NA,
                         latitude_dd_start = NA,
                         longitude_dd_start = NA,
                         cpue_kgkm2 = NA,
                         region = "bs.south",
                         extrap.box = NULL,
                         extrapolation.grid.type = "stars",
                         set.breaks = "jenks",
                         grid.cell = c(5000,5000),
                         in.crs = "+proj=longlat",
                         out.crs = "EPSG:3338",
                         key.title = "auto",
                         key.title.units = "CPUE (kg/km^2)",
                         log.transform = FALSE,
                         idw.nmax = 4,
                         use.survey.bathymetry = FALSE,
                         return.continuous.grid = TRUE) {
  
 # .check_region(select.region = region, type = "survey")
  
  stopifnot("make_idw_map: extra.grid.type must be 'stars', 'sf', or 'sf.simple'" = extrapolation.grid.type %in% c("stars", "sf", "sf.simple"))
  
  # Convert vectors to data frame if x is not a data.frame or tbl-----------------------------------
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
  
  
  # Set legend title--------------------------------------------------------------------------------
  if(key.title == "auto") {
    key.title <- x$common_name[1]
  }
  
  # Load map layers---------------------------------------------------------------------------------
  map_layers <- akgfmaps::get_base_layers(select.region = region, set.crs = out.crs)
  
  # Set up mapping region---------------------------------------------------------------------------
  if(is.null(extrap.box)) {
    extrap.box <- sf::st_bbox(map_layers$survey.area)
  }
  
  # Assign CRS to handle automatic selection--------------------------------------------------------
  if(out.crs == "auto") {
    out.crs <- map_layers$crs
  }
  
  # Use survey bathymetry---------------------------------------------------------------------------
  if(use.survey.bathymetry) {
    map_layers$bathymetry <- akgfmaps::get_survey_bathymetry(select.region = region,
                                                             set.crs = out.crs)
  }
  
  # Assign CRS to input data------------------------------------------------------------------------
  x <- sf::st_as_sf(x,
                    coords = c(x = "longitude_dd_start", y = "latitude_dd_start"),
                    crs = sf::st_crs(in.crs)) |>
    sf::st_transform(crs = map_layers$crs)
  
  # Inverse distance weighting----------------------------------------------------------------------
  idw_fit <- gstat::gstat(formula = cpue_kgkm2~1,
                          locations = x,
                          nmax = idw.nmax)
  
  # Predict station points--------------------------------------------------------------------------
  stn.predict <- predict(idw_fit, x)
  
  # Generate interpolation grid---------------------------------------------------------------------
  extrap_raster <- terra::rast(xmin = extrap.box['xmin'],
                               xmax = extrap.box['xmax'],
                               ymin = extrap.box['ymin'],
                               ymax = extrap.box['ymax'],
                               ncol = (extrap.box['xmax']-extrap.box['xmin'])/grid.cell[1],
                               nrow = (extrap.box['ymax']-extrap.box['ymin'])/grid.cell[2],
                               crs = out.crs)
  
  # Predict, rasterize, mask------------------------------------------------------------------------
  loc_df <- suppressWarnings(terra::crds(extrap_raster, df = TRUE, na.rm = FALSE)) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = out.crs)
  
  extrap.grid <- predict(idw_fit, loc_df) |>
    sf::st_transform(crs = sf::st_crs(x)) |>
    stars::st_rasterize() |>
    sf::st_join(map_layers$survey.area, join = st_intersects)
  
  # Return continuous grid if return.continuous.grid is TRUE ---------------------------------------
  if(return.continuous.grid) {
    continuous.grid <- extrap.grid
  } else {
    continuous.grid <- NA
  }
  
  # Format breaks for plotting----------------------------------------------------------------------
  # Automatic break selection based on character vector.
  alt.round <- 0 # Set alternative rounding factor to zero based on user-specified breaks

  if(is.character(set.breaks[1])) {
    set.breaks <- tolower(set.breaks)

    # Set breaks ----
    break.vals <- classInt::classIntervals(x$cpue_kgkm2, n = 5, style = set.breaks)$brks

    # Setup rounding for small CPUE ----
    alt.round <- floor(-1*(min((log10(break.vals)-2)[abs(break.vals) > 0])))

    set.breaks <- c(-1, round(break.vals, alt.round))
  }

  # Ensure breaks go to zero------------------------------------------------------------------------
  if(min(set.breaks) > 0) {
    set.breaks <- c(0, set.breaks)
  }

  if(min(set.breaks) == 0) {
    set.breaks <- c(-1, set.breaks)
  }

  # Ensure breaks span the full range---------------------------------------------------------------
  if(max(set.breaks) < max(stn.predict$var1.pred)){
    set.breaks[length(set.breaks)] <- max(stn.predict$var1.pred) + 1
  }


  # Trim breaks to significant digits to account for differences in range among species-------------
  dig.lab <- 7
  set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)

  if(alt.round > 0) {
    while(dig.lab > alt.round) { # Rounding for small CPUE
      dig.lab <- dig.lab - 1
      set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
    }
  } else { # Rounding for large CPUE
    while(length(grep("\\.", set.levels)) > 0) {
      dig.lab <- dig.lab - 1
      set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
    }
  }
  
  # Cut extrapolation grid to support discrete scale------------------------------------------------
  extrap.grid$var1.pred <- cut(extrap.grid$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
  
  # # Which breaks need commas?-----------------------------------------------------------------------
  sig.dig <- round(set.breaks[which(nchar(round(set.breaks)) >= 4)])

  # Drop brackets, add commas, create 'No catch' level to legend labels-----------------------------
  make_level_labels <- function(vec) {
    vec <- as.character(vec)
    vec[grep("-1", vec)] <- "No catch"
    vec <- sub("\\(", "\\>", vec)
    vec <- sub("\\,", "–", vec)
    vec <- sub("\\]", "", vec)
    vec <- sub("-Inf", "", vec)
    if(length(sig.dig) > 3) {
      for(j in 1:length(sig.dig)) {
        vec <- sub(sig.dig[j], format(sig.dig[j], nsmall=0, big.mark=","), vec)
      }
    }
    return(vec)
  }
  
  # Assign level names to breaks for plotting-------------------------------------------------------
  extrap.grid$var1.pred <- factor(make_level_labels(extrap.grid$var1.pred),
                                  levels = make_level_labels(levels(set.levels)))
  
  # Number of breaks for color adjustments----------------------------------------------------------
  n.breaks <- length(levels(set.levels))
  
  # Make plot---------------------------------------------------------------------------------------
  
  if(extrapolation.grid.type %in% c("sf", "sf.simple")) {
    extrap.grid <- extrap.grid |>
      sf::st_as_sf() |>
      dplyr::select(-var1.var) |>
      dplyr::group_by(var1.pred) |>
      dplyr::summarise(n = n()) |>
      sf::st_intersection(map_layers$survey.area)
    
    extrap.grid <- extrap.grid |>
      dplyr::select(which(names(extrap.grid) %in% c("var1.pred", "n", "SURVEY", "geometry")))
    

    p1 <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = map_layers$survey.area, fill = NA) +
      ggplot2::geom_sf(data = extrap.grid,
                       aes(fill = var1.pred),
                       color = NA) +
      ggplot2::geom_sf(data = map_layers$survey.area, fill = NA) +
      ggplot2::geom_sf(data = map_layers$akland, fill = "grey80") +
      #ggplot2::geom_sf(data = map_layers$bathymetry) +
      ggplot2::geom_sf(data = map_layers$graticule, color = alpha("grey70", 0.3)) +
      ggplot2::scale_fill_manual(name = paste0(key.title, "\n", key.title.units),
                                 values = c("white", RColorBrewer::brewer.pal(9, name = "Blues")[c(2,4,6,8,9)]),
                                 na.translate = FALSE, # Don't use NA
                                 drop = FALSE) + # Keep all levels in the plot
      ggplot2::scale_x_continuous(breaks = map_layers$lon.breaks) +
      ggplot2::scale_y_continuous(breaks = map_layers$lat.breaks) +
      ggplot2::coord_sf(xlim = map_layers$plot.boundary$x,
                        ylim = map_layers$plot.boundary$y) +
      ggplot2::theme(panel.border = element_rect(color = "black", fill = NA),
                     panel.background = element_rect(fill = NA, color = "black"),
                     legend.key = element_rect(fill = NA, color = "grey70"),
                     legend.position = "bottom",
                     axis.title = element_blank(),
                     axis.text = element_text(size = 10),
                     legend.text = element_text(size = 10),
                     legend.title = element_text(size = 10),
                     plot.background = element_rect(fill = NA, color = NA))
  } else {
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = map_layers$survey.area, fill = NA) +
      stars::geom_stars(data = extrap.grid) +
      ggplot2::geom_sf(data = map_layers$survey.area, fill = NA) +
      ggplot2::geom_sf(data = map_layers$akland, fill = "grey80") +
      ggplot2::geom_sf(data = map_layers$bathymetry) +
      ggplot2::geom_sf(data = map_layers$graticule, color = alpha("grey70", 0.3)) +
      ggplot2::scale_fill_manual(name = paste0(key.title, "\n",  "key.title.units"),
                                 values = c("white", RColorBrewer::brewer.pal(9, name = "Blues")[c(2,4,6,8,9)]),
                                 na.translate = FALSE, # Don't use NA
                                 drop = FALSE) + # Keep all levels in the plot
      ggplot2::scale_x_continuous(breaks = map_layers$lon.breaks) +
      ggplot2::scale_y_continuous(breaks = map_layers$lat.breaks) +
      ggplot2::coord_sf(xlim = map_layers$plot.boundary$x,
                        ylim = map_layers$plot.boundary$y) +
      ggplot2::theme(panel.border = element_rect(color = "black", fill = NA),
                     panel.background = element_rect(fill = NA, color = "black"),
                     legend.key = element_rect(fill = NA, color = "grey70"),
                     legend.position = c(0.12, 0.18),
                     axis.title = element_blank(),
                     axis.text = element_text(size = 10),
                     legend.text = element_text(size = 10),
                     legend.title = element_text(size = 10),
                     plot.background = element_rect(fill = NA, color = NA))
  }
  
  # return(list(plot = p1,
  #             map_layers = map_layers,
  #             extrapolation.grid = extrap.grid,
  #             continuous.grid = continuous.grid,
  #             region = region,
  #             n.breaks = n.breaks,
  #             key.title = key.title,
  #             crs = out.crs))
  return(p1)
}