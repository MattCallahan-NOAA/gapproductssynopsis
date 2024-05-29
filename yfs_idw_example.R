library(akgfmaps)
yfsbreaks <- round(classInt::classIntervals(YFS2017$CPUE_KGHA, n = 5, style = "jenks")$brks, 0)

opt1 <- make_idw_map(x = YFS2017, # Pass data as a data frame
                     region = "bs.all", # Predefined bs.all area
                     set.breaks = yfsbreaks, # Gets Jenks breaks from classint::classIntervals()
                     in.crs = "+proj=longlat", # Set input coordinate reference system
                     out.crs = "EPSG:3338", # Set output coordinate reference system
                     grid.cell = c(20000, 20000), # 20x20km grid
                     key.title = "yellowfin sole") # Include yellowfin sole in the legend title
opt1

