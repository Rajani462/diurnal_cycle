
line_colors <- c("#D41E00","#8C564B", "#009E73", "#0072B2", "#E69F00", "#F0E442", "#56B4E9", "#999999")



###to plot in robinson p[rojection----------------------------------------
# You can now work with data_df_robinson as a data frame with Robinson projection coordinates
load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

# Define Robinson projection
PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Project spatial data to Robinson projection
NE_countries_rob <- spTransform(NE_countries, CRSobj = PROJ)
NE_graticules_rob <- spTransform(NE_graticules, CRSobj = PROJ)
NE_box_rob <- spTransform(NE_box, CRSobj = PROJ)

# Project long-lat coordinates for graticule label data frames
prj.coord <- rgdal::project(cbind(lbl.Y$lon, lbl.Y$lat), proj = PROJ)
lbl.Y.prj <- cbind(prj.coord, lbl.Y)
names(lbl.Y.prj)[1:2] <- c("X.prj", "Y.prj")

prj.coord <- rgdal::project(cbind(lbl.X$lon, lbl.X$lat), proj = PROJ)
lbl.X.prj <- cbind(prj.coord, lbl.X)
names(lbl.X.prj)[1:2] <- c("X.prj", "Y.prj")



# Define a function to create a raster from a dataset(data.atble)
create_raster <- function(dataset_list) {
  #subset_df <- spat_freq_df[spat_freq_df$name == dataset_name, ]
  raster_layer <- rasterFromXYZ(dataset_list, crs = "+proj=longlat +datum=WGS84")
  return(raster_layer)
}

#############################################################################

## for arrowa at the end of colorbar-----------

library(ggplot2)
library(gtable)
library(grid)

my_triangle_colourbar <- function(...) {
  guide <- guide_colourbar(...)
  class(guide) <- c("my_triangle_colourbar", class(guide))
  guide
}



guide_gengrob.my_triangle_colourbar <- function(...) {
  # First draw normal colourbar
  guide <- NextMethod()
  # Extract bar / colours
  is_bar <- grep("^bar$", guide$layout$name)
  bar <- guide$grobs[[is_bar]]
  extremes <- c(bar$raster[1], bar$raster[length(bar$raster)])
  # Extract size
  width  <- guide$widths[guide$layout$l[is_bar]]
  height <- guide$heights[guide$layout$t[is_bar]]
  short  <- min(convertUnit(width, "cm",  valueOnly = TRUE),
                convertUnit(height, "cm", valueOnly = TRUE))
  # Make space for triangles
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar] - 1)
  guide <- gtable_add_rows(guide, unit(short, "cm"),
                           guide$layout$t[is_bar])
  
  # Draw triangles
  top <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(0, 1, 0), "npc"),
    gp = gpar(fill = extremes[1], col = NA)
  )
  bottom <- polygonGrob(
    x = unit(c(0, 0.5, 1), "npc"),
    y = unit(c(1, 0, 1), "npc"),
    gp = gpar(fill = extremes[2], col = NA)
  )
  # Add triangles to guide
  guide <- gtable_add_grob(
    guide, top, 
    t = guide$layout$t[is_bar] - 1,
    l = guide$layout$l[is_bar]
  )
  guide <- gtable_add_grob(
    guide, bottom,
    t = guide$layout$t[is_bar] + 1,
    l = guide$layout$l[is_bar]
  )
  
  return(guide)
}