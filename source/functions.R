source('./source/libs.R')


# funnction ---------------------------------------------------------------

library(ncdf4)
library(stringr)

convertH5toNC <- function(h5file, output_dir) {
  ncpath <- output_dir
  ncname <- basename(h5file)
  ncname <- stringr::str_remove(ncname, "\\.h5$")
  nc_out <- file.path(ncpath, paste(ncname, ".nc", sep = ""))
  
  lon <- seq(-179.95, 179.95, 0.1)
  lat <- seq(-89.95, 89.95, 0.1) 
  #lat <- seq(89.95, -89.95, -0.1) 
  
  ncin <- ncdf4::nc_open(h5file)
  
  start_date_time <- stringr::str_extract(ncdf4::ncatt_get(ncin, 0)[4], "(?<=StartGranuleDateTime=)[^;]+")
  stop_date_time <- stringr::str_extract(ncdf4::ncatt_get(ncin, 0)[5], "(?<=StopGranuleDateTime=)[^;]+")
  
  time <- as.POSIXct(start_date_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  time_in_hours <- as.numeric(difftime(time, as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), units = "hours"))
  
  tp <- ncdf4::ncvar_get(ncin, "Grid/hourlyPrecipRateGC")
  tp[is.na(tp)] <- -9999
  
  deflon <- ncdf4::ncdim_def("lon", vals = lon, longname = "longitude", units = "degrees_east")
  deflat <- ncdf4::ncdim_def("lat", vals = lat, longname = "latitude", units = "degrees_north")
  deftime <- ncdf4::ncdim_def("time", vals = time_in_hours, longname = "time",
                              units = "hours since 1970-01-01 00:00:00",
                              calendar = "standard",
                              unlim = TRUE)
  
  deftp <- ncdf4::ncvar_def(name = "tp", units = "mm", 
                            list(deflat, deflon, deftime), 
                            missval = -9999,
                            compression = 4,
                            longname = "Total monthly precipitation",
                            prec = "float")
  
  ncoutput <- ncdf4::nc_create(nc_out, list(deftp), force_v4 = TRUE, verbose = FALSE)
  ncdf4::ncvar_put(ncoutput, deftp, tp)
  
  ncdf4::ncatt_put(ncoutput, "lon", "axis", "X") 
  ncdf4::ncatt_put(ncoutput, "lat", "axis", "Y")
  ncdf4::ncatt_put(ncoutput, "time", "axis", "T")
  
  ncdf4::nc_close(ncoutput)
}


# imerg -------------------------------------------------------------------
# convert the imerg nc4 files into data.table format

imerg_nc_dtable <- function(file){
  imer <- ncdf4::nc_open(file)
  lon <- ncdf4::ncvar_get(imer, "lon")
  lat <- ncdf4::ncvar_get(imer, "lat")
  rf.array <-  ncdf4::ncvar_get(imer, "precipitationCal")
  
  nc_atts <- ncdf4::ncatt_get(imer, 0)
  date_time_start <- as.POSIXlt(nc_atts$BeginDate)
  
  dimnames(rf.array)[[1]] <- imer$dim$lat$vals
  dimnames(rf.array)[[2]] <- imer$dim$lon$vals
  
  ncdf4::nc_close(imer)
  
  #rf.array_tropic <- rf.array[700:1100, ] #-20S to 19.95N & -180 to 180 (adjust it according to the needs)
  rf.array_tropic <- rf.array[650:1150, ] #--25.05S to 24.95N
  precip_summary <- data.table::data.table(reshape2::melt(rf.array_tropic,
                                                          varnames = c("lat", "lon"),
                                                          value.name = "precipitation"))
  
  
  precip_summary2 <- cbind(precip_summary, date_time_start)
  
  return(precip_summary2)
  
}





# generate seasonal precipitation (sum) from monthly ----------------------

add_seasons <- function(dt){
  dt[month == 12 | month == 1 | month == 2, season := 'winter']
  dt[month == 3 | month == 4 | month == 5, season := 'spring']
  dt[month == 6 | month == 7 | month == 8, season := 'summer']
  dt[month == 9 | month == 10 | month == 11, season := 'autumn']
  dt[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]
  
}


# create colorscale -------------------------------------------------------

plot_discrete_cbar = function(
    breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
    palette = "Greys", # RColorBrewer palette to use
    colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette), # Alternatively, manually set colors
    direction = 1, # Flip colors? Can be 1 or -1
    spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
    border_color = NA, # NA = no border color
    legend_title = NULL,
    legend_direction = "horizontal", # Can be "horizontal" or "vertical"
    font_size = 5,
    expand_size = 1, # Controls spacing around legend plot
    spacing_scaling = 1, # Multiplicative factor for label and legend title spacing
    width = 0.1, # Thickness of color bar
    triangle_size = 0.1 # Relative width of +-Inf triangles
) {
  require(ggplot2)
  if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
  if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
  if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
  breaks = as.numeric(breaks)
  new_breaks = sort(unique(breaks))
  if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
  breaks = new_breaks
  if (class(colors) == "function") colors = colors(length(breaks) - 1)
  if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
  if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")
  
  if (direction == -1) colors = rev(colors)
  
  inf_breaks = which(is.infinite(breaks))
  if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
  plotcolors = colors
  
  n_breaks = length(breaks)
  
  labels = breaks
  
  if (spacing == "constant") {
    breaks = 1:n_breaks
  }
  
  r_breaks = range(breaks)
  
  cbar_df = data.frame(stringsAsFactors = FALSE,
                       y = breaks,
                       yend = c(breaks[-1], NA),
                       color = as.character(1:n_breaks)
  )[-n_breaks,]
  
  xmin = 1 - width/2
  xmax = 1 + width/2
  
  cbar_plot = ggplot(cbar_df, aes(xmin=xmin, xmax = xmax, ymin = y, ymax = yend, fill = factor(color, levels = 1:length(colors)))) +
    geom_rect(show.legend = FALSE,
              color=border_color)
  
  if (any(inf_breaks == 1)) { # Add < arrow for -Inf
    firstv = breaks[1]
    polystart = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-1]
    cbar_plot = cbar_plot +
      geom_polygon(data=polystart, aes(x=x, y=y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[1],
                   color=border_color)
  }
  if (any(inf_breaks > 1)) { # Add > arrow for +Inf
    lastv = breaks[n_breaks]
    polyend = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-length(plotcolors)]
    cbar_plot = cbar_plot +
      geom_polygon(data=polyend, aes(x=x, y=y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[length(colors)],
                   color=border_color)
  }
  
  if (legend_direction == "horizontal") { #horizontal legend
    mul = 1
    x = xmin
    xend = xmax
    cbar_plot = cbar_plot + coord_flip()
    angle = 0
    legend_position = xmax + 0.1 * spacing_scaling
  } else { # vertical legend
    mul = -1
    x = xmax
    xend = xmin
    angle = -90
    legend_position = xmax + 0.2 * spacing_scaling
  }
  
  cbar_plot = cbar_plot +
    geom_segment(data=data.frame(y = breaks, yend = breaks),
                 aes(y=y, yend=yend),
                 x = x - 0.05 * mul * spacing_scaling, xend = xend,
                 inherit.aes = FALSE) +
    annotate(geom = 'text', x = x - 0.1 * mul * spacing_scaling, y = breaks,
             label = labels,
             size = font_size) +
    scale_x_continuous(expand = c(expand_size,expand_size)) +
    scale_fill_manual(values=plotcolors) +
    theme_void()
  
  if (!is.null(legend_title)) { # Add legend title
    cbar_plot = cbar_plot +
      annotate(geom = 'text', x = legend_position, y = mean(r_breaks),
               label = legend_title,
               angle = angle,
               size = font_size)
  }
  
  cbar_plot
}

########################################################################################
