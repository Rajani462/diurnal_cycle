library(raster)
library(data.table)
library(dplyr)

#define my custom function for estimating the mean intensity, frequency, and mean precip for differnt threshold
# mean
# Define my custom function for estimating the mean intensity, frequency, and mean precip for different thresholds
# mean
threshold_mean <- function(x, threshold, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x[x < as.single(threshold)] <- 0
  if (length(x) > 0) {
    return(mean(x))
  } else {
    return(NA_real_)
  }
}

# Intensity
threshold_intensity <- function(x, threshold, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x[x < as.single(threshold)] <- 0
  x_threshold <- x[x >= as.single(threshold)]
  if (length(x_threshold) > 0) {
    return(sum(x_threshold) / length(x_threshold))
  } else {
    return(NA_real_)
  }
}

# Frequency (%)
threshold_frequency <- function(x, threshold, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x_threshold <- x[x >= as.single(threshold)]
  if (length(x_threshold) > 0) {
    return(length(x_threshold) / length(x) * 100)
  } else {
    return(0)  # Set frequency to 0 if no values exceed the threshold
  }
}


######

convert_to_dt <- function(x) {
  brick_name <- deparse(substitute(x))
  dataset_name <- strsplit(brick_name, "_")[[1]][1]
  variable_name <- strsplit(brick_name, "_")[[1]][2]
  threshold_name <- sub(".*_", "", brick_name)
  
  dt <- as.data.frame(x, xy = TRUE) %>%
    as.data.table() %>%
    melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
    `[`(, name := as.factor(dataset_name)) %>%
    `[`(, variable := as.factor(variable_name)) %>%
    `[`(, threshold := as.factor(as.character(threshold_name)))
  
  return(dt)
}



# convert_to_dt <- function(x) {
#   brick_name <- deparse(substitute(x))
#   dataset_name <- strsplit(brick_name, "_")[[1]][1]
#   variable_name <- strsplit(brick_name, "_")[[1]][2]
#   threshold_name <- strsplit(brick_name, "_")[[1]][3]
#   
#   dt <- as.data.frame(x, xy = TRUE) %>%
#     as.data.table() %>%
#     melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
#     `[`(, name := factor(dataset_name)) %>%
#     `[`(, variable := factor(variable_name)) %>%
#     `[`(, threshold := factor(threshold_name))
#   
#   return(dt)
# }

# convert_to_dt <- function(x) {
#   brick_name <- deparse(substitute(x))
#   
#   # Extract dataset name
#   dataset_name <- strsplit(brick_name, "_")[[1]][1]
#   
#   # Extract variable name
#   variable_name <- ""
#   if (length(strsplit(brick_name, "_")[[1]]) >= 2) {
#     variable_name <- strsplit(brick_name, "_")[[1]][2]
#   }
#   
#   # Extract threshold
#   threshold_name <- ""
#   if (length(strsplit(brick_name, "_")[[1]]) >= 3) {
#     threshold_name <- strsplit(brick_name, "_")[[1]][3]
#   }
#   
#   dt <- as.data.frame(x, xy = TRUE) %>%
#     as.data.table() %>%
#     melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
#     `[`(, name := factor(dataset_name)) %>%
#     `[`(, variable := factor(variable_name)) %>%
#     `[`(, threshold := factor(threshold_name))
#   
#   return(dt)
# }
