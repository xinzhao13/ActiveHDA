rm(list = ls())

require(leaflet)
require(tidyverse)
require(leaflet.providers)
require(lubridate)
require(XML)
require(raster)
require(plotKML)

# Reset working directory
setwd("C:/Xin/Studies/MSc_HDA/02_Misc/05_Running_Route/Route")

# List all gpx files in folder
files <- list.files(".", pattern = "*.gpx")

# Loop through all gpx files
agg_gpx <- sapply(files, function(f) {
  readGPX(f)$tracks[[1]]
  
})

names(agg_gpx)

# Define function for distance calculation
# Reference: https://rpubs.com/ials2un/gpx1
shift_vec <- function (vec, shift) {
  if (length(vec) <= abs(shift)) {
    rep(NA , length(vec))
  } else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)])
    }
    else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}

plot_one <- function(data) {
  
  # Extract data frame
  data<-data %>% as.data.frame(.)
  
  # Shift coordinates to previous
  data$lat_p1 <- shift_vec(data$lat,-1)
  data$lon_p1 <- shift_vec(data$lon,-1)
  
  # Calculate distance to previous point using pointDistance function
  # Reference: https://rpubs.com/ials2un/gpx1
  data$dist_p1 <- apply(
    data,
    1,
    FUN = function (row) {
      pointDistance(c(as.numeric(row["lat_p1"]),
                      as.numeric(row["lon_p1"])),
                    c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                    lonlat = T)
    }
  )
  
  head(data)
  
  # Calculate total distance in km
  dist_tot <-
    (sum(data$dist_p1, na.rm = TRUE) / 1000) %>% round(., 1)
  dist_tot
  
  # Reformat time
  data$time_adj <-
    strptime(data$time, format = "%Y-%m-%dT%H:%M:%OS")
  head(data$time_adj)
  
  # Calculate time taken
  time_obj <-
    difftime(data$time_adj[nrow(data)], data$time_adj[1], units="auto") 
  
  # Calculate time in numerical terms
  time_tot <- as.numeric(time_obj) %>% round(.,1)

  # Extract the unit of time
  time_unit<- units(time_obj)
  
  # Remove first and last 5 rows
  data<-data[-c(1:5),]
  data<-data[1:(dim(data)-5),]
  
  # # Make text for popup
  # pop_text <- paste(time_tot, dist_tot, sep = "\n")
  
  # Map the route
  map <- leaflet() %>%
    setView(lng = median(data$lon),
            lat = median(data$lat),
            zoom = 13) %>%
    addTiles() %>% addProviderTiles("CartoDB.Positron") %>%
    addPolylines(
      lng = data$lon,
      lat = data$lat,
      color = "#C70039",
      weight = 3,
      label = paste0("Distance: ", dist_tot, "km, Time: ", time_tot, " ",time_unit))
      # popupOptions = popupOptions(
      #   autoPan = TRUE,
      #   maxWidth = 400,
      #   closeonClick = TRUE
      # )

  map
}


# # Read in the tracks data from gpx file
# df <- readGPX("Battersea_Park.gpx")$tracks[[1]][[1]] %>%
#   as.data.frame(.)

df <- agg_gpx$Lunch_Run.gpx

plot_one(df)

