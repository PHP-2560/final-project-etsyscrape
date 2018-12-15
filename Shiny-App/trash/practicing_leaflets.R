library(leaflet)
library(dplyr)
library(leaflet.extras)

complete_data <- readRDS("beer_data_loc_all.rds")
data <- complete_data[1:25,]


m <-leaflet(options = leafletOptions(minZoom = 1, dragging = TRUE)) %>%
  addProviderTiles("CartoDB") %>%
  setView(lng = -52.7126, lat = 47.5615, zoom = 1)

m %>%
  # If we don't use setView and use addMarkers it will automatically zoom to include all marked points
  addMarkers(lng = data$lon, lat = data$lat, popup=data$UT_beer_name)

m %>%
  addCircleMarkers(lng = data$lon, lat = data$lat, popup=data$UT_beer_name, radius = 3, 
                   color = "yellow")

# Piping in the data to leaflet lets us use ~ instead of specificing the data frame all the time
# Map with circle markers for each beer, all the same colour
data %>%
  leaflet(options = leafletOptions(minZoom = 1, dragging = TRUE)) %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lon, 
                   lat = ~lat,
                   popup = ~paste0("<b>", UT_beer_name, "</b>", 
                                   "<br/>", UT_sub_style,
                                   "<br/>", UT_brewery),
                   radius = 3,
                   color = "green")

# Points coloured by rating
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0:5))

# Map with cluster indicators for beer locations
# Might be nice to have a "point" or "cluster" option in the Shiny App
complete_data %>%
  leaflet(options = leafletOptions(minZoom = 1, dragging = TRUE)) %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lon, 
                   lat = ~lat,
                   popup = ~paste0("<b>", UT_beer_name, "</b>", 
                                   "<br/>", UT_sub_style,
                                   "<br/>", UT_brewery,
                                   "<br/>", country),
                   radius = 3,
                   color = ~pal(UT_rating),
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>%



# Without clusters
complete_data %>%
  leaflet(options = leafletOptions(minZoom = 1, dragging = TRUE)) %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~lon, 
                   lat = ~lat,
                   popup = ~paste0("<b>", UT_beer_name, "</b>", 
                                   "<br/>", UT_sub_style,
                                   "<br/>", UT_brewery,
                                   "<br/>", country),
                   radius = 3,
                   color = ~pal(UT_rating)) %>%
  addLegend(pal = pal, 
            opacity = 0.5,
            values = c(0:5),
            title = "Untappd Rating",
            position = "bottomleft",
            bins = 5)




  

