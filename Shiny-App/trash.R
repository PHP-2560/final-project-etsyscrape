
library(dplyr)
countries <- geojsonio::geojson_read("world.geo.json/countries.geo.json",
                                      what = "sp")

countries@data
# Or use the rgdal equivalent:
# nycounties <- rgdal::readOGR("json/nycounties.geojson", "OGRGeoJSON")

min_age_table$lat <- as.vector(min_age_table$lat)
min_age_table$lon <- as.vector(min_age_table$lon)

mydf <- sp::merge(countries, min_age_table, by = "name", all=F)

min_age_table$country

shp_countries <- countries@data %>% 
  left_join(min_age_table, by = c("name" = "country"))

countries %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 1,
              color = "grey",
              #label = ~paste0("Total Income: " dollar(income)),
              highlight = highlightOptions(weight = 3,
                                           color = "red",
                                           bringToFront = TRUE))
