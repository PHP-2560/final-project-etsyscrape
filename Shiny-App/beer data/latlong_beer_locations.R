# This code finds the latitude and longitude for the location of each beer. 

# Some beer locations are identified directly by brewery location, using a location variable 
#   created by pasting together the brewery name with the UT_loc.

# This character string representing the brewery location is then passed to the geocode() function,
#   which returns the lat/long values. These values are then added to the existing data frame. 

# This process does not work for some beers, as the brewery location cannot be found by geocodes(). 

# In these cases, the beer location is found using the UT_loc data only (searching by city/country,
#   not by brewery). 

library(dplyr)
library(ggmap)

# Splitting up beer_data_clean to obtain lat/long data over multiple pulls
# Getting the locations of ~2500 beers at a time

# Create a variable containing the entire location - the brewery name and location
# from original data frame
beer_data_clean <- beer_data_clean %>%
  mutate(location_string = paste(UT_brewery, UT_loc, sep=" "))

# 1 obtaining lat/long and adding to dataframe
beer_data_1 <- beer_data_clean[1:2499,]
locations1 <- as.data.frame(geocode(beer_data_1$location_string, output = "latlon", source = "dsk"))
beer_data_1 <- cbind(beer_data_1, locations1)

# 2 obtaining lat/long and adding to dataframe
beer_data_2 <- beer_data_clean[2500:4998,]
locations2 <- as.data.frame(geocode(beer_data_2$location_string, output = "latlon", source = "dsk"))
beer_data_2 <- cbind(beer_data_2, locations2)

# 3 obtaining lat/long and adding to dataframe
beer_data_3 <- beer_data_clean[4999:7497,]
locations3 <- as.data.frame(geocode(beer_data_3$location_string, output = "latlon", source = "dsk"))
beer_data_3 <- cbind(beer_data_3, locations3)

# 4 obtaining lat/long and adding to dataframe
beer_data_4 <- beer_data_clean[7498:8467,]
locations4 <- as.data.frame(geocode(beer_data_4$location_string, output = "latlon", source = "dsk"))
beer_data_4 <- cbind(beer_data_4, locations4)

# Combine back together with lat/long data included (for non-error locations by BREWERY)
beer_data_clean_loc <- rbind(beer_data_1, beer_data_2, beer_data_3, beer_data_4)

# Create new variable indicating which beers still have no lat/long data
latlon_missing <- is.na(beer_data_clean_loc$lat)
beer_data_clean_loc$missing <- latlon_missing

# Filter and save the locations that are complete with location by brewery
beer_data_clean_loc_bybrewery <- beer_data_clean_loc %>%
  filter(missing == FALSE) %>%
  select(-missing)

# Save beers with locations identified by brewery
saveRDS(beer_data_clean_loc_bybrewery, file = "beer_data_loc_bybrewery.rds")

# Filter locations that have no lat/long from brewery search
beer_data_clean_loc_bycity <- beer_data_clean_loc %>%
  filter(missing == TRUE) %>%
  select(-lat, -lon, -missing)

# Find beer locations by UT_loc (city/country data)
city_locations <- as.data.frame(geocode(beer_data_clean_loc_bycity$UT_loc, output = "latlon", source = "dsk"))
beer_data_clean_loc_bycity <- cbind(beer_data_clean_loc_bycity, city_locations)

# Save beers with locations identified by city
saveRDS(beer_data_clean_loc_bycity, file = "beer_data_loc_bycity.rds")

# Save complete lat/long data for all beers
beer_data_clean_all_locations <- rbind(beer_data_clean_loc_bybrewery, beer_data_clean_loc_bycity)
saveRDS(beer_data_clean_all_locations, file = "beer_data_loc_all.rds")
