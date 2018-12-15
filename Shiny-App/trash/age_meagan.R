library(ggmap)

colnames(min_age_table) <- c("country", "on premise", "off premise")
min_age_table$country <- as.character(min_age_table$country)
country_latlon <- as.data.frame(geocode(min_age_table$country, output = "latlon", source = "dsk"))

min_age_table$lat <- country_latlon[2]
min_age_table$lon <- country_latlon[1]

saveRDS(min_age_table, "min_age_table_unclean_latlong.rds")
