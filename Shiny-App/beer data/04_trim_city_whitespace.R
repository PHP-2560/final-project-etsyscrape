library(dplyr)

data_now <- readRDS("beer_data_loc_all.rds")

data_now$city <- trimws(as.character(data_now$city))

saveRDS(data_now, "beer_data_loc_all.rds")
