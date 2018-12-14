library(dplyr)
library(countrycode)
colnames(min_age_table) <- c("country", "age", "offage")

min_age_table$age <- as.numeric(gsub("[*]", "", min_age_table$age))

age_tbl <- min_age_table %>%
  select(country, age)

age_tbl$code <- countrycode(age_tbl$country, 'country.name', 'iso3c')

map <- joinCountryData2Map(age_tbl, joinCode = "ISO3", nameJoinColumn =
                     "code", nameCountryColumn = "country", 
                    suggestForFailedCodes = FALSE, mapResolution = "coarse", 
                    projection = NA, verbose = FALSE)

mapCountryData( map)

data(age_tbl)
sPDF <- joinCountryData2Map(age_tbl, joinCode = "code", nameJoinColumn = "country")
