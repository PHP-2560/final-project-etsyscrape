library(dplyr)
library(countrycode)
library(RColorBrewer)

colnames(min_age_table) <- c("country", "age", "offage")

min_age_table$age <- as.numeric(gsub("[*]", "", min_age_table$age))

age_tbl <- min_age_table %>%
  select(country, age)

age_tbl$code <- countrycode(age_tbl$country, 'country.name', 'iso3c')

map <- joinCountryData2Map(age_tbl, joinCode = "ISO3", nameJoinColumn =
                     "code", nameCountryColumn = "country", 
                    suggestForFailedCodes = FALSE, mapResolution = "coarse", 
                    projection = NA, verbose = FALSE)



mapParams <- mapCountryData(map, nameColumnToPlot = "age", addLegend = FALSE, 
                            mapRegion = "world",
                            catMethod = c(16:21), missingCountryCol="dark grey", 
                            numCats = length(unique(map$age)), colourPalette = colourPalette, 
                            mapTitle = "", borderCol = "black")

colourPalette <- RColorBrewer::brewer.pal(5,"PuRd")



do.call( addMapLegend, c(mapParams, legendLabels= "all", legendWidth=0.5, legendMar = 2))


data(age_tbl)

saveRDS(age_tbl, "age_tbl.rds")

colourPalette <- brewer.pal(age_tbl$age, 'RdYlGn')
