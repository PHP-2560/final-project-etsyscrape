library(dplyr)
library(countrycode)
library(RColorBrewer)

colourPalette <- RColorBrewer::brewer.pal(5,"PuRd")

age_tbl <- readRDS("age_tbl.rds")

map <- joinCountryData2Map(age_tbl, joinCode = "ISO3", nameJoinColumn =
                     "code", nameCountryColumn = "country", 
                    suggestForFailedCodes = FALSE, mapResolution = "coarse", 
                    projection = NA, verbose = FALSE)


mapParams <- mapCountryData(map, nameColumnToPlot = "age", addLegend = FALSE, 
                            mapRegion = "world",
                            catMethod = c(16:21), missingCountryCol="dark grey", 
                            numCats = length(unique(map$age)), colourPalette = colourPalette, 
                            mapTitle = "", borderCol = "black")

do.call(addMapLegend, c(mapParams, legendLabels= "all", legendWidth=0.5, legendMar = 2))
