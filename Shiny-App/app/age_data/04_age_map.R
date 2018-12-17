library(dplyr)
library(countrycode)
library(RColorBrewer)
library(rworldmap)

# Choosing colour palette from RColorBrewer
colourPalette <- brewer.pal(6,"PuRd")

# Making the map using the age_tbl data, read in
age_tbl <- readRDS("age_tbl.rds")

# Initiating process to write map to file
png(filename="age_map.png")

# Map object is created from the age_tbl, recognizes the countries by the code column,
# because they are identified as "ISO3".
map <- joinCountryData2Map(age_tbl, joinCode = "ISO3", nameJoinColumn =
                     "code", nameCountryColumn = "country", 
                    suggestForFailedCodes = FALSE, mapResolution = "coarse", 
                    projection = NA, verbose = FALSE)

# Specify the parameters for creating the map - using the age column as the plotting 
# variable, the entire world as the region, with NA age countries coloured dark grey
mapParams <- mapCountryData(map, nameColumnToPlot = "age", addLegend = FALSE, 
                            mapRegion = "world",
                            catMethod = c(16:21), missingCountryCol="dark grey", 
                            numCats = length(unique(map$age)), colourPalette = colourPalette, 
                            mapTitle = "", borderCol = "black")

# Add a custom legend to the map
do.call(addMapLegend, c(mapParams, legendLabels= "all", legendWidth=0.5,
                        legendMar = 2))

# Writing plot output to file
dev.off()


