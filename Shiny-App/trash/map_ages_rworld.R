
install.packages("rworldmap")
library(rworldmap)

theCountries <- shp_countries$id
# These are the ISO3 names of the countries you'd like to plot in red

malDF <- min_age_table
# malDF is a data.frame with the ISO3 country names plus a variable to
# merge to the map data

malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
# This will join your malDF data.frame to the country map data

mapCountryData(malMap, nameColumnToPlot="malaria", catMethod = "categorical",
               missingCountryCol = gray(.8), addLegend=FALSE )
# And this will plot it, with the trick that the color palette's first
# color is red

do.call( addMapLegend, c(malMap, legendWidth=0.5, legendMar = 2))

readRDS("beer_complete_data.rds")
