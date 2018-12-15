
library(rvest)
library(magrittr)

# # minimum drinking age - website 1
# url <- "https://drinkingage.procon.org/view.resource.php?resourceID=004294"
# 
# DA_age_table <- read_html(url)
# 
# DA_min_age_table <- DA_age_table %>%
#   html_nodes(".td") %>%
#   .[11] %>%
#   html_table(header=T, fill = T)
# DA_min_age_table
# 
# DA_min_age_table2 <- html_nodes(DA_age_table, '.newblue-content-text')
# DA_min_age_table2
# 
# # minimum drinking age - website 2
# url2 <- "https://www.alcoholproblemsandsolutions.org/LegalDrinkingAge.html"
# 
# APS_age_table <- read_html(url2)
# 
# APS_min_age_table <- APS_age_table %>%
#   html_nodes(".table") %>%
#   .[1] %>%
#   html_table(header=T, fill = T)
# 
# APS_min_age_table
# 
# # minimum drinking age - website 3
# url3 <- "https://en.wikipedia.org/wiki/Legal_drinking_age"
# 
# wiki_age <- read_html(url3)
# 
# africa_age <- wiki_age
# 
# # minimum drinking age - website 4
# url4 <- "https://www.tripsavvy.com/alcohol-drinking-ages-around-the-world-3150465"
# 
# tripsavvy <- read_html(url4)
# 
# ts_age <- tripsavvy %>%
#   html_nodes("p") %>%
#   html_text()
# 
# ts_age_list <- as.data.frame(ts_age)
# 
# ts_age_list
# 
# # minimum drinking age - website 5
# url5 <- "https://ipfs.io/ipfs/QmXoypizjW3WknFiJnKLwHCnL72vedxjQkDDP1mXWo6uco/wiki/Legal_drinking_age.html"
# 
# wiki_age2 <- read_html(url5)
# 
# wiki_age_africa <- wiki_age2 %>%
#   html_nodes()

# minimum drinking age - website 6
# this one worked and will be used for shiny app
url6 <- "http://www.iard.org/resources/minimum-legal-age-limits/"

IARD <- read_html(url6)

country_table <- as.data.frame(IARD_country <- IARD %>%
  html_nodes(".column-1") %>%
  html_text())

onpremise_table <- as.data.frame(IARD_onpremise <- IARD %>%
  html_nodes(".column-2") %>%
  html_text())

offpremise_table <- as.data.frame(IARD_offpremise <- IARD %>%
  html_nodes(".column-3") %>%
  html_text())

IARD_age_table <- cbind(country_table, onpremise_table, offpremise_table)
saveRDS(IARD_age_table, "/age data/min_age_table.rds" )

