library(dplyr)
library(stringr)

# Setting up beer data for tables
complete_data <- readRDS("beer_data_loc_all.rds")

# Code for examining how many beers there are of each style
styles <- complete_data %>% 
  group_by(UT_sub_style) %>%
  count() %>%
  arrange(desc(n))

# Obtaining style names for input selection ALL features
styles$UT_sub_style <- as.character(styles$UT_sub_style)

styles[str_detect(styles$UT_sub_style, "Brown Ale"),][1]

styles[str_detect(styles$UT_sub_style, "IPA"),][1]

styles[str_detect(styles$UT_sub_style, "Lager"),][1]

styles[str_detect(styles$UT_sub_style, "Pale Ale"),][1]

styles[str_detect(styles$UT_sub_style, "Pilsner"),][1]

styles[str_detect(styles$UT_sub_style, "Porter"),][1]

styles[str_detect(styles$UT_sub_style, "Sour"),][1]

styles[str_detect(styles$UT_sub_style, "Stout"),][1]

# Code for examining how many beers there are of each style, by country
complete_data %>% 
  group_by(UT_sub_style, city, country) %>%
  count() %>%
  arrange(desc(n))

# Playing with renaming to make large_style section
porter_data <- complete_data %>%
  filter(UT_sub_style == "Porter - American" |
           UT_sub_style == "Porter - Other" |
           UT_sub_style == "Porter - Imperial / Double" |
           UT_sub_style == "Porter - Baltic" |
           UT_sub_style == "Porter - English") %>%
  mutate(large_style = "Porter")

brownAle_data <- complete_data %>%
  filter(UT_sub_style == "Brown Ale - American" | 
           UT_sub_style == "Brown Ale - English" |
           UT_sub_style == "Brown Ale - Imperial / Double" | 
           UT_sub_style == "Brown Ale - Belgian" | 
           UT_sub_style =="Brown  Ale - Other" ) %>%
  mutate(large_style = "Brown Ale")

IPA_data <- complete_data %>%
  filter(UT_sub_style == "IPA - American" |
           UT_sub_style == "IPA - Imperial / Double" |
           UT_sub_style == "IPA - Black / Cascadian Dark Ale" |
           UT_sub_style == "IPA - Session / India Session Ale" | 
           UT_sub_style == "IPA - English" |
           UT_sub_style == "IPA - Belgian" | 
           UT_sub_style == "IPA - Triple" |
           UT_sub_style == "Rye IPA" |
           UT_sub_style == "IPA - White"  | 
           UT_sub_style == "IPA - Imperial / Double Black" |
           UT_sub_style == "IPA - Red" | 
           UT_sub_style == "IPA - International" |
           UT_sub_style == "Sour - Farmhouse IPA") %>%
  mutate(large_style = "IPA")

lager_data <- complete_data %>%
  filter(UT_sub_style == "Lager Pale" |
           UT_sub_style == "Lager - North American Adjunct" |
           UT_sub_style == "Lager - American Light" |  
           UT_sub_style == "Lager - Helles"  |              
           UT_sub_style == "Lager - Vienna" |                
           UT_sub_style == "Lager - Dark" | 
           UT_sub_style ==  "Lager - Dortmunder / Export" |   
           UT_sub_style == "Lager - IPL (India Pale Lager)" |
           UT_sub_style == "Lager - American Amber / Red" |  
           UT_sub_style == "Lager - Dunkel Munich" |         
           UT_sub_style == "Lager - Japanese Rice" |         
           UT_sub_style == "Lager - Euro" |                  
           UT_sub_style == "Lager - Euro Dark" |             
           UT_sub_style == "Lager - Winter" |                
           UT_sub_style == "Lager - Euro Strong" |           
           UT_sub_style == "Lager - Amber" |                
           UT_sub_style == "Lager - Red") %>%
  mutate(large_style = "Lager")

paleAle_data <- complete_data %>%
  filter(UT_sub_style == "Pale Ale - American" |
           UT_sub_style == "Pale Ale - English" |
           UT_sub_style == "Pale Ale - Belgian" |
           UT_sub_style == "Pale Ale - Australian" |
           UT_sub_style == "Pale Ale - New Zealand" |
           UT_sub_style ==  "Pale Ale - International"
  ) %>%
  mutate(large_style = "Pale Ale")

pilsner_data <- complete_data %>%
  filter(UT_sub_style == "Pilsner - Other" |
           UT_sub_style == "Pilsner - German" |           
           UT_sub_style == "Pilsner - Czech" |       
           UT_sub_style == "Pilsner - Imperial / Double") %>%
  mutate(large_style = "Pilsner")

porter_data <- complete_data %>%
  filter(UT_sub_style == "Porter - American" |
           UT_sub_style == "Porter - Other" |
           UT_sub_style == "Porter - Imperial / Double" |
           UT_sub_style == "Porter - Baltic" |
           UT_sub_style == "Porter - English") %>%
  mutate(large_style = "Porter")

sour_data <- complete_data %>%
  filter(UT_sub_style == "Sour - American Wild Ale" |
           UT_sub_style == "Sour - Ale" |
           UT_sub_style == "Sour - Gose" |
           UT_sub_style == "Sour - Berliner Weisse" |
           UT_sub_style == "Sour - Gueuze" |
           UT_sub_style == "Sour - Flanders Red Ale" | 
           UT_sub_style == "Sour - Flanders Oud Bruin" |
           UT_sub_style == "Sour - Farmhouse IPA") %>%
  mutate(large_style = "Sour")

stout_data <- complete_data %>%
  filter(UT_sub_style == "Stout - American Imperial / Double" |
           UT_sub_style == "Stout - Russian Imperial" |
           UT_sub_style == "Stout - Milk / Sweet" |
           UT_sub_style == "Stout - Other" | 
           UT_sub_style == "Stout - American" |
           UT_sub_style == "Stout - Imperial / Double" | 
           UT_sub_style == "Stout - Oatmeal" |
           UT_sub_style == "Stout - Imperial Oatmeal" |
           UT_sub_style == "Stout - Irish Dry" |
           UT_sub_style == "Stout - Imperial Milk / Sweet" | 
           UT_sub_style == "Stout - Foreign / Export" |
           UT_sub_style == "Stout - Oyster") %>%
  mutate(large_style = "Stout")

large_style_data <- rbind(brownAle_data, IPA_data, lager_data, paleAle_data,
                                  pilsner_data, porter_data, sour_data, stout_data)
large_style_data <- select(large_style_data, UT_beer_name, large_style)

all_data <- left_join(complete_data, large_style_data, by="UT_beer_name")
