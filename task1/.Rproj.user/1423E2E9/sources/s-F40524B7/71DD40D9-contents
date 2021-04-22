library(readr)
library(tidyverse)
library(janitor)

decath_data <- read_rds("raw_data/decathlon.rds")

# Changing row names into "Competitor variable"
 
decath_data_clean <- rownames_to_column(decath_data, "Competitor") %>% 
  clean_names()

# cleaning to long format, moving columns and changing column names

decath_data_clean <- decath_data_clean %>% 
  pivot_longer(cols = c(2:11), 
               names_to = "event",
               values_to = "result") %>% 
  rename(competiton_rank = rank, competition_points = points) %>% 
  relocate(event, result, .before = competiton_rank)

decath_clean <- decath_data_clean %>% 
  mutate(competitor = str_to_lower(competitor))
  

write_csv(decath_clean, "clean_data/decath_clean.csv")
  
