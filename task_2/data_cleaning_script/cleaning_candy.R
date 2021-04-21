library(readxl)
library(tidyverse)
library(stringr)

candy_2015 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")

# Cleaning and lengthening 2015


colnames(candy_2015)[1] = "[timestamp]"
colnames(candy_2015)[2] = "[age]"
colnames(candy_2015)[3] = "[going_out]"


cleanish_2015 <- candy_2015[, grep("^(\\[)", names(candy_2015))]

cleanish_2015[, "long_id"] <- c(1:5630)

long_2015 <- cleanish_2015 %>% 
  pivot_longer(cols = (4:98), 
               names_to = "candy_type",
               values_to = "rating")


long_2015 <- long_2015 %>% 
  mutate(candy_type = str_remove_all(candy_type, "[\\[|\\]]"))

long_2015$"[timestamp]" <- NULL

colnames(long_2015)[1] = "age"
colnames(long_2015)[2] = "going_out"


long_2015 <- long_2015 %>% 
  add_column(country = NA, .after = "long_id") %>% 
  add_column(state_province = NA, .after = "country") %>% 
  add_column(gender = NA, .after = "age")


long_2015[, "source"] <- "2015"


# Cleaning and lengthening 2016


colnames(candy_2016)[1] = "[timestamp]"
colnames(candy_2016)[2] = "[going_out]"
colnames(candy_2016)[3] = "[gender]"
colnames(candy_2016)[4] = "[age]"
colnames(candy_2016)[5] = "[country]"
colnames(candy_2016)[6] = "[state/province]"

cleanish_2016 <- candy_2016[, grep("^(\\[)", names(candy_2016))]

even_cleaner_2016 <- cleanish_2016[, 1:(length(cleanish_2016) -1) ]

even_cleaner_2016[, "long_id"] <- c(5631:6889)

long_2016 <- even_cleaner_2016 %>% 
  pivot_longer(cols = (7:106), 
               names_to = "candy_type",
               values_to = "rating")


long_2016 <- long_2016 %>% 
  mutate(candy_type = str_remove_all(candy_type, "[\\[|\\]]"))

long_2016$"[timestamp]" <- NULL
long_2016$`[state/province]` <- NULL

colnames(long_2016)[1] = "going_out"
colnames(long_2016)[2] = "gender"
colnames(long_2016)[3] = "age"
colnames(long_2016)[4] = "country"


long_2016[, "source"] <- "2016"


# Cleaning and lengthening 2017

cleaner_2017 <- candy_2017 %>% 
  select(matches('Q1:|Q2:|Q3:|Q4:|Q5:|Q6'))

names(cleaner_2017) <- substring(names(cleaner_2017), 5)


cleaner_2017[, "long_id"] <- c(6890:9349)

long_2017 <- cleaner_2017 %>% 
  pivot_longer(cols = (6:108), 
               names_to = "candy_type",
               values_to = "rating")

long_2017$`STATE, PROVINCE, COUNTY, ETC` <- NULL

colnames(long_2017)[1] = "going_out"
colnames(long_2017)[2] = "gender"
colnames(long_2017)[3] = "age"
colnames(long_2017)[4] = "country"


long_2017[, "source"] <- "2017"


# Binding rows

ready_to_merge_2015 <- long_2015 %>% 
  select(long_id, age, gender, going_out, country, candy_type, rating, source)

ready_to_merge_2016 <- long_2016 %>% 
  select(long_id, age, gender, going_out, country, candy_type, rating, source)

ready_to_merge_2017 <- long_2017 %>% 
  select(long_id, age, gender, going_out, country, candy_type, rating, source)

candy_all_years <- bind_rows(ready_to_merge_2015, ready_to_merge_2016, ready_to_merge_2017)

candy_all_years <- candy_all_years %>% 
  mutate(country = str_to_lower(country))


candy_all_years <- candy_all_years %>% 
  mutate(total_NAs = apply(., MARGIN = 1, function(x) sum(is.na(x))))

candy_all_years <- candy_all_years %>% 
  filter(total_NAs != 5)

candy_all_years %>% 
  count(country)

candy_all_countries <- candy_all_years

all_countries <- candy_all_countries %>% 
  count(country)



candy_all_countries <- candy_all_years %>% 
  mutate(
    country = case_when(
      str_detect(country, pattern_uk) ~ "uk",
      str_detect(country, pattern_us) ~ "usa",
      str_detect(country, pattern_canada) ~ "canada",
      TRUE ~ "Other"
    )
  )


pattern_us <- "'merica|merica|america|usa|us|u.s.|us|state|u s|u.s.a|trump|united|murica|murrika
                |amerca|new york|california|new jersey|alaska|yoo ess"
pattern_uk <- "endland|england|scotland|dom|u.k.|ireland"

pattern_canada <- "canada|canada`"
