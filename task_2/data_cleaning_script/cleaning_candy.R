library(readxl)
library(tidyverse)
library(stringr)

candy_2015 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")



# 2015 --------------------------------------------------------------------------------------------------------

# renaming columns I want to keep 

colnames(candy_2015)[1] = "[timestamp]"
colnames(candy_2015)[2] = "[age]"
colnames(candy_2015)[3] = "[going_out]"

#  pulling out all the columns that have []

cleanish_2015 <- candy_2015[, grep("^(\\[)", names(candy_2015))]

# adding a ID column

cleanish_2015[, "long_id"] <- c(1:5630)

# pivoting longer

long_2015 <- cleanish_2015 %>% 
  pivot_longer(cols = (4:98), 
               names_to = "candy_type",
               values_to = "rating")

# removing [] from the data in candy_type column

long_2015 <- long_2015 %>% 
  mutate(candy_type = str_remove_all(candy_type, "[\\[|\\]]"))

# removing timestamo column

long_2015$"[timestamp]" <- NULL

# renaming columns to remove []

colnames(long_2015)[1] = "age"
colnames(long_2015)[2] = "going_out"

# adding columns that appear in other tables

long_2015 <- long_2015 %>% 
  add_column(country = NA, .after = "long_id") %>% 
  add_column(state_province = NA, .after = "country") %>% 
  add_column(gender = NA, .after = "age")

# adding column to identify which dataset it has come from 

long_2015[, "source"] <- "2015"

ready_to_merge_2015 <- long_2015 %>% 
  select(long_id, age, gender, going_out, country, candy_type, rating, source)

# 2016 -------------------------------------------------------------------------------------------------------
# Cleaning and lengthening 2016
candy_2016 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")

colnames(candy_2016)[1] = "[timestamp]"
colnames(candy_2016)[2] = "[going_out]"
colnames(candy_2016)[3] = "[gender]"
colnames(candy_2016)[4] = "[age]"
colnames(candy_2016)[5] = "[country]"
colnames(candy_2016)[6] = "[state/province]"

cleanish_2016 <- candy_2016[, grep("^(\\[)", names(candy_2016))]

# removing last column

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

ready_to_merge_2016 <- long_2016 %>% 
  select(long_id, age, gender, going_out, country, candy_type, rating, source)

# 2017 ----------------------------------------------------------------------------------------------------------

candy_2017 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")

cleaner_2017 <- candy_2017 %>% 
  select(matches('Q1:|Q2:|Q3:|Q4:|Q5:|Q6'))

# Removing the questions markers from ecah column

names(cleaner_2017) <- substring(names(cleaner_2017), 5)

# Adding ID column 

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

ready_to_merge_2017 <- long_2017 %>% 
  select(long_id, age, gender, going_out, country, candy_type, rating, source)

# Binding rows --------------------------------------------------------------------------------------------



# combing rows

candy_all_years <- bind_rows(ready_to_merge_2015, ready_to_merge_2016, ready_to_merge_2017)

# all countries and candy_type to lower case

candy_all_years <- candy_all_years %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(candy_type = str_to_lower(candy_type))

# calculating number of NAs across rows to find cases where no question has been answered

candy_all_years <- candy_all_years %>% 
  mutate(total_NAs = apply(., MARGIN = 1, function(x) sum(is.na(x))))

# removing any rows where no questions have been answered

candy_all_years <- candy_all_years %>% 
  filter(total_NAs != 5)

# creating patterns for all the different ways people have specifiefd America, Canada or UK

pattern_us <- "'merica|merica|america|usa|us|u.s.|us|state|u s|u.s.a|trump|united|murica|murrika
                |amerca|new york|california|new jersey|alaska|yoo ess"
pattern_uk <- "endland|england|scotland|dom|u.k.|ireland"

pattern_canada <- "canada|canada`"


candy_all_countries <- candy_all_years %>% 
  mutate(
    country = case_when(
      str_detect(country, pattern_uk) ~ "uk",
      str_detect(country, pattern_us) ~ "usa",
      str_detect(country, pattern_canada) ~ "canada",
      TRUE ~ "Other"
    )
  )


test_candy <- candy_all_countries %>% 
  mutate(gender = as.character(gender)) %>% 
  mutate(age = as.numeric(age))

clean_candy <- test_candy



write_csv(clean_candy, "clean_data/clean_candy.csv")


  

