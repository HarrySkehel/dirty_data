library(readxl)
library(tidyverse)
library(stringr)

candy_2015 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")

colnames(candy_2015)[1] = "[timestamp]"
colnames(candy_2015)[2] = "[age]"
colnames(candy_2015)[3] = "[going_out]"

cleanish_2015 <- candy_2015[, grep("^(\\[)", names(candy_2015))]

long_2015 <- cleanish_2015 %>% 
  pivot_longer(cols = (4:98), 
               names_to = "candy_type",
               values_to = "rating")
  
  
colnames(candy_2016)[1] = "[timestamp]"
colnames(candy_2016)[2] = "[going_out]"
colnames(candy_2016)[3] = "[gender]"
colnames(candy_2016)[4] = "[age]"
colnames(candy_2016)[5] = "[country]"
colnames(candy_2016)[6] = "[state/province]"

cleanish_2016 <- candy_2016[, grep("^(\\[)", names(candy_2016))]

even_cleaner_2016 <- cleanish_2016[, 1:(length(cleanish_2016) -1) ]

long_2016 <- even_cleaner_2016 %>% 
  pivot_longer(cols = (7:106), 
               names_to = "candy_type",
               values_to = "rating")


  
