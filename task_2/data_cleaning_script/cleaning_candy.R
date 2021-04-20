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
  
  





  
