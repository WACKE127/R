library(reshape)
library(tidyverse)

setwd("../../mlba/mlba/data-raw")
bostonHousing.df <- read.csv("BostonHousing.csv.gz") %>%
mutate(RM.bin = cut(RM, c(1:9),
       labels = FALSE))

bostonHousing.df %>%
    group_by(RM.bin, CHAS) %>%
    summarize(mean = mean(MEDV)) %>%
    spread(CHAS, mean)

### OUTPUT

# `summarise()` has grouped output by 'RM.bin'. You can override using the `.groups` argument.
# # A tibble: 6 × 3
# # Groups:   RM.bin [6]
#   RM.bin   `0`   `1`
#    <int> <dbl> <dbl>
# 1      3  25.3  NA  
# 2      4  15.4  NA  
# 3      5  17.2  22.2
# 4      6  21.8  25.9
# 5      7  36.0  44.1
# 6      8  45.7  36.0