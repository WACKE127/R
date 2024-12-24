library(tidyverse)

setwd("../../mlba/mlba/data-raw")
bostonHousing.df <- read.csv("BostonHousing.csv.gz")

bostonHousing.df <- bostonHousing.df %>%
#Creates 9 breakpoints and assigns numerics codes to each interval
mutate(RM.bin = cut(RM, c(1:9), 
       labels = FALSE))

aggregate(bostonHousing.df$MEDV,
    by = list(RM = bostonHousing.df$RM.bin,
    CHAS = bostonHousing.df$CHAS), 
    FUN = mean)

### OUTPUT

#    RM CHAS        x
# 1   3    0 25.30000
# 2   4    0 15.40714
# 3   5    0 17.20000
# 4   6    0 21.76917
# 5   7    0 35.96444
# 6   8    0 45.70000
# 7   5    1 22.21818
# 8   6    1 25.91875
# 9   7    1 44.06667
# 10  8    1 35.95000

# Alternative method for Tinyverse (SO MUCH EASIER)
bostonHousing.df %>%
    group_by(RM.bin, CHAS) %>%
    summarise(mean(MEDV))

### OUTPUT 

# `summarise()` has grouped output by 'RM.bin'. You can override using the `.groups` argument.
# # A tibble: 10 × 3
# # Groups:   RM.bin [6]
#    RM.bin  CHAS `mean(MEDV)`
#     <int> <int>        <dbl>
#  1      3     0         25.3
#  2      4     0         15.4
#  3      5     0         17.2
#  4      5     1         22.2
#  5      6     0         21.8
#  6      6     1         25.9
#  7      7     0         36.0
#  8      7     1         44.1
#  9      8     0         45.7
# 10      8     1         36.0

