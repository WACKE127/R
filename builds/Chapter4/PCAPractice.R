library(tidyverse)

setwd("../../mlba/mlba/data-raw")
cereals.df <- read.csv("Cereals.csv.gz") %>%
    select(calories, rating)

pcs <- prcomp(cereals.df %>%
              select(calories, rating))
summary(pcs)

pcs$rot
scores <- pcs$x
head(scores, 5)

### OUTPUT 

# Importance of components:
#                            PC1    PC2
# Standard deviation     22.3165 8.8844
# Proportion of Variance  0.8632 0.1368
# Cumulative Proportion   0.8632 1.0000
#                 PC1       PC2
# calories  0.8470535 0.5315077
# rating   -0.5315077 0.8470535
#             PC1        PC2
# [1,] -44.921528  2.1971833
# [2,]  15.725265 -0.3824165
# [3,] -40.149935 -5.4072123
# [4,] -75.310772 12.9991256
# [5,]   7.041508 -5.3576857