library(tidyverse)
library(caret)
library(e1071)

setwd("../../mlba/mlba/data-raw")
delays.df <- read.csv("FlightDelays.csv.gz") %>%
#IMPORTANT, numerical variables must be categorical
             mutate(
                DAY_WEEK = factor(DAY_WEEK),
                ORIGIN = factor(ORIGIN),
                DEST = factor(DEST),
                CARRIER = factor(CARRIER),
                Flight.Status = factor(Flight.Status),
                CRS_DEP_TIME = factor(round(CRS_DEP_TIME/100))
             ) %>%
             select(DAY_WEEK, CRS_DEP_TIME, ORIGIN, DEST, CARRIER, Flight.Status)

set.seed(1)
idx <- createDataPartition(delays.df$Flight.Status, 
                           p=0.6, 
                           list = FALSE)
train.df <- delays.df[idx, ]
holdout.df <- delays.df[-idx, ]

delays.nb <- naiveBayes(Flight.Status ~., data = train.df)
delays.nb
