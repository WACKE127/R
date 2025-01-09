library(caret)
setwd("../../mlba/mlba/data-raw/")

car.df <- read.csv("ToyotaCorolla.csv.gz")

outcome <- "Price"
predictors <- c("Age_08_04", "KM", "Fuel_Type", "HP", "Met_Color", "Automatic", "CC", "Doors", "Quarterly_Tax", "Weight")

car.df <- car.df[1:1000, c(outcome, predictors)]

set.seed(1)
idx <- createDataPartition(car.df$Price, p=0.6, list=FALSE)

train.df <- car.df[idx,]
holdout.df <- car.df[-idx,]

car.lm <- lm(Price ~., data = train.df)

options(scipen = 999)
summary(car.lm)