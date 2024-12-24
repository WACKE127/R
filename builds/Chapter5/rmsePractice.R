library(tidyverse)

set.seed(1)

setwd("../../mlba/mlba/data-raw")
car.df <- read.csv("ToyotaCorolla.csv.gz") %>%
    select(-one_of("Id", "Model", "Fuel_Type", "Color")) %>%
    drop_na()

# head(car.df, 5)


idx <- caret::createDataPartition(car.df$Price, p=0.6, list = FALSE)
train.df <- car.df[idx,]
holdout.df <- car.df[-idx,]

#linear reg
reg <- lm(Price~., data = train.df)
pred_t <- predict(reg)
pred_h <- predict(reg, newdata = holdout.df)

caret::RMSE(pred_t, train.df$Price)
caret::RMSE(pred_h, holdout.df$Price)

### OUTPUT

# [1] 1045.999
# [1] 1395.578