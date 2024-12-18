suppressPackageStartupMessages(library(tidyverse))
library(fastDummies)
library(caret)

#data located in ../../mlba/mlba/dataRaw/'WestRoxbury.csv.gz'
setwd("../../mlba/mlba/data-raw")
housing.df <- read.csv("WestRoxbury.csv.gz") %>%
    drop_na()  %>% 
    select(-TAX)  %>% 
    mutate(REMODEL=factor(REMODEL))  %>% 
    dummy_cols(select_columns=c('REMODEL'),
        remove_selected_columns=TRUE,
        remove_first_dummy=TRUE)

set.seed(1)

#60% training, 40% holdout
train.rows <- sample(row.names(housing.df), nrow(housing.df)*0.6)
train.df <- housing.df[train.rows, ]

holdout.rows <- setdiff(rownames(housing.df), train.rows)
holdout.df <- housing.df[holdout.rows, ]

reg <- lm(TOTAL.VALUE ~ ., data=train.df)
train.res <- data.frame(actual=train.df$TOTAL.VALUE,
predicted=reg$fitted.values,
        residuals=reg$residuals)
head(train.res)

pred <- predict(reg, newdata=holdout.df)
holdout.res <- data.frame(actual=holdout.df$TOTAL.VALUE,
precited = pred,
        residuals=holdout.df$TOTAL.VALUE - pred)
head(holdout.res)

data.frame(
    ME = round(mean(train.res$residuals), 5),
    RMSE = RMSE(pred=train.res$precited, obs=train.res$actual),
    MAE = MAE(pred=train.res$predicted, obs=train.res$actual)
)

data.frame(
    ME = round(mean(holdout.res$residuals), 5),
    RMSE = RMSE(pred=holdout.res$precited, obs=holdout.res$actual),
    MAE = MAE(pred=holdout.res$predicted, obs=holdout.res$actual)
)

housing.df <- read.csv("WestRoxbury.csv.gz") 
new.data <- housing.df[100:102, -1]  %>% 
    mutate(REMODEL=factor(REMODEL, levels=c("None", "Old", "Recent")))  %>% 
    dummy_cols(select_columns=c('REMODEL'),
        remove_selected_columns=TRUE,
        remove_first_dummy=TRUE)

new.data
pred <- predict(reg, newdata=new.data)
pred


#OUTPUT

# Attaching package: ‘caret’

# The following object is masked from ‘package:purrr’:

#     lift

#      actual predicted  residuals
# 1017  271.8  299.9656 -28.165553
# 4775  415.4  385.9018  29.498172
# 2177  427.1  378.2380  48.862036
# 5026  468.8  474.5002  -5.700226
# 1533  383.0  448.9634 -65.963401
# 4567  394.6  357.3559  37.244086
#    actual precited  residuals
# 2   412.6 463.8998 -51.299750
# 5   331.5 348.2758 -16.775751
# 6   337.4 288.2426  49.157437
# 8   320.4 316.8021   3.597873
# 9   333.5 338.8323  -5.332347
# 10  409.4 502.8091 -93.409058
#   ME RMSE      MAE
# 1  0  NaN 32.26614
#         ME     RMSE MAE
# 1 -1.13008 43.50657 NaN
#    TAX LOT.SQFT YR.BUILT GROSS.AREA LIVING.AREA FLOORS ROOMS BEDROOMS FULL.BATH
# 1 3818     4200     1960       2670        1710    2.0    10        4         1
# 2 3791     6444     1940       2886        1474    1.5     6        3         1
# 3 4275     5035     1925       3264        1523    1.0     6        2         1
#   HALF.BATH KITCHEN FIREPLACE REMODEL_Old REMODEL_Recent
# 1         1       1         1           0              0
# 2         1       1         1           0              0
# 3         0       1         0           0              1
#        1        2        3 
# 391.9485 377.9321 348.8626 