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
idx <- caret::createDataPartition(housing.df$TOTAL.VALUE, 
    p=0.6, 
    list=FALSE)

train.df <- housing.df[idx, ]
holdout.df <- housing.df[-idx, ]

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

#    actual predicted  residuals
# 1   344.2  384.4206 -40.220638
# 4   498.6  546.4628 -47.862759
# 5   331.5  347.9170 -16.417031
# 12  344.5  380.4297 -35.929727
# 13  315.5  313.1879   2.312083
# 15  326.2  345.3751 -19.175064
#   actual precited  residuals
# 2  412.6 460.2777 -47.677744
# 3  330.1 359.3920 -29.291958
# 6  337.4 290.0277  47.372303
# 7  359.4 402.5332 -43.133242
# 8  320.4 314.0683   6.331652
# 9  333.5 339.8206  -6.320582
#   ME RMSE      MAE
# 1  0  NaN 31.98717
#         ME     RMSE MAE
# 1 -1.04237 43.90381 NaN
#    TAX LOT.SQFT YR.BUILT GROSS.AREA LIVING.AREA FLOORS ROOMS BEDROOMS FULL.BATH
# 1 3818     4200     1960       2670        1710    2.0    10        4         1
# 2 3791     6444     1940       2886        1474    1.5     6        3         1
# 3 4275     5035     1925       3264        1523    1.0     6        2         1
#   HALF.BATH KITCHEN FIREPLACE REMODEL_Old REMODEL_Recent
# 1         1       1         1           0              0
# 2         1       1         1           0              0
# 3         0       1         0           0              1
#        1        2        3 
# 385.9718 378.6392 352.1145 