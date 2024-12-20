#data located in ../../mlba/mlba/dataRaw/'WestRoxbury.csv.gz'
setwd("../../mlba/mlba/data-raw")
housing.df <- read.csv("WestRoxbury.csv.gz")

s <- sample(row.names(housing.df), 5)
housing.df[s,]

s <- sample(row.names(housing.df), 5, 
prob=ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,]

housing.df$REMODEL <- factor(housing.df$REMODEL)
table(housing.df$REMODEL)
upsampled.df <- caret::upSample(housing.df, housing.df$REMODEL,
list=TRUE)$x

table(upsampled.df$REMODEL)

# EXPECTED OUTPUT
#      TOTAL.VALUE  TAX LOT.SQFT YR.BUILT GROSS.AREA LIVING.AREA FLOORS ROOMS
# 4837       603.1 7587    10307     1958       4651        2904    1.0     9
# 254        334.4 4206     7622     1940       2896        1510    1.5     6
# 4312       459.4 5779     6556     1890       2745        1645    2.0     7
# 2196       478.9 6024     7647     1930       2855        1728    2.0     6
# 3908       246.1 3095     3652     1930       2068         936    1.0     5
#      BEDROOMS FULL.BATH HALF.BATH KITCHEN FIREPLACE REMODEL
# 4837        3         3         0       1         2    None
# 254         3         1         1       1         1    None
# 4312        4         1         1       1         1    None
# 2196        3         1         0       1         1    None
# 3908        2         1         0       1         1    None
#      TOTAL.VALUE   TAX LOT.SQFT YR.BUILT GROSS.AREA LIVING.AREA FLOORS ROOMS
# 3165       873.0 10982    19630     1910       6565        3374    2.0    12
# 4921       386.6  4863     5000     1920       2802        1512    2.0     7
# 579        326.7  4109    12568     1950       3230        1747    1.5    11
# 4053       552.4  6949     9397     1920       3952        2448    2.0     8
# 2889       560.2  7047    15690     1890       5052        2780    2.0    11
#      BEDROOMS FULL.BATH HALF.BATH KITCHEN FIREPLACE REMODEL
# 3165        4         3         1       1         4  Recent
# 4921        3         1         1       1         1    None
# 579         4         1         1       1         2    None
# 4053        4         1         1       1         1    None
# 2889        6         2         0       1         1    None

#   None    Old Recent 
#   4346    581    875 

#   None    Old Recent 
#   4346   4346   4346 