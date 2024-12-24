#Data present within ../../mlba/mlba/data-raw/BostonHousing.csv.gz
setwd("../../mlba/mlba/data-raw")
bostonHousing.df <- read.csv("BostonHousing.csv.gz")

#View first 5 rows
head(bostonHousing.df, 5)

### OUTPUT 

#      CRIM ZN INDUS CHAS   NOX    RM  AGE    DIS RAD TAX PTRATIO LSTAT MEDV
# 1 0.00632 18  2.31    0 0.538 6.575 65.2 4.0900   1 296    15.3  4.98 24.0
# 2 0.02731  0  7.07    0 0.469 6.421 78.9 4.9671   2 242    17.8  9.14 21.6
# 3 0.02729  0  7.07    0 0.469 7.185 61.1 4.9671   2 242    17.8  4.03 34.7
# 4 0.03237  0  2.18    0 0.458 6.998 45.8 6.0622   3 222    18.7  2.94 33.4
# 5 0.06905  0  2.18    0 0.458 7.147 54.2 6.0622   3 222    18.7  5.33 36.2
#   CAT..MEDV
# 1         0
# 2         0
# 3         1
# 4         1
# 5         1

summary(bostonHousing.df)

### OUTPUT

#       CRIM                ZN             INDUS            CHAS        
#  Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000  
#  1st Qu.: 0.08205   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000  
#  Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000  
#  Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917  
#  3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000  
#  Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000  
#       NOX               RM             AGE              DIS        
#  Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
#  1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
#  Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
#  Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
#  3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
#  Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
#       RAD              TAX           PTRATIO          LSTAT      
#  Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   : 1.73  
#  1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.: 6.95  
#  Median : 5.000   Median :330.0   Median :19.05   Median :11.36  
#  Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :12.65  
#  3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:16.95  
#  Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :37.97  
#       MEDV         CAT..MEDV    
#  Min.   : 5.00   Min.   :0.000  
#  1st Qu.:17.02   1st Qu.:0.000  
#  Median :21.20   Median :0.000  
#  Mean   :22.53   Mean   :0.166  
#  3rd Qu.:25.00   3rd Qu.:0.000  
#  Max.   :50.00   Max.   :1.000  

# Table 4.3 suggests that we calculate Mean, SD, Min, Max, Median and Length
# only SD and Length are left out of the summary function

sd(bostonHousing.df$CRIM)
length(bostonHousing.df$CRIM)

### OUTPUT

# [1] 8.601545
# [1] 506

data.frame(mean = sapply(bostonHousing.df, mean),
sd = sapply(bostonHousing.df, sd),
min = sapply(bostonHousing.df, min),
max = sapply(bostonHousing.df, max),
median = sapply(bostonHousing.df, median),
length = sapply(bostonHousing.df, length),
missVal = sapply(bostonHousing.df, function(x) sum(length(which(is.na(x)))))
)

### Output
 
#                   mean          sd       min      max    median length missVal
# CRIM        3.61352356   8.6015451   0.00632  88.9762   0.25651    506       0
# ZN         11.36363636  23.3224530   0.00000 100.0000   0.00000    506       0
# INDUS      11.13677866   6.8603529   0.46000  27.7400   9.69000    506       0
# CHAS        0.06916996   0.2539940   0.00000   1.0000   0.00000    506       0
# NOX         0.55469506   0.1158777   0.38500   0.8710   0.53800    506       0
# RM          6.28463439   0.7026171   3.56100   8.7800   6.20850    506       0
# AGE        68.57490119  28.1488614   2.90000 100.0000  77.50000    506       0
# DIS         3.79504269   2.1057101   1.12960  12.1265   3.20745    506       0
# RAD         9.54940711   8.7072594   1.00000  24.0000   5.00000    506       0
# TAX       408.23715415 168.5371161 187.00000 711.0000 330.00000    506       0
# PTRATIO    18.45553360   2.1649455  12.60000  22.0000  19.05000    506       0
# LSTAT      12.65306324   7.1410615   1.73000  37.9700  11.36000    506       0
# MEDV       22.53280632   9.1971041   5.00000  50.0000  21.20000    506       0
# CAT..MEDV   0.16600791   0.3724560   0.00000   1.0000   0.00000    506       0