#dataset is present in mlba::BostonHousing AKA ../../mlba/mlba/data-raw/BostonHousing.csv.gz
setwd("../../mlba/mlba/data-raw")
housing.df <- read.csv("BostonHousing.csv.gz")

head(housing.df, 9)

#dataset is present in mlba::Amtrak AKA ../../mlba/mlba/data-raw/Amtrak.csv.gz
amtrak.df <- read.csv("Amtrak.csv.gz")

ridership.ts <- ts(amtrak.df$Ridership, 
    start=c(1991, 1), 
    end=c(2004, 3), 
    freq=12)

MEDV.per.CHAS <- aggregate(housing.df$MEDV,
    by=list(housing.df$CHAS),
    FUN=mean)
names(MEDV.per.CHAS) <- c("CHAS", "MeanMEDV")
MEDV.per.CHAS$CHAS <- factor(MEDV.per.CHAS$CHAS)

CAT.MEDV.per.CHAS <- aggregate(housing.df$CAT..MEDV,
    by=list(housing.df$CHAS),
    FUN=mean)
names(CAT.MEDV.per.CHAS) <- c("CHAS", "MeanCATMEDV")
CAT.MEDV.per.CHAS$CHAS <- factor(CAT.MEDV.per.CHAS$CHAS)

#Line Chart
plot(ridership.ts, 
    xlab="Year", 
    ylab="Ridership (in 000s)",
    ylim=c(1300,2300))

#Scatter Plot of LSTAT vs MEDV
plot(housing.df$MEDV ~ housing.df$LSTAT,
    xlab="LSTAT",
    ylab="MEDV")

#Barchart 