library(shiny)
library(ggplot2)
library(ggfortify)
library(gridExtra)

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

g1 <- autoplot(ridership.ts) +
    xlab("Year") +
    ylab("Ridership (in 000)")

#Scatter plot with Axes 
g2 <- ggplot(housing.df) +
    geom_point(aes(x = LSTAT, y = MEDV),
    color = "navy",
    alpha = 0.5)

g3 <- ggplot(MEDV.per.CHAS) +
    geom_bar(aes(x = CHAS, y = MeanMEDV, fill = CHAS), stat = "identity")

g4 <- ggplot(CAT.MEDV.per.CHAS) +
    geom_bar(aes(x = CHAS, y = MeanCATMEDV, fill = CHAS), stat = "identity") + 
    ylab("% of CAT.MEDV") 

plots.1 <- function() {
    grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)
}

ui <- fluidPage(
    plotOutput("plot1"),
)

server <- function(input, output) {
    output$plot1 <- renderPlot({
        plots.1()
    })
}

shinyApp(ui = ui, server = server)
    

