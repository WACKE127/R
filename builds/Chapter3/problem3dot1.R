#Using the dataset mlba::ApplianceShipments
#Create a well-formatted time plot 
#Determine whether there is a quarterly pattern
#  - Analyze the region of 3500-500 on the y-axis
#Create one chart with four separate lines
#  - One per each quarter labeled Q1-Q4
#Create line graph at yearly aggregated level

library(ggplot2)
library(shiny)
suppressPackageStartupMessages(library(tidyverse))
library(ggfortify)
library(gridExtra)

setwd("../../mlba/mlba/data-raw")
dataFrame <- read.csv("ApplianceShipments.csv.gz")

#Visualize the data before beginning any work
# head(dataFrame, 5)

#Reform dataset
dataFrame <- dataFrame %>%
    mutate(Year = substring(dataFrame$Quarter, 4)) %>%
    mutate(Quarter = substring(dataFrame$Quarter, 1, 2))

dataFrame.ts <- ts(dataFrame$Shipments,
    start = min(dataFrame$Year),
    end = max(dataFrame$Year)
    )

g1 <- autoplot(dataFrame.ts) +
    xlab("Year") +
    ylab("Shipments")

g2 <- ggplot(data = dataFrame, 
    mapping = aes(x = Year, 
                  y = Shipments,
                  color = Quarter)) + geom_point()

g3 <- ggplot(data = dataFrame,
    mapping = aes(x = Quarter,
                  y = Shipments)) + geom_point()

plots.1 <- function() {
    grid.arrange(g1, g2, g3)
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

#Good enough