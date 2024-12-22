library(gplots)
library(shiny)

setwd("../../mlba/mlba/data-raw/")
dataFrame = read.csv("NYPD_Motor_Vehicle_Collisions_1000.csv.gz")

missing = dataFrame
missing[missing == ""] = NA
missing = 1 * is.na(missing)

plots.1 <- function(){
    heatmap(missing, 
    Rowv = NA,
    Colv = NA)
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
    
