#ROC R, not R OCR
library(ROCR)
library(ggplot2)
library(shiny)

setwd("../../mlba/mlba/data-raw")
df <- read.csv("liftExample.csv.gz")

predprob <- prediction(df$prob, df$actual)
perf <- performance(predprob, "tpr", "fpr")
perf.df <- data.frame(
    tpr = perf@x.values[[1]],
    fpr = perf@y.values[[1]]
)

plot1 <- function()
{
    ggplot(perf.df, aes(x = tpr, y = fpr)) + 
    geom_line() +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "grey", linetype = "dashed") +
    labs(x = "1 - Specificity", y = "Sensitivity")
}

performance(predprob, measure = "auc")@y.values[[1]]

ui <- fluidPage(
    plotOutput("plot1"),
)

server <- function(input, output) {
    output$plot1 <- renderPlot({
        plot1()
    })
}

shinyApp(ui = ui, server = server)