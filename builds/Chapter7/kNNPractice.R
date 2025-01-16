library(ggrepel)
library(ggplot2)
library(shiny)
library(ggfortify)
library(gridExtra)

setwd('../../mlba/mlba/data-raw')
mowers.df <- read.csv('RidingMowers.csv.gz')

set.seed(35)

idx <- sample(nrow(mowers.df), 0.6 * nrow(mowers.df))

train.df <- mowers.df[idx, ]
holdout.df <- mowers.df[-idx, ]

new.df <- data.frame(Income = 60, Lot_Size = 20)

#DISPLAY NEW DATA POINT

g1 <- ggplot(mapping = aes(x = Income, y = Lot_Size, shape = Ownership, color = Ownership)) +
    geom_point(data = train.df) + 
    geom_text_repel(aes(label = rownames(train.df)), data = train.df, show.legend = FALSE) +
    geom_point(data = cbind(new.df, Ownership = 'New'))

g2 <- ggplot(mapping = aes(x = Income, y = Lot_Size, shape = Ownership, color = Ownership)) +
    geom_point(data = train.df, size = 4) +
    geom_text_repel(aes(label = rownames(train.df)), data = train.df, show.legend = FALSE) +
    geom_point(data = cbind(new.df, Ownership = 'New'), size = 5) +
    scale_shape_manual(values = c(18, 15, 21)) +
    scale_color_manual(values = c('black', 'darkorange', 'steelblue')) +
    scale_fill_manual(values = c('black', 'darkorange', 'lightblue'))

plots.1 <- function() {
    grid.arrange(g1, g2, ncol = 2, nrow = 1)
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

