library(rpart)
library(rpart.plot)
library(shiny)

setwd("../../mlba/mlba/data-raw")
mowers.df <- read.csv("RidingMowers.csv.gz")

head(mowers.df, n = 20)

class.tree <- rpart(Ownership ~., 
              data = mowers.df,
              control = rpart.control(maxdepth = 2),
              method = "class")

g1 <- function(){
    rpart.plot(class.tree, 
               extra = 1, 
               fallen.leaves = FALSE)
}
ui <- fluidPage(
    plotOutput("g1"),
)

server <- function(input, output) {
    output$g1 <- renderPlot({
        g1()
    })
}

shinyApp(ui = ui, server = server)
