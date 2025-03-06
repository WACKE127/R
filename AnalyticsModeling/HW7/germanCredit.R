library(shiny)
library(rpart)
if(!require(rpart.plot)) install.packages("rpart.plot", dependencies=TRUE)
library(rpart.plot)
df <- read.table("data/germancredit.txt", header=FALSE, stringsAsFactors=FALSE)
df[] <- lapply(df, function(x) {
  nx <- suppressWarnings(as.numeric(x))
  if(all(!is.na(nx))) nx else as.factor(x)
})
df$Risk <- factor(ifelse(df$V21==1, "Good", "Bad"))
df <- df[,-21]
model <- glm(Risk ~ ., data=df, family=binomial(link="logit"))
tree_model <- rpart(Risk ~ ., data=df, method="class")
ui <- fluidPage(
  titlePanel("German Credit Modeling"),
  mainPanel(
    verbatimTextOutput("modelSummary"),
    verbatimTextOutput("thresholdOutput"),
    plotOutput("treePlot")
  )
)
server <- function(input, output, session) {
  output$modelSummary <- renderPrint({
    summary(model)
  })
  output$thresholdOutput <- renderPrint({
    th <- 5/6
    cat("Optimal threshold probability (for predicting Good):", th, "\n")
  })
  output$treePlot <- renderPlot({
    rpart.plot(tree_model)
  })
}
shinyApp(ui, server)
