# I am confused by this.

library(caret)

setwd("../../mlba/mlba/data-raw")
owner.df <- read.csv("ownerExample.csv.gz")

#Factor is data structure for categories
#Levels are unique categories
#confusionMatrix expects levels to match perfectly
#PRINT LEVELS TO CONSOLE
levels(owner.df$Class)

owner.df$Class <- factor(owner.df$Class, levels = c("nonowner", "owner"))

#PRINT LEVELS AGAIN
levels(owner.df$Class)

confusionMatrix(factor(ifelse(owner.df$Probability > 0.5,
                      "owner",
                      "nonowner")),
                      owner.df$Class)$table
                      
confusionMatrix(factor(ifelse(owner.df$Probability > 0.25,
                      "owner",
                      "nonowner")),
                      owner.df$Class)$table

confusionMatrix(factor(ifelse(owner.df$Probability > 0.75,
                      "owner",
                      "nonowner")),
                      owner.df$Class)$table