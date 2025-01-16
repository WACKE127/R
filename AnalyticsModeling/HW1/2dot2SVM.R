library(kernlab)

data("credit_card_data")

# call ksvm.  Vanilladot is a simple linear kernel.
model <- ksvm(as.matrix(credit_card_data[,1:10]),
              as.matrix(credit_card_data[,11]),
              type = "C-svc",
              kernel = "vanilladot",
              C = 100,
              scaled=TRUE)

# calculate a1…am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a

# calculate a0
a0 <- -model@b
a0

# see what the model predicts
pred <- predict(model, credit_card_data[,1:10])
pred

# see what fraction of the model’s predictions match the actual classification
sum(pred == credit_card_data[,11]) / nrow(credit_card_data)
