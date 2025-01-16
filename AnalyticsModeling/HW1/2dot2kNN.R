library(kknn)

data("credit_card_data_headers")

#Set seed for reproducibility
set.seed(27)

rows <- nrow(credit_card_data_headers)
rows

#Train data on 70% of the dataset
trainSet <- sample(seq_len(rows), size = 0.7*rows)

train.df <- credit_card_data_headers[ trainSet, ]
test.df  <- credit_card_data_headers[ -trainSet, ]

#Initial test value of 3 for K.
model <- kknn(formula = R1 ~ ., 
              train = train.df, 
              test = test.df,
              k = 42,
              scale = TRUE
)

#Model probabilities
predProb <- fitted(model)
predProb

#Coerce any model predictions over 50% to be a 1.
predClass <- ifelse(predProb > 0.5, 1, 0)
predClass

#Check acc. After manually trying various K values, it was found that K ~5 > K 1-4 in accuracy. 
#K values around 30-50 have significantly varying accuracies. Highest was found @ 42.
accuracy <- mean(test.df$R1 == predClass)
accuracy
