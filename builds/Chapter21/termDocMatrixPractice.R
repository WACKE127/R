library(tm)

text <- c("This is the first sentence",
           "This is the second sentence", 
           "The third sentence is here")

corp <- Corpus(VectorSource(text))

tdm <- TermDocumentMatrix(corp)
inspect(tdm)