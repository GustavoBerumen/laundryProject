### functions

# function to return a dataframe of word frequency
wordFreq <- function(pivotText) {
  # create a corpus AND clean text 
  docs <- Corpus(VectorSource(pivotText))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  # create a document-term-matrix
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  dfText <- data.frame(word = names(words),freq=words)
  
  wordElms <- list(dtm, dfText)
  return(wordElms)
}

# function to replace numeric answers for character answer in pivot of survey data frame
replaceAnswer <- function(pivot, column){
  for (i in 1:lenAns){
    pivot[[column]][pivot[[column]] == i] <-  charAns[i]
  }
  return(pivot)
}