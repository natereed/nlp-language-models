library(text2vec)
library(tm)
library(dplyr)

BiGramModel <- function(doc) {
  unigrams <- build_vocab(1, doc)
  bigrams <- build_vocab(2, doc)
  unigrams <- select(unigrams, 
                     unigram=terms, 
                     unigram_count=terms_counts)
  bigrams <- select(bigrams,
                    bigram=terms,
                    bigram_count=terms_counts)
  bigrams$word_n_minus_1 = tstrsplit(bigrams$bigram, '_')[[1]]
  bigrams <- merge(bigrams, unigrams, by.x="word_n_minus_1", by.y="unigram")
  bigrams$p <- bigrams$bigram_count / bigrams$unigram_count
    
  ngrams <- list(bigrams)
  class(ngrams) <- append(class(doc), "BiGramModel")
  return(ngrams)
}

## Use:
# Read data into doc
# model <- BiGramModel(doc)

# predict(model, 'this_is_a')
# predict(model, test_subset)
# score(model, test_subset)

get_regex <- function(input) {
  return(paste(paste("^", input, sep=""), "_", sep=""))
}

predict_next_word <- function(model, x) {
  UseMethod("predict_next_word", model)
}

predict_next_word.BiGramModel <- function(model, ngram) {
  pat <- get_regex(ngram)
  vocab <- model[[1]]
  matches <- vocab[vocab$word_n_minus_1 == ngram]
  if (nrow(matches) > 0) {
    matches[with(matches, order(-p))][1];
  } else {
    return(NULL);
  }
}

# Convert into vector operation:
predict_p <- function(model, X) {
  UseMethod("predict_p", model)
}

predict_p.BiGramModel <- function(model, X) {
  predictions <- lapply(X, function(x) {
    vocab <- model[[1]]
    matches <- vocab[vocab$bigram == x]
    if (nrow(matches) > 0) {
      return(matches[1]$p);
    } else {
      return(0);
    }
  });
  return(unlist(predictions))
}

score <- function(model, X) {
  UseMethod("score", model)
}

# Get the values of p for test_vocab and calculate the MSE 
score.BiGramModel <- function(model, test_vocab) {
  X <- test_vocab$terms
  predictions <- predict_p(model, X)
  return(test_vocab$p - predictions)
}

# Given a vocabulary of n-grams (bi-grams), return the MSE of the predicted p's vs. the actual p's

#score <- function(model, X) {
#  for()  
#}

# test
doc = c('This is a test', 'a courageous thing', 'An unforunate event happened', 'Say my name!', 'He experienced a test of character')