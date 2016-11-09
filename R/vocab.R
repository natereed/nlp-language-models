library(text2vec)
library(tm)
library(data.table)

get_swear_words <- function() {
  if (!file.exists("swearwords.txt")) {
    download.file(url="https://s3.amazonaws.com/natereed-coursera/swearwords.txt", 
                  destfile="swearwords.txt",
                  method="curl")
  }
  swear_words <- readLines("swearwords.txt")
  return(swear_words)
}

clean_twitter <- function(t) {
  # Remove hashes
  t <- gsub('#', '', t)
  
  # Remove email addresses
  t <- gsub("[[:alnum:]]+\\@[[:alpha:]]+\\.com", '', t)
  
  # Remove url's
  t <- gsub('http\\S+\\s*', '', t)
  
  # Translate some common "textese" (a.k.a. "SMS language")
  # The list of such words is very long. TBD: Add comprehensive translation.
  t <- gsub('\\br\\b', 'are', t)
  t <- gsub('\\b@\\b', 'at', t)
  t <- gsub('\\bjst\\b', 'just', t)
  t <- gsub('\\bluv\\b', 'love', t)
  t <- gsub('\\bu\\b', 'you', t)
  return(t)
}

strip_profanity_from_entry <- function(v) {
  expr <- do.call(paste, c(as.list(get_swear_words()), sep="|"))
  return(v[grep(expr, v, invert=TRUE)])
}

strip_profanity <- function(vector_list) {
  return(lapply(vector_list, strip_profanity_from_entry))
}

# Chain functions for tokenizing and cleaning:
cleaning_tokenizer <- function(v) {
  v %>% 
    clean_twitter %>%
    removeNumbers %>%
    removePunctuation %>%
    word_tokenizer %>%
    strip_profanity
} 

calc_probabilities <- function(vocab) {
  vocab$p <- vocab$terms_counts / sum(vocab$terms_counts)
  return(vocab)
}

build_vocab <- function(n, text_vec) {
  text_vec <- iconv(text_vec, 'utf-8', 'ascii', sub='')
  it <- itoken(text_vec, 
               preprocess_function = tolower, 
               tokenizer = cleaning_tokenizer);
  
  # Create vocab from iterator
  print(paste("Initializing vocabulary for n=", n))
  vocab <- create_vocabulary(it, ngram=c(n, n));
  vocab <- vocab[[1]]
  return(vocab)
}
