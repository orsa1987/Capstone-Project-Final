### Coursera's Data Science Capstone : Final Project
### This code generates the required ngram data files used to predict ngrams.
### These files are used by prediction functions found in server.R.


# Load the required packages
    library(tidytext)
    library(tidyverse)
    library(stringr)
    library(knitr)
    library(wordcloud)
    library(ngram)

# Download and unzip the Data
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip")
unzip("Coursera-SwiftKey.zip")

# Load the Data

blogs <- readLines("./final/en_US/en_US.blogs.txt", skipNul = TRUE)
con <- file("./final/en_US/en_US.news.txt", open="rb")
news <- readLines(con,  skipNul = TRUE)
twitter <- readLines("./final/en_US/en_US.twitter.txt", skipNul = TRUE)
close(con)
rm(con)

# Create Dataframes for the Data
blogs <- data_frame(text = blogs)
news <- data_frame(text = news)
twitter <- data_frame(text = twitter)

# Sampling the Data
set.seed(1987)

blogs <- blogs %>%
    sample_n(., nrow(blogs)*.03)
news <- news %>%
    sample_n(., nrow(news)*.03)
twitter <- twitter %>%
    sample_n(., nrow(twitter)*.03)

# Create Sample Data
sample <- bind_rows(
    mutate(blogs, source = "blogs"),
    mutate(news,  source = "news"),
    mutate(twitter, source = "twitter")
)
sampleData$source <- as.factor(sampleData$source)

# Clear the un-neccessary data variables
rm(list = c("twitter", "news", "blogs")
)

# Clean the sampleData

data("stop_words")

# upload swear_words
download.file("http://www.bannedwordlist.com/lists/swearWords.csv", "swearWords.csv")
swear_words <- read_delim("swearWords.csv", delim = "\n", col_names = FALSE)
swear_words <- unnest_tokens(swear_words, word, X1)

# Clean the sample
sample <-  sample %>%
    mutate(text = str_replace_all(text, "[^[:alpha:][:space:]]*", "")) %>%
    mutate(text = str_replace_all(text, "http[^[:space:]]*", "")) %>%
    mutate(text = str_replace_all(text, "\\b(?=\\w*(\\w)\\1)\\w+\\b", "")) %>%
    mutate(text = iconv(text, "ASCII//TRANSLIT"))

# Generate Ngrams

unigrams <- sample %>%
    unnest_tokens(word, text) %>%
    anti_join(swear_words) %>%
    anti_join(stop_words)

bigrams <- sample %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams <- bigrams %>%
    count(bigram) %>%
    filter(n > 10) %>%
    arrange(desc(n))
bigrams <- bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

trigrams <- sample %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3)
trigrams <- trigrams %>%
    count(trigram) %>%
    filter(n > 10) %>%
    arrange(desc(n))
trigrams <- trigrams %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ")

quadgrams <- sample %>%
    unnest_tokens(quadgram, text, token = "ngrams", n = 4)
quadgrams <- quadgrams %>%
    count(quadgram) %>%
    filter(n > 10) %>%
    arrange(desc(n))
quadgrams <- quadgrams %>%
    separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")

quintgrams <- sample %>%
    unnest_tokens(quintgram, text, token = "ngrams", n = 5)
quintgrams <- quintgrams %>%
    count(quintgram) %>%
    filter(n > 10) %>%
    arrange(desc(n))
quintgrams <- quintgrams %>%
    separate(quintgram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")

# Save the data
saveRDS(bigrams, "bigrams.rds")
saveRDS(trigrams, "trigrams.rds")
saveRDS(quadgrams,"quadgrams.rds")
saveRDS(quintgrams,"quintgrams.rds")
